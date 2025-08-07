defmodule MazarynWeb.HomeLive.IpnsManager do
  @moduledoc """
  Manages IPNS fetching, caching, and background refresh operations.
  """

  @content_cache :post_content_cache
  @max_retries 10
  @base_timeout 5000
  @cache_ttl 600 # 10 minutes
  @fresh_threshold 60 # 1 minute

  def get_ipns_fast(post_id) do
    cache_key = {:ipns, post_id}
    current_time = :erlang.system_time(:second)

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, ipns, timestamp}] ->
        age = current_time - timestamp
        handle_cached_ipns(post_id, ipns, age)
      [] ->
        spawn_background_refresh(post_id)
        nil
    end
  end

  def schedule_ipns_fetch(post_id, delay_ms \\ 30_000) do
    IO.puts("⏰ Scheduling IPNS fetch for post #{post_id} in #{delay_ms}ms")

    Task.start(fn ->
      Process.sleep(delay_ms)
      IO.puts("🚀 Starting scheduled IPNS fetch for post #{post_id}")

      clear_cache_and_failures(post_id)

      case fetch_with_exponential_backoff(post_id, 1) do
        ipns when not is_nil(ipns) ->
          cache_ipns(post_id, ipns)
          broadcast_ipns_ready(post_id, ipns)
          IO.puts("✅ Scheduled IPNS fetch completed for post #{post_id}")
        nil ->
          IO.puts("❌ All IPNS fetch attempts failed for post #{post_id}")
          record_persistent_failure(post_id)
      end
    end)
  end

  def warm_cache_async(recent_post_ids) when is_list(recent_post_ids) do
    Task.start(fn ->
      IO.puts("🔥 Starting async IPNS cache warming for #{length(recent_post_ids)} posts")

      recent_post_ids
      |> Enum.take(10)
      |> Enum.chunk_every(3)
      |> Enum.each(fn batch ->
        tasks = Enum.map(batch, &Task.async(fn -> spawn_background_refresh(&1) end))
        Task.yield_many(tasks, 500)
        Process.sleep(200)
      end)

      IO.puts("✅ IPNS cache warming completed")
    end)
  end

  def clear_cache(post_id) do
    cache_key = {:ipns, post_id}
    failure_key = {:ipns_failures, post_id}

    :ets.delete(@content_cache, cache_key)
    :ets.delete(@content_cache, failure_key)

    IO.puts("🧹 Cleared IPNS cache for post #{post_id}")
  end

  defp handle_cached_ipns(post_id, ipns, age) do
    cond do
      age < @fresh_threshold ->
        IO.puts("🔄 Post #{post_id} is fresh (#{age}s old), forcing refresh")
        spawn_background_refresh(post_id)
        ipns
      age < @cache_ttl ->
        IO.puts("📦 IPNS Cache HIT for post #{post_id}")
        ipns
      true ->
        spawn_background_refresh(post_id)
        ipns
    end
  end

  defp spawn_background_refresh(post_id) do
    parent_pid = self()

    Task.start(fn ->
      case fetch_with_timeout(post_id, 3000, true) do
        ipns when not is_nil(ipns) ->
          cache_ipns(post_id, ipns)
          send(parent_pid, {:ipns_updated, post_id, ipns})
          broadcast_ipns_ready(post_id, ipns)
          IO.puts("✅ Background IPNS refresh completed for #{post_id}")
        nil ->
          IO.puts("⚠️ Background IPNS fetch failed for #{post_id}")
      end
    end)
  end

  defp fetch_with_exponential_backoff(post_id, attempt) when attempt <= @max_retries do
    IO.puts("🔄 IPNS fetch attempt #{attempt}/#{@max_retries} for post #{post_id}")

    case fetch_with_timeout(post_id, 8000, true) do
      ipns when not is_nil(ipns) ->
        IO.puts("✅ IPNS fetch successful on attempt #{attempt}")
        :ets.delete(@content_cache, {:ipns_persistent_failures, post_id})
        ipns
      nil ->
        backoff_delay = calculate_backoff_delay(attempt)
        IO.puts("⚠️ IPNS fetch failed on attempt #{attempt}, retrying in #{backoff_delay}ms...")
        Process.sleep(backoff_delay)
        fetch_with_exponential_backoff(post_id, attempt + 1)
    end
  end

  defp fetch_with_exponential_backoff(post_id, attempt) when attempt > @max_retries do
    IO.puts("❌ All #{@max_retries} IPNS fetch attempts exhausted for post #{post_id}")
    schedule_long_term_retry(post_id)
    nil
  end

  defp calculate_backoff_delay(attempt) do
    base_delay = :math.pow(2, attempt - 1) * 1000
    max_delay = 30_000
    jitter = :rand.uniform(1000)

    min(base_delay + jitter, max_delay) |> round()
  end

  defp schedule_long_term_retry(post_id) do
    Task.start(fn ->
      Process.sleep(300_000)
      IO.puts("🔄 Starting long-term retry for post #{post_id}")

      case fetch_with_exponential_backoff(post_id, 1) do
        ipns when not is_nil(ipns) ->
          cache_ipns(post_id, ipns)
          broadcast_ipns_ready(post_id, ipns)
          IO.puts("✅ Long-term retry successful for post #{post_id}")
        nil ->
          IO.puts("❌ Long-term retry failed for post #{post_id}")
          schedule_very_long_term_retry(post_id)
      end
    end)
  end

  defp schedule_very_long_term_retry(post_id) do
    Task.start(fn ->
      case :ets.lookup(@content_cache, {:ipns_persistent_failures, post_id}) do
        [{_, failure_count, _}] when failure_count >= 24 ->
          IO.puts("🛑 Giving up on post #{post_id} after 24 hours of failures")
        _ ->
          Process.sleep(3_600_000) # 1 hour
          IO.puts("🔄 Starting hourly retry for post #{post_id}")

          case fetch_with_exponential_backoff(post_id, 1) do
            ipns when not is_nil(ipns) ->
              cache_ipns(post_id, ipns)
              broadcast_ipns_ready(post_id, ipns)
              IO.puts("✅ Hourly retry successful for post #{post_id}")
            nil ->
              record_persistent_failure(post_id)
              schedule_very_long_term_retry(post_id)
          end
      end
    end)
  end

  defp fetch_with_timeout(post_id, timeout, ignore_circuit_breaker \\ false) do
    case validate_post_id(post_id) do
      {:ok, valid_post_id} ->
        if should_fetch?(post_id, ignore_circuit_breaker) do
          execute_fetch(valid_post_id, timeout)
        else
          nil
        end
      {:error, _} ->
        nil
    end
  end

  defp validate_post_id(post_id) do
    case post_id do
      nil -> {:error, :nil_id}
      "" -> {:error, :empty_id}
      [] -> {:error, :empty_list}
      _ -> {:ok, post_id}
    end
  end

  defp should_fetch?(post_id, ignore_circuit_breaker) do
    if ignore_circuit_breaker do
      true
    else
      check_circuit_breaker(post_id)
    end
  end

  defp check_circuit_breaker(post_id) do
    persistent_failure_key = {:ipns_persistent_failures, post_id}
    failure_key = {:ipns_failures, post_id}

    case :ets.lookup(@content_cache, persistent_failure_key) do
      [{_, persistent_count, _}] when persistent_count >= 24 ->
        IO.puts("🛑 Permanent circuit breaker: post #{post_id} has failed too many times")
        false
      _ ->
        case :ets.lookup(@content_cache, failure_key) do
          [{_, failure_count, last_failure}] ->
            age = :erlang.system_time(:second) - last_failure
            if failure_count > 3 and age < 60 do
              IO.puts("🚫 Circuit breaker: skipping IPNS for #{post_id}")
              false
            else
              true
            end
          [] ->
            true
        end
    end
  end

  defp execute_fetch(post_id, timeout) do
    task = Task.async(fn ->
      try do
        post_id_charlist = if is_binary(post_id), do: to_charlist(post_id), else: post_id
        Core.PostClient.get_ipns_from_post(post_id_charlist)
      rescue
        _ -> nil
      catch
        _, _ -> nil
      end
    end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} ->
        :ets.delete(@content_cache, {:ipns_failures, post_id})
        result
      nil ->
        record_ipns_failure(post_id)
        nil
    end
  end

  defp record_ipns_failure(post_id) do
    failure_key = {:ipns_failures, post_id}
    timestamp = :erlang.system_time(:second)

    case :ets.lookup(@content_cache, failure_key) do
      [{_, count, _}] ->
        :ets.insert(@content_cache, {failure_key, count + 1, timestamp})
      [] ->
        :ets.insert(@content_cache, {failure_key, 1, timestamp})
    end
  end

  defp record_persistent_failure(post_id) do
    failure_key = {:ipns_persistent_failures, post_id}
    timestamp = :erlang.system_time(:second)

    case :ets.lookup(@content_cache, failure_key) do
      [{_, count, _}] ->
        :ets.insert(@content_cache, {failure_key, count + 1, timestamp})
      [] ->
        :ets.insert(@content_cache, {failure_key, 1, timestamp})
    end
  end

  defp cache_ipns(post_id, ipns) do
    cache_key = {:ipns, post_id}
    timestamp = :erlang.system_time(:second)
    :ets.insert(@content_cache, {cache_key, ipns, timestamp})
  end

  defp broadcast_ipns_ready(post_id, ipns) do
    Phoenix.PubSub.broadcast(
      Mazaryn.PubSub,
      "home_feed_updates",
      {:ipns_ready, post_id, ipns}
    )
  end

  defp clear_cache_and_failures(post_id) do
    cache_key = {:ipns, post_id}
    failure_key = {:ipns_failures, post_id}

    :ets.delete(@content_cache, cache_key)
    :ets.delete(@content_cache, failure_key)

    IO.puts("🧹 Cleared IPNS cache and failures for post #{post_id}")
  end
end
