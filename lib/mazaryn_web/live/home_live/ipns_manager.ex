defmodule MazarynWeb.HomeLive.IpnsManager do

  @content_cache :post_content_cache

  @base_timeout 8_000
  @cache_ttl 900
  @fresh_threshold 120

  @backoff_min 2_000
  @backoff_max 60_000
  @backoff_factor 1.8
  @max_retries 15

  require Logger

  def get_ipns_fast(post_id) do
    cache_key = {:ipns, post_id}
    now = :erlang.system_time(:second)

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, ipns, ts}] ->
        age = now - ts

        cond do
          age < @fresh_threshold ->
            ipns

          age < @cache_ttl ->
            ensure_ipns(post_id)
            ipns

          true ->
            ensure_ipns(post_id)
            ipns
        end

      [] ->
        ensure_ipns(post_id)
        nil
    end
  end

  def ensure_ipns(post_id) do
    Task.start(fn -> insist_until_ipns_aggressive(post_id) end)
  end

  def clear_cache(post_id) do
    :ets.delete(@content_cache, {:ipns, post_id})
  end

  def warm_cache_async(recent_post_ids) when is_list(recent_post_ids) do
    Task.start(fn ->
      recent_post_ids
      |> Enum.take(8)
      |> Enum.chunk_every(2)
      |> Enum.each(fn batch ->
        tasks = Enum.map(batch, &Task.async(fn -> soft_refresh(&1) end))
        Task.yield_many(tasks, 1000)
        Process.sleep(500)
      end)
    end)
  end

  defp insist_until_ipns_aggressive(post_id, attempt \\ 1) do
    case fetch_with_timeout(post_id, calculate_timeout(attempt)) do
      ipns when not is_nil(ipns) ->
        cache_ipns(post_id, ipns)
        broadcast_ipns_ready(post_id, ipns)
        Logger.info("✅ IPNS resolved for post #{post_id} on attempt #{attempt}")
        :ok

      nil when attempt < @max_retries ->
        delay = backoff_delay(attempt)
        Logger.info("⏳ IPNS attempt #{attempt} failed for post #{post_id}. Retrying in #{delay}ms...")
        Process.sleep(delay)
        insist_until_ipns_aggressive(post_id, attempt + 1)

      nil ->
        Logger.warning("❌ IPNS failed for post #{post_id} after #{@max_retries} attempts")
        :failed
    end
  end

  defp calculate_timeout(attempt) when attempt <= 3, do: @base_timeout
  defp calculate_timeout(attempt) when attempt <= 7, do: @base_timeout + 2000
  defp calculate_timeout(attempt) when attempt <= 12, do: @base_timeout + 5000
  defp calculate_timeout(_attempt), do: @base_timeout + 10000

  defp backoff_delay(1), do: @backoff_min
  defp backoff_delay(attempt) do
    base = :math.pow(@backoff_factor, attempt - 1) * @backoff_min
    jitter = :rand.uniform(500)
    round(min(base + jitter, @backoff_max))
  end

  defp soft_refresh(post_id) do
    case fetch_with_timeout(post_id, 4000) do
      ipns when not is_nil(ipns) ->
        cache_ipns(post_id, ipns)
        broadcast_ipns_ready(post_id, ipns)
      nil ->
        :noop
    end
  end

  defp fetch_with_timeout(post_id, timeout_ms) do
    with {:ok, valid_id} <- validate_post_id(post_id) do
      task = Task.async(fn ->
        try do
          id_charlist = if is_binary(valid_id), do: to_charlist(valid_id), else: valid_id
          Core.PostClient.get_ipns_from_post(id_charlist)
        rescue
          _ -> nil
        catch
          _, _ -> nil
        end
      end)

      case Task.yield(task, timeout_ms) || Task.shutdown(task, :brutal_kill) do
        {:ok, result} -> result
        nil -> nil
      end
    else
      {:error, _} -> nil
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

  defp cache_ipns(post_id, ipns) do
    timestamp = :erlang.system_time(:second)
    :ets.insert(@content_cache, {{:ipns, post_id}, ipns, timestamp})
  end

  defp broadcast_ipns_ready(post_id, ipns) do
    Phoenix.PubSub.broadcast(Mazaryn.PubSub, "home_feed_updates", {:ipns_ready, post_id, ipns})
    Phoenix.PubSub.broadcast(Mazaryn.PubSub, "post_updates", {:ipns_ready, post_id, ipns})
  end
end
