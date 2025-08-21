defmodule MazarynWeb.HomeLive.IpnsManager do
  @moduledoc """
  Manages IPNS fetching, caching, and background refresh operations.
  """

  @content_cache :post_content_cache

  @base_timeout 5_000
  @cache_ttl 600
  @fresh_threshold 60

  @backoff_min 1_000
  @backoff_max 300_000
  @backoff_factor 2.0

  def get_ipns_fast(post_id) do
    cache_key = {:ipns, post_id}
    now = :erlang.system_time(:second)

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, ipns, ts}] ->
        age = now - ts

        cond do
          age < @fresh_threshold ->
            IO.puts("📦 IPNS Cache HIT (fresh) for post #{post_id} (#{age}s old) — refreshing softly")
            ensure_ipns(post_id)
            ipns

          age < @cache_ttl ->
            IO.puts("📦 IPNS Cache HIT for post #{post_id} (#{age}s old)")
            ensure_ipns(post_id)
            ipns

          true ->
            IO.puts("♻️ IPNS cache stale for post #{post_id} (#{age}s old) — starting insistent refresh")
            ensure_ipns(post_id)
            ipns
        end

      [] ->
        IO.puts("🔎 IPNS cache miss for post #{post_id} — starting insistent fetch")
        ensure_ipns(post_id)
        nil
    end
  end

  def ensure_ipns(post_id) do
    Task.start(fn -> insist_until_ipns(post_id) end)
  end

  def clear_cache(post_id) do
    :ets.delete(@content_cache, {:ipns, post_id})
    IO.puts("🧹 Cleared IPNS cache for post #{post_id}")
  end

  def warm_cache_async(recent_post_ids) when is_list(recent_post_ids) do
    Task.start(fn ->
      IO.puts("🔥 Starting async IPNS cache warming for #{length(recent_post_ids)} posts")

      recent_post_ids
      |> Enum.take(10)
      |> Enum.chunk_every(3)
      |> Enum.each(fn batch ->
        tasks = Enum.map(batch, &Task.async(fn -> soft_refresh(&1) end))
        Task.yield_many(tasks, 500)
        Process.sleep(200)
      end)

      IO.puts("✅ IPNS cache warming completed")
    end)
  end

  defp insist_until_ipns(post_id, attempt \\ 1) do
    case fetch_with_timeout(post_id, @base_timeout) do
      ipns when not is_nil(ipns) ->
        cache_ipns(post_id, ipns)
        broadcast_ipns_ready(post_id, ipns)
        IO.puts("✅ IPNS resolved for post #{post_id} on attempt #{attempt}")
        :ok

      nil ->
        delay = backoff_delay(attempt)
        IO.puts("⏳ IPNS not ready for post #{post_id} (attempt #{attempt}). Retrying in #{delay}ms...")
        Process.sleep(delay)
        insist_until_ipns(post_id, attempt + 1)
    end
  end

  defp backoff_delay(1), do: @backoff_min
  defp backoff_delay(attempt) do
    base = :math.pow(@backoff_factor, attempt - 1) * @backoff_min
    jitter = :rand.uniform(1_000)
    round(min(base + jitter, @backoff_max))
  end

  defp soft_refresh(post_id) do
    case fetch_with_timeout(post_id, 3_000) do
      ipns when not is_nil(ipns) ->
        cache_ipns(post_id, ipns)
        broadcast_ipns_ready(post_id, ipns)
        IO.puts("✅ Background IPNS refresh completed for #{post_id}")
      nil ->
        IO.puts("⚠️ Background IPNS refresh failed for #{post_id}")
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
