defmodule MazarynWeb.HomeLive.IpnsManager do
  @content_cache :post_content_cache

  @base_timeout 3_000
  @cache_ttl 1800
  @fresh_threshold 300

  @backoff_min 1_000
  @backoff_max 30_000
  @backoff_factor 1.5
  @max_retries 8

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
            ensure_ipns_background(post_id)
            ipns

          true ->
            ensure_ipns_background(post_id)
            ipns
        end

      [] ->
        ensure_ipns_background(post_id)
        nil
    end
  end

  def ensure_ipns(post_id) do
    ensure_ipns_background(post_id)
  end

  def ensure_ipns_background(post_id) do
    Task.start(fn ->
      try do
        fetch_ipns_with_retries(post_id)
      rescue
        e ->
          Logger.error("Background IPNS fetch failed for post #{post_id}: #{inspect(e)}")
      end
    end)
  end

  def clear_cache(post_id) do
    :ets.delete(@content_cache, {:ipns, post_id})
  end

  def warm_cache_async(recent_post_ids) when is_list(recent_post_ids) do
    Task.start(fn ->
      recent_post_ids
      |> Enum.take(5)
      |> Task.async_stream(&background_ipns_fetch/1, max_concurrency: 2, timeout: 5000)
      |> Stream.run()
    end)
  end

  defp fetch_ipns_with_retries(post_id, attempt \\ 1) do
    result = fetch_with_timeout(post_id, calculate_timeout(attempt))

    if is_valid_ipns(result) do
      cache_ipns(post_id, result)
      broadcast_ipns_ready(post_id, result)
      Logger.info("✅ IPNS resolved for post #{post_id} on attempt #{attempt}")
      :ok
    else
      if attempt < @max_retries do
        delay = backoff_delay(attempt)
        Logger.debug("⏳ IPNS attempt #{attempt} failed for post #{post_id}. Retrying in #{delay}ms...")
        Process.sleep(delay)
        fetch_ipns_with_retries(post_id, attempt + 1)
      else
        Logger.debug("❌ IPNS failed for post #{post_id} after #{@max_retries} attempts")
        :failed
      end
    end
  end

  defp calculate_timeout(attempt) when attempt <= 2, do: @base_timeout
  defp calculate_timeout(attempt) when attempt <= 4, do: @base_timeout + 1000
  defp calculate_timeout(_attempt), do: @base_timeout + 2000

  defp backoff_delay(1), do: @backoff_min
  defp backoff_delay(attempt) do
    base = :math.pow(@backoff_factor, attempt - 1) * @backoff_min
    jitter = :rand.uniform(200)
    round(min(base + jitter, @backoff_max))
  end

  defp background_ipns_fetch(post_id) do
    result = fetch_with_timeout(post_id, 3000)

    if is_valid_ipns(result) do
      cache_ipns(post_id, result)
      broadcast_ipns_ready(post_id, result)
      Logger.debug("Background IPNS fetch succeeded for #{post_id}")
    else
      Logger.debug("Background IPNS fetch failed for #{post_id}")
    end
  end

  defp fetch_with_timeout(post_id, timeout_ms) do
    with {:ok, valid_id} <- validate_post_id(post_id) do
      task = Task.async(fn ->
        try do
          id_charlist = if is_binary(valid_id), do: to_charlist(valid_id), else: valid_id

          internal_task = Task.async(fn ->
            result = Core.PostClient.get_ipns_from_post(id_charlist)
            normalize_ipns_result(result)
          end)

          case Task.yield(internal_task, div(timeout_ms, 2)) || Task.shutdown(internal_task, :brutal_kill) do
            {:ok, result} -> result
            nil -> nil
          end
        rescue
          e ->
            Logger.debug("Error in IPNS fetch task: #{inspect(e)}")
            nil
        catch
          kind, reason ->
            Logger.debug("Caught #{kind} in IPNS fetch task: #{inspect(reason)}")
            nil
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

  defp normalize_ipns_result(result) do
    case result do
      :undefined ->
        nil
      :error ->
        nil
      {:error, _} ->
        nil
      nil ->
        nil
      "" ->
        nil
      result when is_binary(result) and byte_size(result) > 0 ->
        result
      result when is_list(result) ->
        try do
          if Enum.all?(result, &is_integer/1) and Enum.all?(result, &(&1 >= 0 and &1 <= 1_114_111)) do
            case List.to_string(result) do
              "" -> nil
              str -> str
            end
          else
            nil
          end
        rescue
          ArgumentError ->
            Logger.debug("Invalid character list: #{inspect(result)}")
            nil
          _ ->
            Logger.debug("Error converting list to string: #{inspect(result)}")
            nil
        end
      result when is_atom(result) ->
        nil
      _ ->
        Logger.debug("Unhandled IPNS result type: #{inspect(result)}")
        nil
    end
  end

  defp is_valid_ipns(ipns) do
    case ipns do
      nil -> false
      "" -> false
      :undefined -> false
      :error -> false
      {:error, _} -> false
      result when is_binary(result) and byte_size(result) > 0 -> true
      _ -> false
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
    Logger.debug("Cached IPNS for post #{post_id}")
  end

  defp broadcast_ipns_ready(post_id, ipns) do
    Task.start(fn ->
      try do
        Phoenix.PubSub.broadcast(Mazaryn.PubSub, "home_feed_updates", {:ipns_ready, post_id, ipns})
        Phoenix.PubSub.broadcast(Mazaryn.PubSub, "post_updates", {:ipns_ready, post_id, ipns})
      rescue
        e ->
          Logger.error("Failed to broadcast IPNS ready: #{inspect(e)}")
      end
    end)
  end

  def resolve_ipns_batch(post_ids) when is_list(post_ids) do
    post_ids
    |> Enum.take(3)
    |> Task.async_stream(&background_ipns_fetch/1,
      max_concurrency: 2,
      timeout: 4000,
      on_timeout: :kill_task)
    |> Stream.run()
  end

  def ipns_processing?(post_id) do
    Process.whereis(:"ipns_task_#{post_id}") != nil
  end

  def get_ipns_with_fallback(post_id, fallback \\ nil) do
    case get_ipns_fast(post_id) do
      nil -> fallback
      ipns -> ipns
    end
  end

  def preload_critical_ipns(post_ids) when is_list(post_ids) do
    Task.start(fn ->
      post_ids
      |> Enum.take(2)
      |> Enum.each(&ensure_ipns_background/1)
    end)
  end
end
