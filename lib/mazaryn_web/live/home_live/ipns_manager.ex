defmodule MazarynWeb.HomeLive.IpnsManager do
  @content_cache :post_content_cache

  @base_timeout 2_000
  @cache_ttl 1800
  @fresh_threshold 300

  @backoff_min 2_000
  @backoff_max 45_000
  @backoff_factor 2.0
  @max_retries 5

  require Logger

  def get_ipns_fast(post_id) do
    cache_key = {:ipns, post_id}
    now = :erlang.system_time(:second)

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, ipns, ts}] ->
        age = now - ts

        if age >= @cache_ttl do
          spawn(fn -> fetch_ipns_silent(post_id) end)
        end

        ipns

      [] ->
        spawn(fn -> fetch_ipns_silent(post_id) end)
        nil
    end
  end

  def ensure_ipns(post_id) do
    spawn(fn -> fetch_ipns_silent(post_id) end)
    :ok
  end

  def ensure_ipns_background(post_id) do
    spawn(fn -> fetch_ipns_silent(post_id) end)
    :ok
  end

  def clear_cache(post_id) do
    :ets.delete(@content_cache, {:ipns, post_id})
  end

  def warm_cache_async(recent_post_ids) when is_list(recent_post_ids) do
    spawn(fn ->
      recent_post_ids
      |> Enum.take(3)
      |> Enum.each(fn post_id ->
        spawn(fn -> fetch_ipns_silent(post_id) end)
        Process.sleep(500)
      end)
    end)
  end

  defp fetch_ipns_silent(post_id) do
    try do
      fetch_ipns_with_retries(post_id)
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  defp fetch_ipns_with_retries(post_id, attempt \\ 1) do
    result = fetch_with_timeout(post_id, calculate_timeout(attempt))

    if is_valid_ipns(result) do
      cache_ipns(post_id, result)
      broadcast_ipns_ready(post_id, result)
      Logger.debug("IPNS resolved for post #{post_id} on attempt #{attempt}")
      :ok
    else
      if attempt < @max_retries do
        delay = backoff_delay(attempt)
        Process.sleep(delay)
        fetch_ipns_with_retries(post_id, attempt + 1)
      else
        :failed
      end
    end
  end

  defp calculate_timeout(attempt) when attempt <= 1, do: @base_timeout
  defp calculate_timeout(attempt) when attempt <= 3, do: @base_timeout + 1000
  defp calculate_timeout(_attempt), do: @base_timeout + 2000

  defp backoff_delay(1), do: @backoff_min
  defp backoff_delay(attempt) do
    base = :math.pow(@backoff_factor, attempt - 1) * @backoff_min
    jitter = :rand.uniform(1000)
    round(min(base + jitter, @backoff_max))
  end

  defp fetch_with_timeout(post_id, timeout_ms) do
    with {:ok, valid_id} <- validate_post_id(post_id) do
      parent = self()
      ref = make_ref()

      pid = spawn(fn ->
        result = try do
          id_charlist = if is_binary(valid_id), do: to_charlist(valid_id), else: valid_id
          Core.PostClient.get_ipns_from_post(id_charlist)
        rescue
          _ -> nil
        catch
          _, _ -> nil
        end

        send(parent, {ref, normalize_ipns_result(result)})
      end)

      receive do
        {^ref, result} -> result
      after
        timeout_ms ->
          Process.exit(pid, :kill)
          nil
      end
    else
      {:error, _} -> nil
    end
  end

  defp normalize_ipns_result(result) do
    case result do
      :undefined -> nil
      :error -> nil
      {:error, _} -> nil
      nil -> nil
      "" -> nil
      result when is_binary(result) and byte_size(result) > 0 -> result
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
          _ -> nil
        end
      _ -> nil
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
  end

  defp broadcast_ipns_ready(post_id, ipns) do
    spawn(fn ->
      try do
        Phoenix.PubSub.broadcast(Mazaryn.PubSub, "home_feed_updates", {:ipns_ready, post_id, ipns})
        Phoenix.PubSub.broadcast(Mazaryn.PubSub, "post_updates", {:ipns_ready, post_id, ipns})
      rescue
        _ -> :ok
      end
    end)
  end

  def resolve_ipns_batch(post_ids) when is_list(post_ids) do
    spawn(fn ->
      post_ids
      |> Enum.take(3)
      |> Enum.each(fn post_id ->
        spawn(fn -> fetch_ipns_silent(post_id) end)
        Process.sleep(300)
      end)
    end)
  end

  def ipns_processing?(_post_id), do: false

  def get_ipns_with_fallback(post_id, fallback \\ nil) do
    case get_ipns_fast(post_id) do
      nil -> fallback
      ipns -> ipns
    end
  end

  def preload_critical_ipns(post_ids) when is_list(post_ids) do
    spawn(fn ->
      post_ids
      |> Enum.take(2)
      |> Enum.each(&ensure_ipns_background/1)
    end)
  end
end
