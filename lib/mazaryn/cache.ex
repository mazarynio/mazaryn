defmodule Mazaryn.Translator.Cache do
  @moduledoc """
  ETS cache for translations.
  Key: {:translation, post_id, target_lang}
  TTL: 24h
  """

  @tab :translation_cache
  @ttl 24 * 60 * 60

  def ensure! do
    case :ets.whereis(@tab) do
      :undefined -> :ets.new(@tab, [:set, :public, :named_table, {:read_concurrency, true}])
      _ -> :ok
    end
    :ok
  end

  def put(post_id, target, text) when is_binary(text) do
    ensure!()
    ts = :erlang.system_time(:second)
    :ets.insert(@tab, {{:translation, post_id, target}, text, ts})
    :ok
  end

  def get(post_id, target) do
    ensure!()
    key = {:translation, post_id, target}

    case :ets.lookup(@tab, key) do
      [{^key, val, ts}] ->
        if fresh?(ts), do: {:hit, val}, else: :miss
      _ ->
        :miss
    end
  end

  defp fresh?(ts), do: :erlang.system_time(:second) - ts < @ttl
end
