defmodule MazarynWeb.HomeLive.CacheInitializer do
  require Logger
  use GenServer

  @cache_cleanup_interval 60_000
  @cache_ttl 300_000
  @max_cache_size 15_000

  @table_configs %{
    post_content_cache: [:named_table, :public, :set, {:write_concurrency, true}, {:read_concurrency, true}],
    ipns_failures: [:named_table, :public, :set, {:write_concurrency, true}],
    post_cache: [:named_table, :public, :set, {:write_concurrency, true}, {:read_concurrency, true}],
    comment_cache: [:named_table, :public, :set, {:write_concurrency, true}, {:read_concurrency, true}],
    likes_cache: [:named_table, :public, :set, {:write_concurrency, true}, {:read_concurrency, true}],
    user_cache: [:named_table, :public, :set, {:write_concurrency, true}, {:read_concurrency, true}],
    translation_cache: [:named_table, :public, :set, {:write_concurrency, true}, {:read_concurrency, true}]
  }

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init_all_tables() do
    @table_configs
    |> Enum.each(fn {table_name, options} ->
      init_table(table_name, options)
    end)

    Logger.info("All ETS cache tables initialized successfully")
    :ok
  end

  def init_table(table_name, options \\ [:named_table, :public, :set]) do
    case :ets.info(table_name) do
      :undefined ->
        try do
          :ets.new(table_name, options)
          Logger.info("Initialized ETS table: #{table_name}")
          :ok
        rescue
          e ->
            Logger.error("Failed to initialize ETS table #{table_name}: #{inspect(e)}")
            :error
        end
      _ ->
        Logger.debug("ETS table #{table_name} already exists")
        :ok
    end
  end

  def safe_ets_insert_fast(table_name, data, options \\ nil) do
    table_options = options || Map.get(@table_configs, table_name, [:named_table, :public, :set])

    init_table(table_name, table_options)

    try do
      :ets.insert(table_name, data)
      :ok
    rescue
      e ->
        Logger.error("Failed to insert into #{table_name}: #{inspect(e)}")
        :error
    end
  end

  def safe_ets_lookup_fast(table_name, key) do
    case :ets.info(table_name) do
      :undefined ->
        []
      _ ->
        try do
          :ets.lookup(table_name, key)
        rescue
          e ->
            Logger.error("Failed to lookup from #{table_name}: #{inspect(e)}")
            []
        end
    end
  end

  def safe_ets_delete(table_name, key) do
    case :ets.info(table_name) do
      :undefined ->
        Logger.debug("Attempted to delete from non-existent table #{table_name}")
        :ok
      _ ->
        try do
          :ets.delete(table_name, key)
          :ok
        rescue
          e ->
            Logger.error("Failed to delete from #{table_name}: #{inspect(e)}")
            :error
        end
    end
  end

  def safe_ets_insert(table_name, data, options \\ nil) do
    safe_ets_insert_fast(table_name, data, options)
  end

  def safe_ets_lookup(table_name, key) do
    safe_ets_lookup_fast(table_name, key)
  end

  def safe_ets_insert_with_ttl(table_name, key, value, ttl \\ @cache_ttl) do
    timestamp = :erlang.system_time(:millisecond)
    expiry = timestamp + ttl
    data = {key, {value, timestamp, expiry}}
    safe_ets_insert_fast(table_name, data)
  end

  def safe_ets_lookup_with_ttl(table_name, key) do
    case safe_ets_lookup_fast(table_name, key) do
      [{^key, {value, _timestamp, expiry}}] ->
        current_time = :erlang.system_time(:millisecond)
        if current_time < expiry do
          {:ok, value}
        else
          safe_ets_delete(table_name, key)
          :expired
        end

      [{^key, value}] ->
        {:ok, value}

      [] ->
        :not_found
    end
  end

  def cache_comments_fast(post_id, comments, ttl \\ @cache_ttl) do
    key = {:comments_fast, post_id}
    smart_insert(:comment_cache, key, comments, ttl)
  end

  def get_cached_comments_fast(post_id) do
    key = {:comments_fast, post_id}
    safe_ets_lookup_with_ttl(:comment_cache, key)
  end

  def cache_post_content_fast(post_id, content, ttl \\ @cache_ttl) do
    key = {:post_content_fast, post_id}
    smart_insert(:post_content_cache, key, content, ttl)
  end

  def get_cached_post_content_fast(post_id) do
    key = {:post_content_fast, post_id}
    safe_ets_lookup_with_ttl(:post_content_cache, key)
  end

  def safe_ets_delete_pattern(table_name, pattern) do
    case :ets.info(table_name) do
      :undefined ->
        Logger.debug("Attempted to delete pattern from non-existent table #{table_name}")
        :ok
      _ ->
        try do
          num_deleted = :ets.select_delete(table_name, [{pattern, [], [true]}])
          Logger.debug("Deleted #{num_deleted} entries matching pattern from #{table_name}")
          {:ok, num_deleted}
        rescue
          e ->
            Logger.error("Failed to delete pattern from #{table_name}: #{inspect(e)}")
            :error
        end
    end
  end

  def safe_ets_size(table_name) do
    case :ets.info(table_name) do
      :undefined -> 0
      _ ->
        try do
          :ets.info(table_name, :size)
        rescue
          _ -> 0
        end
    end
  end

  def safe_ets_info(table_name, item \\ nil) do
    case :ets.info(table_name) do
      :undefined ->
        if item, do: :undefined, else: []
      _ ->
        try do
          if item do
            :ets.info(table_name, item)
          else
            :ets.info(table_name)
          end
        rescue
          _ ->
            if item, do: :undefined, else: []
        end
    end
  end

  def clear_table(table_name) do
    case :ets.info(table_name) do
      :undefined ->
        Logger.debug("Attempted to clear non-existent table #{table_name}")
        :ok
      _ ->
        try do
          :ets.delete_all_objects(table_name)
          Logger.info("Cleared all entries from table #{table_name}")
          :ok
        rescue
          e ->
            Logger.error("Failed to clear table #{table_name}: #{inspect(e)}")
            :error
        end
    end
  end

  def cleanup_expired_entries() do
    GenServer.cast(__MODULE__, :cleanup_expired)
  end

  def cleanup_table(table_name) do
    GenServer.cast(__MODULE__, {:cleanup_table, table_name})
  end

  def get_cache_stats() do
    @table_configs
    |> Map.keys()
    |> Enum.map(fn table_name ->
      size = safe_ets_size(table_name)
      memory = safe_ets_info(table_name, :memory)

      {table_name, %{
        size: size,
        memory: memory,
        exists: :ets.info(table_name) != :undefined
      }}
    end)
    |> Map.new()
  end

  def bulk_insert(table_name, data_list) when is_list(data_list) do
    table_options = Map.get(@table_configs, table_name, [:named_table, :public, :set])
    init_table(table_name, table_options)

    try do
      :ets.insert(table_name, data_list)
      {:ok, length(data_list)}
    rescue
      e ->
        Logger.error("Failed to bulk insert into #{table_name}: #{inspect(e)}")
        :error
    end
  end

  def smart_insert(table_name, key, value, ttl \\ @cache_ttl) do
    current_size = safe_ets_size(table_name)

    if current_size >= @max_cache_size do
      cleanup_oldest_entries(table_name, div(@max_cache_size, 4))
    end

    safe_ets_insert_with_ttl(table_name, key, value, ttl)
  end

  @impl true
  def init(_opts) do
    init_all_tables()
    Process.send_after(self(), :periodic_cleanup, @cache_cleanup_interval)
    {:ok, %{}}
  end

  @impl true
  def handle_cast(:cleanup_expired, state) do
    cleanup_all_expired()
    {:noreply, state}
  end

  def handle_cast({:cleanup_table, table_name}, state) do
    perform_table_cleanup(table_name)
    {:noreply, state}
  end

  @impl true
  def handle_info(:periodic_cleanup, state) do
    cleanup_all_expired()
    Process.send_after(self(), :periodic_cleanup, @cache_cleanup_interval)
    {:noreply, state}
  end

  defp cleanup_all_expired() do
    @table_configs
    |> Map.keys()
    |> Enum.each(&perform_table_cleanup/1)
  end

  defp perform_table_cleanup(table_name) do
    try do
      current_time = :erlang.system_time(:millisecond)

      match_spec = [
        {{:'$1', {:'$2', :'$3', :'$4'}},
         [{:'<', :'$4', current_time}],
         [true]}
      ]

      case safe_ets_info(table_name) do
        :undefined ->
          :ok
        _ ->
          deleted_count = :ets.select_delete(table_name, match_spec)
          if deleted_count > 0 do
            Logger.debug("Cleaned up #{deleted_count} expired entries from #{table_name}")
          end
      end
    rescue
      e ->
        Logger.error("Failed to cleanup table #{table_name}: #{inspect(e)}")
    end
  end

  defp cleanup_oldest_entries(table_name, count_to_remove) do
    try do
      all_entries = :ets.tab2list(table_name)

      entries_to_remove =
        all_entries
        |> Enum.filter(fn {_key, data} ->
          case data do
            {_value, timestamp, _expiry} when is_integer(timestamp) -> true
            _ -> false
          end
        end)
        |> Enum.sort_by(fn {_key, {_value, timestamp, _expiry}} -> timestamp end)
        |> Enum.take(count_to_remove)
        |> Enum.map(fn {key, _data} -> key end)

      Enum.each(entries_to_remove, fn key ->
        :ets.delete(table_name, key)
      end)

      Logger.debug("Removed #{length(entries_to_remove)} oldest entries from #{table_name}")
    rescue
      e ->
        Logger.error("Failed to cleanup oldest entries from #{table_name}: #{inspect(e)}")
    end
  end

  def cache_post_content(post_id, content, ttl \\ @cache_ttl) do
    cache_post_content_fast(post_id, content, ttl)
  end

  def get_cached_post_content(post_id) do
    get_cached_post_content_fast(post_id)
  end

  def cache_comments(post_id, comments, ttl \\ @cache_ttl) do
    cache_comments_fast(post_id, comments, ttl)
  end

  def get_cached_comments(post_id) do
    get_cached_comments_fast(post_id)
  end

  def cache_likes_count(post_id, count, ttl \\ @cache_ttl) do
    key = {:likes_count, post_id}
    smart_insert(:likes_cache, key, count, ttl)
  end

  def get_cached_likes_count(post_id) do
    key = {:likes_count, post_id}
    safe_ets_lookup_with_ttl(:likes_cache, key)
  end

  def clear_post_caches(post_id) do
    keys_to_clear = [
      {:post_content, post_id},
      {:post_content_fast, post_id},
      {:comments, post_id},
      {:comments_fast, post_id},
      {:likes_count, post_id}
    ]

    tables_to_clear = [:post_content_cache, :comment_cache, :likes_cache]

    Enum.zip(tables_to_clear ++ tables_to_clear, keys_to_clear)
    |> Enum.each(fn {table, key} ->
      safe_ets_delete(table, key)
    end)

    Logger.debug("Cleared all caches for post #{post_id}")
  end

  def invalidate_user_caches(user_id) do
    pattern_keys = [
      {:user_posts, user_id, :_},
      {:user_comments, user_id, :_},
      {:user_likes, user_id, :_}
    ]

    Enum.each(pattern_keys, fn pattern ->
      safe_ets_delete_pattern(:post_content_cache, {pattern, :_, :_})
    end)

    Logger.debug("Invalidated user caches for user #{user_id}")
  end
end
