defmodule MazarynWeb.HomeLive.CommentUtilities do
  @moduledoc """
  Optimized utility functions for comment operations with immediate content availability.
  """
  alias Core.PostClient
  alias Mazaryn.Schema.{Comment, Post, Reply}
  alias Mazaryn.Posts
  alias Home.Like

  require Logger

  @content_cache :post_content_cache
  @cache_ttl_seconds 300
  @fast_cache_ttl 180
  @fetch_timeout_ms 1200
  @max_comments_per_fetch 20
  @max_replies_per_comment 10

  @loading_states [
    "Loading...",
    "Loading content...",
    "Content loading...",
    "Content temporarily unavailable",
    "Content loading failed",
    "Loading reply..."
  ]

  def get_comments_with_content_fast(post_id) do
    start_time = System.monotonic_time(:millisecond)
    Logger.info("Fast fetching comments for post #{post_id}")

    result =
      try do
        cache_key = {:comments_fast, post_id}

        case lookup_cache_fast(cache_key) do
          {:hit, comments} ->
            Logger.info("Fast cache hit for post #{post_id}")
            verify_and_ensure_comment_content(comments)

          {:miss, _} ->
            Logger.info("Fast cache miss for post #{post_id}")
            regular_key = {:comments_processed, post_id}

            case lookup_cache_fast(regular_key) do
              {:hit, comments} ->
                processed_comments = verify_and_ensure_comment_content(comments)
                store_in_cache_fast(cache_key, processed_comments)
                processed_comments

              _ ->
                fetch_comments_immediately(post_id, cache_key)
            end

          {:expired, comments} ->
            Logger.info("Fast cache expired for post #{post_id}")
            spawn_background_comment_refresh(post_id, cache_key)
            verify_and_ensure_comment_content(comments)
        end
      rescue
        e ->
          Logger.error("Error getting comments for post #{post_id}: #{inspect(e)}")
          fetch_comments_immediately(post_id, {:comments_fast, post_id})
      end

    log_operation_time("Fast comments fetch", start_time)
    result
  end

  def fetch_comments_immediately(post_id, cache_key) do
    Logger.info("Immediately fetching comments for post #{post_id}")

    parent = self()
    ref = make_ref()

    pid =
      spawn(fn ->
        result =
          try do
            case Posts.get_comment_by_post_id(post_id) do
              comments when is_list(comments) -> comments
              nil -> []
              _ -> []
            end
          rescue
            _ -> []
          end

        send(parent, {ref, result})
      end)

    comments =
      receive do
        {^ref, result} ->
          Logger.info("Immediately fetched #{length(result)} comments for post #{post_id}")
          result
      after
        @fetch_timeout_ms ->
          Process.exit(pid, :kill)
          Logger.warning("Immediate timeout fetching comments for post #{post_id}")
          []
      end

    processed_comments =
      comments
      |> Enum.take(@max_comments_per_fetch)
      |> Enum.map(&process_comment_with_immediate_content/1)
      |> Enum.filter(&(&1 != nil))

    store_in_cache_fast(cache_key, processed_comments)

    processed_comments
  end

  def process_comment_with_immediate_content(comment) do
    try do
      content = get_comment_content_immediate(comment.id)

      replies = get_comment_replies_immediate(comment.id)

      comment
      |> Map.put(:content, content)
      |> Map.put(:like_comment_event, "like-comment")
      |> Map.put(:replies, replies)
    rescue
      e ->
        Logger.error("Error processing comment #{comment.id}: #{inspect(e)}")
        nil
    end
  end

  def get_comment_content_immediate(comment_id) do
    case get_cached_content_fast(:comment, comment_id) do
      content when is_binary(content) ->
        if content != "" and not is_loading_state(content) do
          content
        else
          fetch_content_from_all_sources(comment_id, :comment)
        end

      _ ->
        fetch_content_from_all_sources(comment_id, :comment)
    end
  end

  def fetch_content_from_all_sources(id, type \\ :comment) do
    parent = self()
    ref = make_ref()

    spawn(fn ->
      result =
        try do
          case type do
            :comment -> Core.PostClient.get_comment_content(id)
            :reply -> Core.PostClient.get_reply_content(id)
          end
        rescue
          _ -> nil
        end

      send(parent, {ref, {:source1, result}})
    end)

    spawn(fn ->
      result =
        try do
          case type do
            :comment ->
              case Posts.get_comment_by_id(id) do
                %{content: content} when is_binary(content) -> content
                _ -> nil
              end

            :reply ->
              nil
          end
        rescue
          _ -> nil
        end

      send(parent, {ref, {:source2, result}})
    end)

    spawn(fn ->
      result =
        try do
          case type do
            :comment -> :postdb.get_comment_content(id)
            :reply -> :postdb.get_reply_content(id)
          end
        rescue
          _ -> nil
        end

      send(parent, {ref, {:source3, result}})
    end)

    content = collect_fetch_results(ref, 3, 600, [])

    final_content = content || "Content unavailable"

    if is_valid_content(final_content) do
      cache_content_fast(type, id, final_content)
    end

    final_content
  end

  defp collect_fetch_results(_ref, 0, _timeout, results) do
    results
    |> Enum.map(&normalize_content/1)
    |> Enum.find(&is_valid_content/1)
  end

  defp collect_fetch_results(ref, remaining, timeout, results) do
    receive do
      {^ref, {_source, result}} ->
        updated_results = [result | results]

        case normalize_content(result) do
          content when is_binary(content) and byte_size(content) > 0 ->
            content

          _ ->
            collect_fetch_results(ref, remaining - 1, timeout, updated_results)
        end
    after
      timeout ->
        collect_fetch_results(ref, 0, 0, results)
    end
  end

  def verify_and_ensure_comment_content(comments) do
    Enum.map(comments, fn comment ->
      content =
        case comment.content do
          content when is_binary(content) ->
            if content != "" and not is_loading_state(content) do
              content
            else
              get_comment_content_immediate(comment.id)
            end

          _ ->
            get_comment_content_immediate(comment.id)
        end

      replies =
        case comment.replies do
          replies when is_list(replies) ->
            Enum.map(replies, fn reply ->
              reply_content =
                case reply.content do
                  content when is_binary(content) ->
                    if content != "" and not is_loading_state(content) do
                      content
                    else
                      get_reply_content_immediate(reply.id)
                    end

                  _ ->
                    get_reply_content_immediate(reply.id)
                end

              Map.put(reply, :content, reply_content)
            end)

          _ ->
            []
        end

      comment
      |> Map.put(:content, content)
      |> Map.put(:replies, replies)
    end)
  end

  def get_reply_content_immediate(reply_id) do
    case get_cached_content_fast(:reply, reply_id) do
      content when is_binary(content) ->
        if content != "" and not is_loading_state(content) do
          content
        else
          fetch_content_from_all_sources(reply_id, :reply)
        end

      _ ->
        fetch_content_from_all_sources(reply_id, :reply)
    end
  end

  def get_comment_replies_immediate(comment_id) do
    parent = self()
    ref = make_ref()

    pid =
      spawn(fn ->
        result =
          try do
            :postdb.get_comment_replies(to_charlist(comment_id))
          rescue
            _ -> []
          end

        send(parent, {ref, result})
      end)

    receive do
      {^ref, replies} ->
        replies
        |> Enum.take(@max_replies_per_comment)
        |> Enum.map(&process_reply_immediate/1)
        |> Enum.filter(&(&1 != nil))
    after
      800 ->
        Process.exit(pid, :kill)
        []
    end
  rescue
    _ -> []
  end

  defp process_reply_immediate(reply) do
    case build_reply_from_erl_data(reply) do
      {:ok, built_reply} ->
        content = get_reply_content_immediate(built_reply.id)
        Map.put(built_reply, :content, content)

      {:error, _} ->
        nil
    end
  end

  def rebuild_post_safe(post_id) do
    start_time = System.monotonic_time(:millisecond)
    Logger.debug("Safely rebuilding post: #{post_id}")

    try do
      cache_key = {:post_fast, post_id}

      case lookup_cache_fast(cache_key) do
        {:hit, post} ->
          {:ok, post}

        _ ->
          case PostClient.get_by_id(post_id) do
            :post_not_exist ->
              Logger.warning("Post #{post_id} does not exist")
              {:error, :post_not_exist}

            post_data when post_data != nil ->
              changeset = Post.erl_changeset(post_data)

              case Post.build(changeset) do
                {:ok, post} ->
                  store_in_cache_fast(cache_key, post)
                  log_operation_time("Safe post rebuild", start_time, :debug)
                  {:ok, post}

                {:error, reason} ->
                  Logger.error("Failed to build post from changeset: #{inspect(reason)}")
                  {:error, reason}
              end

            nil ->
              Logger.warning("PostClient.get_by_id returned nil for post #{post_id}")
              {:error, :post_not_found}

            other ->
              Logger.warning("Unexpected result from PostClient.get_by_id: #{inspect(other)}")
              {:error, :unexpected_result}
          end
      end
    rescue
      exception ->
        Logger.error("Exception in safe rebuild_post: #{inspect(exception)}")
        {:error, exception}
    end
  end

  def rebuild_post_fast(post_id) do
    case rebuild_post_safe(post_id) do
      {:ok, post} ->
        post

      {:error, _reason} ->
        %{
          id: post_id,
          author: "unknown",
          content: "Post content unavailable",
          inserted_at: DateTime.utc_now(),
          updated_at: DateTime.utc_now()
        }
    end
  end

  def cache_content_fast(type, id, content) do
    unless is_loading_state(content) do
      init_cache_table()
      cache_key = {type, id}
      timestamp = :erlang.system_time(:second)

      try do
        :ets.insert(@content_cache, {cache_key, content, timestamp})
        Logger.debug("Fast cached content for #{type}:#{id}")
      rescue
        e ->
          Logger.error("Failed to fast cache content for #{type}:#{id}: #{inspect(e)}")
      end
    end

    content
  end

  def clear_content_cache_fast(type, id) do
    init_cache_table()
    cache_key = {type, id}

    try do
      :ets.delete(@content_cache, cache_key)
    rescue
      e ->
        Logger.error("Failed to clear fast cache: #{inspect(e)}")
    end
  end

  def create_temp_reply(user_id, comment_id, content) do
    temp_id = "temp_" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))

    cache_content_fast(:reply, temp_id, content)

    %{
      id: temp_id,
      content: content,
      user_id: user_id,
      comment_id: comment_id,
      inserted_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now(),
      is_temp: true
    }
  end

  def build_like(like_data) do
    {:ok, like} =
      like_data
      |> Like.erl_changeset()
      |> Like.build()

    like
  end

  defp spawn_background_comment_refresh(post_id, cache_key) do
    spawn(fn ->
      try do
        Logger.info("Background comment refresh started for post #{post_id}")
        _fresh_comments = fetch_comments_immediately(post_id, cache_key)
        Logger.info("Background comment refresh completed for post #{post_id}")
      rescue
        _ -> :ok
      end
    end)
  end

  defp lookup_cache_fast(cache_key) do
    init_cache_table()

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, data, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp

        if age < @fast_cache_ttl do
          {:hit, data}
        else
          {:expired, data}
        end

      [] ->
        {:miss, nil}
    end
  rescue
    e ->
      Logger.error("Error in fast lookup_cache: #{inspect(e)}")
      {:miss, nil}
  end

  defp store_in_cache_fast(cache_key, data) do
    init_cache_table()
    timestamp = :erlang.system_time(:second)

    try do
      :ets.insert(@content_cache, {cache_key, data, timestamp})
      Logger.debug("Fast stored data in cache with key #{inspect(cache_key)}")
    rescue
      e ->
        Logger.error("Failed to fast store in cache: #{inspect(e)}")
    end
  end

  defp get_cached_content_fast(type, id) do
    init_cache_table()
    cache_key = {type, id}

    try do
      case :ets.lookup(@content_cache, cache_key) do
        [{^cache_key, content, timestamp}] ->
          age = :erlang.system_time(:second) - timestamp

          if age < @cache_ttl_seconds and is_valid_content(content) do
            Logger.debug("Fast cache hit for #{type}:#{id}")
            content
          else
            Logger.debug("Fast cache expired for #{type}:#{id}")
            nil
          end

        [] ->
          Logger.debug("Fast cache miss for #{type}:#{id}")
          nil
      end
    rescue
      e ->
        Logger.error("Error in fast get_cached_content: #{inspect(e)}")
        nil
    end
  end

  def init_cache_table() do
    case :ets.info(@content_cache) do
      :undefined ->
        :ets.new(@content_cache, [:named_table, :public, :set])
        Logger.info("Initialized ETS cache table: #{@content_cache}")
        :ok

      _ ->
        Logger.debug("ETS cache table #{@content_cache} already exists")
        :ok
    end
  rescue
    e ->
      Logger.error("Failed to initialize ETS cache table: #{inspect(e)}")
      :error
  end

  defp build_reply_from_erl_data(reply_data) do
    reply_data
    |> Reply.erl_changeset()
    |> Reply.build()
  end

  defp normalize_content(content) do
    cond do
      is_binary(content) and content != "" ->
        content

      is_list(content) ->
        case List.to_string(content) do
          "" -> nil
          str -> str
        end

      true ->
        nil
    end
  end

  defp is_valid_content(content) do
    is_binary(content) and
      content != "" and
      content != "Content unavailable" and
      not is_loading_state(content)
  end

  defp is_loading_state(content) do
    content in @loading_states
  end

  defp log_operation_time(operation, start_time, level \\ :info) do
    duration = System.monotonic_time(:millisecond) - start_time
    Logger.log(level, "#{operation} completed in #{duration}ms")
  end

  def get_comments_with_content_optimized(post_id) do
    get_comments_with_content_fast(post_id)
  end

  def cache_content(type, id, content) do
    cache_content_fast(type, id, content)
  end

  def clear_content_cache(type, id) do
    clear_content_cache_fast(type, id)
  end

  def rebuild_post(post_id) do
    rebuild_post_fast(post_id)
  end

  def clear_comments_cache(post_id) do
    init_cache_table()

    cache_keys = [
      {:comments_fast, post_id},
      {:comments_processed, post_id}
    ]

    try do
      Enum.each(cache_keys, fn key ->
        :ets.delete(@content_cache, key)
      end)
    rescue
      e ->
        Logger.error("Failed to clear comments cache: #{inspect(e)}")
    end
  end

  def get_cached_content(type, id) do
    get_cached_content_fast(type, id)
  end

  def safe_ets_lookup_with_ttl(table_name, key) do
    case :ets.lookup(table_name, key) do
      [{^key, {value, _timestamp, expiry}}] ->
        current_time = :erlang.system_time(:millisecond)

        if current_time < expiry do
          {:ok, value}
        else
          :ets.delete(table_name, key)
          :expired
        end

      [{^key, value}] ->
        {:ok, value}

      [] ->
        :not_found
    end
  rescue
    _ ->
      :not_found
  end

  def fetch_and_process_comments_improved(post_id, cache_key) do
    fetch_comments_immediately(post_id, cache_key)
  end

  def get_comment_content_with_fallbacks(comment_id) do
    get_comment_content_immediate(comment_id)
  end

  def process_single_comment_with_content(comment) do
    process_comment_with_immediate_content(comment)
  end

  def get_comment_replies_optimized(comment_id) do
    get_comment_replies_immediate(comment_id)
  end

  def attempt_synchronous_fetch(comment_id) do
    case get_comment_content_immediate(comment_id) do
      "Content unavailable" -> {:error, :no_content}
      content -> {:ok, content}
    end
  end

  def fetch_comment_content_final_attempt(comment_id) do
    case get_comment_content_immediate(comment_id) do
      "Content unavailable" -> {:error, :no_content}
      content -> {:ok, content}
    end
  end

  def spawn_comment_content_fetch_improved(comment_id) do
    :ok
  end

  def save_comment_directly(comment_id, post_id, author, content) do
    try do
      case PostClient.create(author, post_id, content) do
        {:ok, result} ->
          Logger.info("PostClient.create_comment succeeded")
          {:ok, result}

        {:error, reason} ->
          Logger.info("PostClient.create_comment failed: #{inspect(reason)}")
          try_postdb_save(comment_id, post_id, author, content)

        other ->
          Logger.warning("PostClient.create_comment returned: #{inspect(other)}")
          try_postdb_save(comment_id, post_id, author, content)
      end
    rescue
      _ ->
        Logger.info("PostClient.create_comment not available, trying postdb...")
        try_postdb_save(comment_id, post_id, author, content)
    end
  end

  defp try_postdb_save(comment_id, post_id, author, content) do
    try do
      comment_data = {
        to_charlist(comment_id),
        to_charlist(post_id),
        to_charlist(author),
        to_charlist(content),
        :erlang.system_time(:second),
        []
      }

      case :postdb.save_comment(comment_data) do
        :ok ->
          Logger.info("Direct postdb save succeeded")

          comment = %{
            id: comment_id,
            post_id: post_id,
            author: author,
            content: content,
            inserted_at: DateTime.utc_now(),
            likes: []
          }

          {:ok, comment}

        error ->
          Logger.info("Direct postdb save failed: #{inspect(error)}")
          {:error, error}
      end
    rescue
      exception ->
        Logger.error("Exception in postdb save: #{inspect(exception)}")
        {:error, exception}
    end
  end
end
