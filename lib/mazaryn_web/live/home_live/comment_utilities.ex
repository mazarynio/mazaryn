defmodule MazarynWeb.HomeLive.CommentUtilities do
  @moduledoc """
  Utility functions for comment operations.

  This module provides:
  - Content caching and retrieval
  - Content fetching from multiple sources with fallbacks
  - Post rebuilding and optimization
  - Background content synchronization
  - Direct database operations
  """

  alias Core.PostClient
  alias Mazaryn.Schema.{Comment, Post, Reply}
  alias Mazaryn.Posts
  alias Home.Like

  require Logger

  @content_cache :post_content_cache
  @cache_ttl_seconds 180
  @fetch_timeout_ms 2000
  @max_comments_per_fetch 5
  @max_replies_per_comment 3

  @doc """
  Initializes the ETS cache table if it doesn't exist.
  """
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

  # Content Management

  @doc """
  Gets comments with content for a post, using caching and optimization.
  """
  def get_comments_with_content_optimized(post_id) do
    start_time = System.monotonic_time(:millisecond)
    Logger.info("Fetching comments for post #{post_id}")

    result = try do
      cache_key = {:comments_processed, post_id}

      case lookup_cache(cache_key) do
        {:hit, comments} ->
          Logger.info("Cache hit for post #{post_id}")
          verify_and_fix_comment_content(comments, post_id)

        {:miss, _} ->
          Logger.info("Cache miss for post #{post_id}")
          fetch_and_process_comments_improved(post_id, cache_key)

        {:expired, _} ->
          Logger.info("Cache expired for post #{post_id}")
          fetch_and_process_comments_improved(post_id, cache_key)
      end
    rescue
      e ->
        Logger.error("Error getting comments for post #{post_id}: #{inspect(e)}")
        []
    end

    log_operation_time("Comments fetch", start_time)
    result
  end

  @doc """
  Fetches and processes comments with improved error handling and timeouts.
  """
  def fetch_and_process_comments_improved(post_id, cache_key) do
    start_time = System.monotonic_time(:millisecond)
    Logger.info("Starting fresh comment fetch for post #{post_id}")

    task = Task.async(fn ->
      fetch_start = System.monotonic_time(:millisecond)
      result = Posts.get_comment_by_post_id(post_id)
      fetch_end = System.monotonic_time(:millisecond)

      Logger.info("Database fetch took #{fetch_end - fetch_start}ms")
      result || []
    end)

    comments = case Task.yield(task, @fetch_timeout_ms) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} ->
        Logger.info("Fetched #{length(result)} comments for post #{post_id}")
        result

      nil ->
        Logger.warning("Timeout fetching comments for post #{post_id}")
        []
    end

    processed_comments = comments
                         |> Enum.take(@max_comments_per_fetch)
                         |> process_comments_batch()
                         |> Enum.filter(&(&1 != nil))

    store_in_cache(cache_key, processed_comments)
    log_operation_time("Comment processing", start_time)

    processed_comments
  end

  @doc """
  Processes a single comment with content and metadata.
  """
  def process_single_comment_with_content(comment) do
    start_time = System.monotonic_time(:millisecond)
    Logger.debug("Processing comment #{comment.id}")

    result = try do
      content = get_comment_content_with_fallbacks(comment.id)
      replies = get_comment_replies_optimized(comment.id)

      comment
      |> Map.put(:content, content)
      |> Map.put(:like_comment_event, "like-comment")
      |> Map.put(:replies, replies)
    rescue
      e ->
        Logger.error("Error processing comment #{comment.id}: #{inspect(e)}")
        nil
    end

    log_operation_time("Single comment processing", start_time, :debug)
    result
  end

  @doc """
  Gets comment content with multiple fallback strategies.
  """
  def get_comment_content_with_fallbacks(comment_id) do
    start_time = System.monotonic_time(:millisecond)
    Logger.debug("Fetching content for comment #{comment_id}")

    result = case get_cached_content(:comment, comment_id) do
      content when is_binary(content) and content != "" ->
        if is_valid_content(content) do
          Logger.debug("Using cached content for comment #{comment_id}")
          content
        else
          fetch_content_fallback(comment_id)
        end

      _ ->
        Logger.debug("Cache miss for comment #{comment_id}, attempting synchronous fetch")

        case attempt_synchronous_fetch(comment_id) do
          {:ok, content} ->
            if is_valid_content(content) do
              Logger.debug("Synchronous fetch succeeded for comment #{comment_id}")
              cache_content(:comment, comment_id, content)
              content
            else
              fetch_content_fallback(comment_id)
            end

          _ ->
            Logger.debug("Synchronous fetch failed, attempting final fetch for comment #{comment_id}")

            case fetch_comment_content_final_attempt(comment_id) do
              {:ok, content} ->
                cache_content(:comment, comment_id, content)
                content

              _ ->
                Logger.debug("Final fetch failed, spawning background fetch for comment #{comment_id}")
                spawn_comment_content_fetch_improved(comment_id)
                "Loading content..."
            end
        end
    end

    log_operation_time("Content fetch", start_time, :debug)
    result
  end

  defp fetch_content_fallback(comment_id) do
    Logger.debug("Cache miss for comment #{comment_id}, attempting synchronous fetch")

    case attempt_synchronous_fetch(comment_id) do
      {:ok, content} ->
        if is_valid_content(content) do
          Logger.debug("Synchronous fetch succeeded for comment #{comment_id}")
          cache_content(:comment, comment_id, content)
          content
        else
          final_fallback(comment_id)
        end

      _ ->
        final_fallback(comment_id)
    end
  end

  defp final_fallback(comment_id) do
    Logger.debug("Synchronous fetch failed, attempting final fetch for comment #{comment_id}")

    case fetch_comment_content_final_attempt(comment_id) do
      {:ok, content} ->
        cache_content(:comment, comment_id, content)
        content

      _ ->
        Logger.debug("Final fetch failed, spawning background fetch for comment #{comment_id}")
        spawn_comment_content_fetch_improved(comment_id)
        "Loading content..."
    end
  end

  @doc """
  Gets replies for a comment with optimization and caching.
  """
  def get_comment_replies_optimized(comment_id) do
    try do
      Logger.debug("Fetching replies for comment #{comment_id}")

      task = Task.async(fn ->
        :postdb.get_comment_replies(to_charlist(comment_id))
      end)

      case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
        {:ok, replies} ->
          replies
          |> Enum.take(@max_replies_per_comment)
          |> process_replies_batch()
          |> Enum.filter(&(&1 != nil))

        nil -> []
      end
    rescue
      e ->
        Logger.error("Error getting replies for comment #{comment_id}: #{inspect(e)}")
        []
    end
  end

  # Content Fetching Strategies

  @doc """
  Attempts synchronous content fetch with multiple strategies.
  """
  def attempt_synchronous_fetch(comment_id) do
    strategies = [
      &fetch_comment_content_direct/1,
      &fetch_comment_content_alternative/1,
      &fetch_from_ipfs_alternative/1
    ]

    Enum.reduce_while(strategies, {:error, :all_failed}, fn strategy, _acc ->
      case strategy.(comment_id) do
        {:ok, content} ->
          if is_valid_content(content) do
            {:halt, {:ok, content}}
          else
            Process.sleep(200)
            {:cont, {:error, :failed}}
          end

        _ ->
          Process.sleep(200)
          {:cont, {:error, :failed}}
      end
    end)
  end

  @doc """
  Makes a final attempt to fetch comment content with extended timeout.
  """
  def fetch_comment_content_final_attempt(comment_id) do
    start_time = System.monotonic_time(:millisecond)
    Logger.debug("Starting final attempt to fetch comment content for #{comment_id}")

    task = Task.async(fn ->
      try do
        case Core.PostClient.get_comment_content(comment_id) do
          content when is_binary(content) and content != "" ->
            {:ok, content}

          content when is_list(content) ->
            string_content = List.to_string(content)
            if string_content != "", do: {:ok, string_content}, else: fallback_to_db(comment_id)

          _ ->
            fallback_to_db(comment_id)
        end
      rescue
        e ->
          Logger.error("Error in final fetch for comment #{comment_id}: #{inspect(e)}")
          {:error, e}
      end
    end)

    result = case Task.yield(task, 1500) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} ->
        Logger.debug("Final fetch completed for comment #{comment_id}")
        result

      nil ->
        Logger.debug("Timeout after 1500ms in final fetch for comment #{comment_id}")
        {:error, :timeout}
    end

    log_operation_time("Final content fetch", start_time, :debug)
    result
  end

  @doc """
  Fetches comment content directly from PostClient.
  """
  def fetch_comment_content_direct(comment_id) do
    task = Task.async(fn ->
      try do
        case Core.PostClient.get_comment_content(comment_id) do
          content when is_binary(content) and content != "" ->
            {:ok, content}

          content when is_list(content) ->
            string_content = List.to_string(content)
            if string_content != "", do: {:ok, string_content}, else: {:error, :empty_content}

          _ ->
            {:error, :no_content}
        end
      rescue
        e -> {:error, e}
      end
    end)

    case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  @doc """
  Fetches comment content from alternative source (database).
  """
  def fetch_comment_content_alternative(comment_id) do
    try do
      case Posts.get_comment_by_id(comment_id) do
        %{content: content} when is_binary(content) and content != "" ->
          {:ok, content}

        %{content: content} when is_list(content) ->
          {:ok, List.to_string(content)}

        _ ->
          fetch_from_ipfs_alternative(comment_id)
      end
    rescue
      _ -> {:error, :alternative_failed}
    end
  end

  @doc """
  Fetches content from IPFS alternative source.
  """
  def fetch_from_ipfs_alternative(comment_id) do
    try do
      case :postdb.get_comment_content(comment_id) do
        content when is_binary(content) and content != "" ->
          {:ok, content}

        content when is_list(content) ->
          string_content = List.to_string(content)
          if string_content != "", do: {:ok, string_content}, else: {:error, :empty_alternative}

        _ ->
          {:error, :no_alternative_content}
      end
    rescue
      _ -> {:error, :ipfs_alternative_failed}
    end
  end

  # Background Tasks

  @doc """
  Spawns an improved background task to fetch comment content.
  """
  def spawn_comment_content_fetch_improved(comment_id) do
    parent_pid = self()

    Task.start(fn ->
      Logger.debug("Background fetch started for comment #{comment_id}")

      content = fetch_content_with_strategies(comment_id)

      if content && is_valid_content(content) do
        cache_content(:comment, comment_id, content)
        send(parent_pid, {:comment_content_updated, comment_id, content})
        Logger.debug("Background fetch completed for comment #{comment_id}")
      else
        Logger.debug("Background fetch failed for comment #{comment_id}")
      end
    end)
  end

  # Direct Database Operations

  @doc """
  Saves comment directly using multiple backend strategies.
  """
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

  # Post Management

  @doc """
  Rebuilds a post with proper error handling and logging.
  """
  def rebuild_post(post_id) do
    start_time = System.monotonic_time(:millisecond)
    Logger.debug("Rebuilding post: #{post_id}")

    try do
      post_data = PostClient.get_by_id(post_id)
      changeset = Post.erl_changeset(post_data)
      {:ok, post} = Post.build(changeset)

      log_operation_time("Post rebuild", start_time, :debug)
      post
    rescue
      exception ->
        Logger.error("Exception in rebuild_post: #{inspect(exception)}")
        reraise exception, __STACKTRACE__
    end
  end

  # Caching Functions

  @doc """
  Gets cached content with TTL validation.
  """
  def get_cached_content(type, id) do
    init_cache_table()
    cache_key = {type, id}

    try do
      case :ets.lookup(@content_cache, cache_key) do
        [{^cache_key, content, timestamp}] ->
          age = :erlang.system_time(:second) - timestamp

          if age < 600 and is_valid_content(content) do
            Logger.debug("Valid cache hit for #{type}:#{id} (age: #{age}s)")
            content
          else
            Logger.debug("Invalid or expired cache for #{type}:#{id}")
            :ets.delete(@content_cache, cache_key)
            nil
          end

        [] ->
          Logger.debug("Cache miss for #{type}:#{id}")
          nil
      end
    rescue
      e ->
        Logger.error("Error in get_cached_content: #{inspect(e)}")
        nil
    end
  end

  @doc """
  Caches content with timestamp.
  """
  def cache_content(type, id, content) do
    unless is_loading_state(content) do
      init_cache_table()
      cache_key = {type, id}
      timestamp = :erlang.system_time(:second)

      try do
        :ets.insert(@content_cache, {cache_key, content, timestamp})
        Logger.debug("Cached content for #{type}:#{id}")
      rescue
        e ->
          Logger.error("Failed to cache content for #{type}:#{id}: #{inspect(e)}")
      end
    else
      Logger.debug("Skipping cache for loading state: #{content}")
    end

    content
  end

  @doc """
  Clears comments cache for a specific post.
  """
  def clear_comments_cache(post_id) do
    init_cache_table()
    cache_key = {:comments_processed, post_id}

    try do
      :ets.delete(@content_cache, cache_key)
    rescue
      e ->
        Logger.error("Failed to clear comments cache: #{inspect(e)}")
    end
  end

  @doc """
  Clears content cache for a specific item.
  """
  def clear_content_cache(type, id) do
    init_cache_table()
    cache_key = {type, id}

    try do
      :ets.delete(@content_cache, cache_key)
    rescue
      e ->
        Logger.error("Failed to clear content cache: #{inspect(e)}")
    end
  end

  # Reply Management

  @doc """
  Creates a temporary reply for optimistic updates.
  """
  def create_temp_reply(user_id, comment_id, content) do
    temp_id = "temp_" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))

    cache_content(:reply, temp_id, content)

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

  @doc """
  Gets reply content with fallback strategies.
  """
  def get_reply_content_with_fallbacks(reply_id) do
    case get_cached_content(:reply, reply_id) do
      content when is_binary(content) and content != "" ->
        if is_valid_content(content) do
          Logger.debug("Using cached content for reply #{reply_id}")
          content
        else
          fetch_reply_fallback(reply_id)
        end

      _ ->
        fetch_reply_fallback(reply_id)
    end
  end

  defp fetch_reply_fallback(reply_id) do
    case fetch_reply_content_direct(reply_id) do
      {:ok, content} ->
        if is_valid_content(content) do
          cache_content(:reply, reply_id, content)
          content
        else
          final_reply_fallback(reply_id)
        end

      _ ->
        final_reply_fallback(reply_id)
    end
  end

  defp final_reply_fallback(reply_id) do
    case fetch_reply_content_alternative(reply_id) do
      {:ok, content} ->
        cache_content(:reply, reply_id, content)
        content

      _ ->
        spawn_reply_content_fetch_improved(reply_id)
        "Loading reply..."
    end
  end

  # Helper Functions

  @doc """
  Builds a Like struct from erlang data.
  """
  def build_like(like_data) do
    {:ok, like} = like_data
                  |> Like.erl_changeset()
                  |> Like.build()
    like
  end

  # Private Functions

  defp lookup_cache(cache_key) do
    init_cache_table()

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, comments, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp

        if age < @cache_ttl_seconds do
          {:hit, comments}
        else
          {:expired, comments}
        end

      [] ->
        {:miss, nil}
    end
  rescue
    e ->
      Logger.error("Error in lookup_cache: #{inspect(e)}")
      {:miss, nil}
  end

  defp store_in_cache(cache_key, data) do
    init_cache_table()
    timestamp = :erlang.system_time(:second)

    try do
      :ets.insert(@content_cache, {cache_key, data, timestamp})
      Logger.debug("Stored data in cache with key #{inspect(cache_key)}")
    rescue
      e ->
        Logger.error("Failed to store in cache: #{inspect(e)}")
    end
  end

  defp process_comments_batch(comments) do
    Enum.map(comments, &process_single_comment_with_content/1)
  end

  defp process_replies_batch(replies) do
    Enum.map(replies, fn reply ->
      case build_reply_from_erl_data(reply) do
        {:ok, built_reply} ->
          content = get_reply_content_with_fallbacks(built_reply.id)
          Map.put(built_reply, :content, content)

        {:error, _} ->
          nil
      end
    end)
  end

  defp build_reply_from_erl_data(reply_data) do
    reply_data
    |> Reply.erl_changeset()
    |> Reply.build()
  end

  defp verify_and_fix_comment_content(comments, post_id) do
    needs_refresh = Enum.any?(comments, fn comment ->
      is_loading_state(comment.content) or is_nil(comment.content) or comment.content == ""
    end)

    if needs_refresh do
      Logger.info("Some comments need content refresh, fetching fresh data")
      fetch_and_process_comments_improved(post_id, {:comments_processed, post_id})
    else
      comments
    end
  end

  defp fetch_content_with_strategies(comment_id) do
    case fetch_comment_content_direct(comment_id) do
      {:ok, content} -> content
      _ ->
        case fetch_comment_content_alternative(comment_id) do
          {:ok, content} -> content
          _ ->
            case fetch_content_with_timeout(:comment, comment_id, 5000) do
              {:ok, content} -> content
              _ -> nil
            end
        end
    end
  end

  defp fallback_to_db(comment_id) do
    case Posts.get_comment_by_id(comment_id) do
      %{content: db_content} when is_binary(db_content) and db_content != "" ->
        {:ok, db_content}
      _ ->
        {:error, :no_content}
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

  defp fetch_reply_content_direct(reply_id) do
    task = Task.async(fn ->
      try do
        case Core.PostClient.get_reply_content(reply_id) do
          content when is_binary(content) and content != "" ->
            {:ok, content}

          content when is_list(content) ->
            string_content = List.to_string(content)
            if string_content != "", do: {:ok, string_content}, else: {:error, :empty_content}

          _ ->
            {:error, :no_content}
        end
      rescue
        e -> {:error, e}
      end
    end)

    case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  defp fetch_reply_content_alternative(reply_id) do
    try do
      case :postdb.get_reply(reply_id) do
        reply_data when reply_data != nil ->
          case build_reply_from_erl_data(reply_data) do
            {:ok, %{content: content}} when is_binary(content) and content != "" ->
              {:ok, content}

            {:ok, %{content: content}} when is_list(content) ->
              {:ok, List.to_string(content)}

            _ ->
              fetch_reply_from_ipfs_alternative(reply_id)
          end

        _ ->
          fetch_reply_from_ipfs_alternative(reply_id)
      end
    rescue
      _ -> {:error, :alternative_failed}
    end
  end

  defp fetch_reply_from_ipfs_alternative(reply_id) do
    try do
      case :postdb.get_reply_content_alternative(reply_id) do
        content when is_binary(content) and content != "" ->
          {:ok, content}

        content when is_list(content) ->
          string_content = List.to_string(content)
          if string_content != "", do: {:ok, string_content}, else: {:error, :empty_alternative}

        _ ->
          {:error, :no_alternative_content}
      end
    rescue
      _ -> {:error, :ipfs_alternative_failed}
    end
  end

  defp spawn_reply_content_fetch_improved(reply_id) do
    parent_pid = self()

    Task.start(fn ->
      Logger.debug("Background fetch started for reply #{reply_id}")

      content = case fetch_reply_content_direct(reply_id) do
        {:ok, content} -> content
        _ ->
          case fetch_reply_content_alternative(reply_id) do
            {:ok, content} -> content
            _ ->
              case fetch_content_with_timeout(:reply, reply_id, 5000) do
                {:ok, content} -> content
                _ -> nil
              end
          end
      end

      if content && is_valid_content(content) do
        cache_content(:reply, reply_id, content)
        send(parent_pid, {:reply_content_updated, reply_id, content})
        Logger.debug("Background fetch completed for reply #{reply_id}")
      else
        Logger.debug("Background fetch failed for reply #{reply_id}")
      end
    end)
  end

  defp fetch_content_with_timeout(type, id, timeout) do
    task = Task.async(fn ->
      try do
        start_time = System.monotonic_time(:millisecond)

        content = case type do
          :comment -> Core.PostClient.get_comment_content(id)
          :reply -> Core.PostClient.get_reply_content(id)
        end

        result = case content do
          {:error, :content_processing} -> {:error, :content_processing}
          {:error, :content_cache_missing} -> {:error, :content_cache_missing}
          content when is_binary(content) and content != "" -> {:ok, content}
          content when is_list(content) ->
            case List.to_string(content) do
              "" -> {:error, :empty_content}
              str -> {:ok, str}
            end
          _ -> {:error, :invalid_content}
        end

        end_time = System.monotonic_time(:millisecond)
        duration = end_time - start_time

        if duration > 2000 do
          Logger.warning("Slow IPFS #{type} fetch: #{duration}ms for #{type} #{id}")
        end

        result
      catch
        _, e ->
          Logger.error("Error fetching #{type} content from IPFS: #{inspect(e)}")
          {:error, e}
      end
    end)

    case Task.yield(task, timeout) || Task.shutdown(task) do
      {:ok, result} -> result
      nil -> {:timeout, "Timeout after #{timeout}ms"}
    end
  end

  # Validation Helpers

  defp is_valid_content(content) do
    is_binary(content) and
    content != "" and
    not is_loading_state(content)
  end

  defp is_loading_state(content) do
    content in [
      "Loading...",
      "Loading content...",
      "Content loading...",
      "Content temporarily unavailable",
      "Content loading failed",
      "Loading reply..."
    ]
  end

  defp log_operation_time(operation, start_time, level \\ :info) do
    duration = System.monotonic_time(:millisecond) - start_time
    Logger.log(level, "#{operation} completed in #{duration}ms")
  end
end
