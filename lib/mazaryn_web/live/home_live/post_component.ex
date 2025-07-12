defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.UserClient
  alias Core.PostClient
  alias Mazaryn.Schema.Comment
  alias Mazaryn.Posts
  alias Mazaryn.Schema.Post
  alias Phoenix.LiveView.JS

  @content_cache :post_content_cache
  @batch_size 5
  @max_concurrent_tasks 8

  defp log_performance(operation, func) do
    start_time = :erlang.system_time(:millisecond)
    result = func.()
    end_time = :erlang.system_time(:millisecond)
    duration = end_time - start_time

    IO.puts("🕐 PERFORMANCE: #{operation} took #{duration}ms")

    if duration > 500 do
      IO.puts("⚠️  SLOW OPERATION DETECTED: #{operation} took #{duration}ms - INVESTIGATE!")
    end

    result
  end

  @impl Phoenix.LiveComponent
  def update_many(list_of_assigns) do
    IO.puts("=== OPTIMIZED UPDATE_MANY called ===")
    IO.inspect(length(list_of_assigns), label: "Number of assigns")

    total_start = :erlang.system_time(:millisecond)

    ensure_cache_table()

    changeset = Comment.changeset(%Comment{})
    update_comment_changeset = Comment.changeset(%Comment{})
    update_post_changeset = Post.changeset(%Post{})

    result = list_of_assigns
    |> Enum.chunk_every(3)
    |> Enum.flat_map(fn batch ->
      IO.puts("Processing batch of #{length(batch)} posts")

      batch
      |> Task.async_stream(
        fn {assigns, socket} ->
          process_single_post(assigns, socket, changeset, update_comment_changeset, update_post_changeset)
        end,
        max_concurrency: 4,
        timeout: 5000,
        on_timeout: :kill_task
      )
      |> Enum.map(fn
        {:ok, result} -> result
        {:exit, reason} ->
          IO.puts("❌ Task failed with reason: #{inspect(reason)}")
          {assigns, socket} = hd(batch)
          assign(socket, basic_assigns(assigns))
      end)
    end)

    total_end = :erlang.system_time(:millisecond)
    total_duration = total_end - total_start
    IO.puts("🏁 TOTAL UPDATE_MANY completed in #{total_duration}ms")

    result
  end

  def warm_cache_for_recent_posts(recent_post_ids) when is_list(recent_post_ids) do
    Task.start(fn ->
      IO.puts("🔥 Warming cache for #{length(recent_post_ids)} posts")

      recent_post_ids
      |> Enum.take(10)
      |> Enum.each(fn post_id ->
        spawn_background_ipns_refresh(post_id)
        spawn_likes_refresh(post_id)
        Process.sleep(100)
      end)
    end)
  end

  defp await_tasks_with_fallbacks_improved(tasks, post_id) do
    %{
      comments: await_with_fallback(tasks.comments, 2000, [], "comments", post_id),
      ipns_id: await_with_fallback_async(tasks.ipns, 200, nil, "ipns", post_id),
      likes_count: await_with_fallback(tasks.likes, 500, 0, "likes", post_id),
      post_content_cached: await_with_fallback(tasks.content, 1500, "Content loading...", "content", post_id)
    }
  end

  defp fetch_comment_content_direct(comment_id) do
    task = Task.async(fn ->
      try do
        case Core.PostClient.get_comment_content(comment_id) do
          content when is_binary(content) and content != "" ->
            {:ok, content}
          content when is_list(content) ->
            string_content = List.to_string(content)
            if string_content != "" do
              {:ok, string_content}
            else
              {:error, :empty_content}
            end
          _ ->
            {:error, :no_content}
        end
      rescue
        e -> {:error, e}
      end
    end)

    case Task.yield(task, 2000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  defp process_single_post(assigns, socket, changeset, update_comment_changeset, update_post_changeset) do
    try do
      post_start = :erlang.system_time(:millisecond)
      IO.puts("--- Processing post #{assigns.post.id} ---")

      tasks = %{
        comments: Task.async(fn -> get_comments_with_content_optimized(assigns.post.id) end),
        ipns: Task.async(fn -> get_post_ipns_fast(assigns.post.id) end),
        likes: Task.async(fn -> get_likes_count_cached(assigns.post.id) end),
        content: Task.async(fn -> get_post_content_optimized(assigns.post.id) end)
      }

      results = await_tasks_with_fallbacks_improved(tasks, assigns.post.id)

      final_assigns = assigns
      |> Map.merge(%{
        follow_event: follow_event(assigns.current_user.id, assigns.post.author),
        follow_text: follow_text(assigns.current_user.id, assigns.post.author),
        like_icon: like_icon(assigns.current_user.id, assigns.post.id),
        like_event: like_event(assigns.current_user.id, assigns.post.id),
        changeset: changeset,
        update_comment_changeset: update_comment_changeset,
        update_post_changeset: update_post_changeset,
        report_action: false,
        like_action: false,
        is_liked: false
      })
      |> Map.merge(results)

      post_end = :erlang.system_time(:millisecond)
      IO.puts("✅ Post #{assigns.post.id} completed in #{post_end - post_start}ms")

      assign(socket, final_assigns)
    rescue
      e ->
        IO.puts("❌ Error processing post #{assigns.post.id}: #{inspect(e)}")
        assign(socket, basic_assigns(assigns))
    end
  end

  defp get_post_ipns_fast(post_id) do
    cache_key = {:ipns, post_id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, ipns, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp
        if age < 600 do
          IO.puts("📦 IPNS Cache HIT for post #{post_id}")
          ipns
        else
          spawn_background_ipns_refresh(post_id)
          ipns
        end
      [] ->
        spawn_background_ipns_refresh(post_id)
        nil
    end
  end

  defp get_post_content_optimized(post_id) do
    cache_key = {:post_content, post_id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, content, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp
        if age < 600 do
          IO.puts("📦 Content Cache HIT for post #{post_id}")
          content
        else
          spawn_content_refresh(post_id)
          content
        end
      [] ->
        case get_post_content_with_timeout(post_id, 800) do
          {:ok, content} ->
            cache_key = {:post_content, post_id}
            timestamp = :erlang.system_time(:second)
            :ets.insert(@content_cache, {cache_key, content, timestamp})
            content
          _ ->
            "Content loading..."
        end
    end
  end

  defp spawn_content_refresh(post_id) do
    Task.start(fn ->
      case get_post_content_with_timeout(post_id, 2000) do
        {:ok, content} ->
          cache_key = {:post_content, post_id}
          timestamp = :erlang.system_time(:second)
          :ets.insert(@content_cache, {cache_key, content, timestamp})
        _ -> :ok
      end
    end)
  end

  defp get_comments_with_content_optimized(post_id) do
    try do
      IO.puts("🔍 Fetching comments OPTIMIZED for post #{post_id}")

      cache_key = {:comments_processed, post_id}
      case :ets.lookup(@content_cache, cache_key) do
        [{^cache_key, comments, timestamp}] ->
          age = :erlang.system_time(:second) - timestamp
          if age < 180 do
            IO.puts("📦 Comments Cache HIT for post #{post_id}")
            verify_and_fix_comment_content(comments, post_id)
          else
            fetch_and_process_comments_improved(post_id, cache_key)
          end
        [] ->
          fetch_and_process_comments_improved(post_id, cache_key)
      end
    rescue
      e ->
        IO.puts("❌ Error getting comments: #{inspect(e)}")
        []
    end
  end

  defp fetch_and_process_comments_improved(post_id, cache_key) do
    comments_start = :erlang.system_time(:millisecond)

    task = Task.async(fn -> Posts.get_comment_by_post_id(post_id) end)
    comments = case Task.yield(task, 2000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result || []
      nil -> []
    end

    IO.inspect(length(comments), label: "Number of comments found")

    processed_comments = comments
    |> Enum.take(5) #
    |> Enum.map(fn comment ->
      process_single_comment_with_content(comment)
    end)
    |> Enum.filter(&(&1 != nil))

    :ets.insert(@content_cache, {cache_key, processed_comments, :erlang.system_time(:second)})

    comments_end = :erlang.system_time(:millisecond)
    IO.puts("✅ Comments processed in #{comments_end - comments_start}ms")

    processed_comments
  end

  defp process_single_comment_with_content(comment) do
    try do
      IO.puts("📝 Processing comment #{comment.id}")

      content = get_comment_content_with_fallbacks(comment.id)

      comment
      |> Map.put(:content, content)
      |> Map.put(:like_comment_event, like_comment_event_cached(comment.id))
      |> Map.put(:replies, get_comment_replies_optimized(comment.id))
    rescue
      e ->
        IO.puts("❌ Error processing comment #{comment.id}: #{inspect(e)}")
        nil
    end
  end

  defp get_comment_content_with_fallbacks(comment_id) do
    case get_cached_content(:comment, comment_id) do
      content when is_binary(content) and content != "" and
                content not in ["Loading...", "Loading content...", "Content loading...", "Content temporarily unavailable"] ->
        IO.puts("📦 Using cached content for comment #{comment_id}")
        content

      _ ->
        case attempt_synchronous_fetch(comment_id) do
          {:ok, content} when is_binary(content) and content != "" ->
            cache_content(:comment, comment_id, content)
            content
          _ ->
            case fetch_comment_content_final_attempt(comment_id) do
              {:ok, content} ->
                cache_content(:comment, comment_id, content)
                content
              _ ->
                spawn_comment_content_fetch_improved(comment_id)
                "Loading content..."
            end
        end
    end
  end

  defp attempt_synchronous_fetch(comment_id) do
    attempts = [
      fn -> fetch_comment_content_direct(comment_id) end,
      fn -> fetch_comment_content_alternative(comment_id) end,
      fn -> fetch_from_ipfs_alternative(comment_id) end
    ]

    Enum.reduce_while(attempts, {:error, :all_failed}, fn attempt, _acc ->
      case attempt.() do
        {:ok, content} when is_binary(content) and content != "" ->
          {:halt, {:ok, content}}
        _ ->
          Process.sleep(200)
          {:cont, {:error, :failed}}
      end
    end)
  end

  defp fetch_comment_content_final_attempt(comment_id) do
    task = Task.async(fn ->
      try do
        case Core.PostClient.get_comment_content(comment_id) do
          content when is_binary(content) and content != "" ->
            {:ok, content}
          content when is_list(content) ->
            string_content = List.to_string(content)
            if string_content != "" do
              {:ok, string_content}
            else
              case Posts.get_comment_by_id(comment_id) do
                %{content: db_content} when is_binary(db_content) and db_content != "" ->
                  {:ok, db_content}
                _ ->
                  {:error, :no_content}
              end
            end
          _ ->
            {:error, :no_content}
        end
      rescue
        e -> {:error, e}
      end
    end)

    case Task.yield(task, 3000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  defp fetch_comment_content_direct(comment_id) do
    task = Task.async(fn ->
      try do
        case Core.PostClient.get_comment_content(comment_id) do
          content when is_binary(content) and content != "" ->
            {:ok, content}
          content when is_list(content) ->
            string_content = List.to_string(content)
            if string_content != "" do
              {:ok, string_content}
            else
              {:error, :empty_content}
            end
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

  defp fetch_comment_content_alternative(comment_id) do
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

  defp fetch_from_ipfs_alternative(comment_id) do
    try do
      case :postdb.get_comment_content(comment_id) do
        content when is_binary(content) and content != "" ->
          {:ok, content}
        content when is_list(content) ->
          string_content = List.to_string(content)
          if string_content != "" do
            {:ok, string_content}
          else
            {:error, :empty_alternative}
          end
        _ ->
          {:error, :no_alternative_content}
      end
    rescue
      _ -> {:error, :ipfs_alternative_failed}
    end
  end

  defp spawn_comment_content_fetch_improved(comment_id) do
    parent_pid = self()

    Task.start(fn ->
      IO.puts("🔄 Background fetch started for comment #{comment_id}")

      content = case fetch_comment_content_direct(comment_id) do
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

      if content && content != "" do
        cache_content(:comment, comment_id, content)
        send(parent_pid, {:comment_content_updated, comment_id, content})
        IO.puts("✅ Background fetch completed for comment #{comment_id}")
      else
        IO.puts("❌ Background fetch failed for comment #{comment_id}")
      end
    end)
  end

  def handle_info({:comment_content_updated, comment_id, content}, socket) do
    updated_comments = update_comment_content_in_list(socket.assigns[:comments] || [], comment_id, content)

    if updated_comments != socket.assigns[:comments] do
      IO.puts("🔄 Updating comment #{comment_id} with fresh content")
      {:noreply, assign(socket, :comments, updated_comments)}
    else
      {:noreply, socket}
    end
  end

  defp update_comment_content_in_list(comments, comment_id, content) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        Map.put(comment, :content, content)
      else
        comment
      end
    end)
  end

  defp verify_and_fix_comment_content(comments, post_id) do
    needs_refresh = Enum.any?(comments, fn comment ->
      comment.content in ["Loading content...", "Loading...", "Content loading...", nil, ""]
    end)

    if needs_refresh do
      IO.puts("🔧 Some comments need content refresh, fetching fresh data")
      fetch_and_process_comments_improved(post_id, {:comments_processed, post_id})
    else
      comments
    end
  end

  defp get_comment_replies_optimized(comment_id) do
    try do
      IO.puts("🔄 Fetching replies for comment #{comment_id}")

      task = Task.async(fn ->
        :postdb.get_comment_replies(comment_id |> to_charlist)
      end)

      case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
        {:ok, replies} ->
          replies
          |> Enum.take(3)
          |> Enum.map(fn reply ->
            case reply |> Mazaryn.Schema.Reply.erl_changeset() |> Mazaryn.Schema.Reply.build() do
              {:ok, built_reply} ->
                content = get_reply_content_with_fallbacks(built_reply.id)
                Map.put(built_reply, :content, content)
              {:error, _} -> nil
            end
          end)
          |> Enum.filter(&(&1 != nil))

        nil -> []
      end
    rescue
      e ->
        IO.puts("❌ Error getting replies: #{inspect(e)}")
        []
    end
  end

  defp get_reply_content_with_fallbacks(reply_id) do
    case get_cached_content(:reply, reply_id) do
      content when is_binary(content) and content != "" and content != "Loading..." ->
        IO.puts("📦 Using cached content for reply #{reply_id}")
        content

      _ ->
        case fetch_reply_content_direct(reply_id) do
          {:ok, content} when is_binary(content) and content != "" ->
            cache_content(:reply, reply_id, content)
            content

          _ ->
            case fetch_reply_content_alternative(reply_id) do
              {:ok, content} ->
                cache_content(:reply, reply_id, content)
                content

              _ ->
                spawn_reply_content_fetch_improved(reply_id)
                "Loading reply..."
            end
        end
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
            if string_content != "" do
              {:ok, string_content}
            else
              {:error, :empty_content}
            end
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
      case :postdb.get_reply_by_id(reply_id) do
        reply_data when reply_data != nil ->
          case reply_data |> Mazaryn.Schema.Reply.erl_changeset() |> Mazaryn.Schema.Reply.build() do
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
        if string_content != "" do
          {:ok, string_content}
        else
          {:error, :empty_alternative}
        end
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
      IO.puts("🔄 Background fetch started for reply #{reply_id}")

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

      if content && content != "" do
        cache_content(:reply, reply_id, content)
        send(parent_pid, {:reply_content_updated, reply_id, content})
        IO.puts("✅ Background fetch completed for reply #{reply_id}")
      else
        IO.puts("❌ Background fetch failed for reply #{reply_id}")
      end
    end)
  end

  def handle_info({:reply_content_updated, reply_id, content}, socket) do
    updated_comments = update_reply_content_in_comments(socket.assigns[:comments] || [], reply_id, content)

    if updated_comments != socket.assigns[:comments] do
      IO.puts("🔄 Updating reply #{reply_id} with fresh content")
      {:noreply, assign(socket, :comments, updated_comments)}
    else
      {:noreply, socket}
    end
  end

  defp update_reply_content_in_comments(comments, reply_id, content) do
    Enum.map(comments, fn comment ->
      updated_replies = Enum.map(comment.replies || [], fn reply ->
        if to_string(reply.id) == to_string(reply_id) do
          Map.put(reply, :content, content)
        else
          reply
        end
      end)
      Map.put(comment, :replies, updated_replies)
    end)
  end

  def handle_event("reply_comment_content", %{"comment" => comment_params} = _params, socket) do
    %{"comment_id" => comment_id, "content" => content} = comment_params
    user_id = socket.assigns.current_user.id

    IO.puts("🚀 Creating reply for comment #{comment_id} with content: #{content}")

    temp_reply = create_temp_reply(user_id, comment_id, content)

    updated_comments = add_reply_to_comment(socket.assigns.comments, comment_id, temp_reply)

    Task.start(fn ->
      case PostClient.reply_comment(user_id, to_charlist(comment_id), content) do
        {:ok, reply} ->
          IO.puts("✅ Reply saved successfully: #{inspect(reply.id)}")
          if reply && reply.id do
            cache_content(:reply, reply.id, content)
          end
          send(self(), {:reply_saved, comment_id, reply, temp_reply.id})

        error ->
          IO.puts("❌ Error saving reply: #{inspect(error)}")
          send(self(), {:reply_failed, comment_id, temp_reply.id})
      end
    end)

    {:noreply,
     socket
     |> assign(:comments, updated_comments)
     |> assign(:reply_comment, false)
     |> assign(:replying_to_comment_id, nil)}
  end

  defp replace_temp_reply_with_real(comments, comment_id, temp_id, real_reply) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        updated_replies = Enum.map(comment.replies || [], fn reply ->
          if reply.id == temp_id do
            preserved_content = reply.content
            cache_content(:reply, real_reply.id, preserved_content)

            %{
              id: real_reply.id,
              content: preserved_content,
              user_id: real_reply.user_id,
              comment_id: comment_id,
              inserted_at: real_reply.inserted_at || reply.inserted_at,
              updated_at: real_reply.updated_at || reply.updated_at,
              is_temp: false
            }
          else
            reply
          end
        end)

        Map.put(comment, :replies, updated_replies)
      else
        comment
      end
    end)
  end

  defp create_temp_reply(user_id, comment_id, content) do
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

  def handle_event("save-comment", %{"comment" => comment_params}, socket) do
    IO.puts("💾 SAVE-COMMENT EVENT TRIGGERED")
    IO.inspect(comment_params, label: "Comment params received")

    comment_params = Map.new(comment_params, fn {k, v} -> {String.to_atom(k), v} end)

    case comment_params do
      %{post_id: post_id, author: author, content: content} ->
        changeset = %Comment{}
          |> Comment.changeset(%{
            post_id: post_id,
            author: author,
            content: content
          })

        case Posts.create_comment(changeset) do
          {:ok, comment} ->
            cache_content(:comment, comment.id, content)

            :ets.delete(@content_cache, {:comments_processed, post_id})

            comments = get_comments_with_content_optimized(post_id)

            post = rebuild_post(post_id)

            {:noreply,
             socket
             |> assign(:post, post)
             |> assign(:comments, comments)
             |> assign(:changeset, Comment.changeset(%Comment{}))}

          {:error, changeset} ->
            IO.puts("❌ Error creating comment: #{inspect(changeset.errors)}")
            {:noreply, assign(socket, :changeset, changeset)}
        end
      _ ->
        IO.puts("❌ Missing required comment parameters")
        {:noreply, socket |> put_flash(:error, "Missing required fields")}
    end
  end

  defp get_cached_content(type, id) do
    cache_key = {type, id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, content, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp
        if age < 600 do
          case content do
            content when is_binary(content) and content != "" and
                         content not in ["Loading...", "Loading content...", "Content loading...",
                                       "Content temporarily unavailable", "Content loading failed"] ->
              IO.puts("📦 Valid cache HIT for #{type}:#{id} (age: #{age}s)")
              content
            _ ->
              IO.puts("⚠️ Invalid cached content for #{type}:#{id}, removing from cache")
              :ets.delete(@content_cache, cache_key)
              nil
          end
        else
          IO.puts("⏰ Cache EXPIRED for #{type}:#{id} (age: #{age}s)")
          :ets.delete(@content_cache, cache_key)
          nil
        end
      [] ->
        IO.puts("❌ Cache MISS for #{type}:#{id}")
        nil
    end
  end

  defp get_likes_count_cached(post_id) do
    cache_key = {:likes_count, post_id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, count, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp
        if age < 300 do
          IO.puts("📦 Likes Cache HIT for post #{post_id}")
          count
        else
          spawn_likes_refresh(post_id)
          count
        end
      [] ->
        case get_likes_count_with_timeout(post_id, 300) do
          count when is_integer(count) ->
            :ets.insert(@content_cache, {cache_key, count, :erlang.system_time(:second)})
            count
          _ -> 0
        end
    end
  end

  defp spawn_likes_refresh(post_id) do
    Task.start(fn ->
      try do
        count = get_likes_count(post_id)
        cache_key = {:likes_count, post_id}
        :ets.insert(@content_cache, {cache_key, count, :erlang.system_time(:second)})
      rescue
        _ -> :ok
      end
    end)
  end

  def handle_event("save-comment", %{"comment" => comment_params}, socket) do
    IO.puts("💾 SAVE-COMMENT EVENT TRIGGERED")
    IO.inspect(comment_params, label: "Comment params received")

    comment_params = Map.new(comment_params, fn {k, v} -> {String.to_atom(k), v} end)

    case comment_params do
      %{post_id: post_id, author: author, content: content} ->
        changeset = %Comment{}
          |> Comment.changeset(%{
            post_id: post_id,
            author: author,
            content: content
          })

        case Posts.create_comment(changeset) do
          {:ok, comment} ->
            cache_content(:comment, comment.id, content)

            :ets.delete(@content_cache, {:comments_processed, post_id})

            post = rebuild_post(post_id)
            comments = get_comments_with_content_optimized(post.id)

            {:noreply,
             socket
             |> assign(:post, post)
             |> assign(:comments, comments)
             |> assign(:changeset, Comment.changeset(%Comment{}))}

          {:error, changeset} ->
            IO.puts("❌ Error creating comment: #{inspect(changeset.errors)}")
            {:noreply, assign(socket, :changeset, changeset)}
        end
      _ ->
        IO.puts("❌ Missing required comment parameters")
        {:noreply, socket |> put_flash(:error, "Missing required fields")}
    end
  end

  defp get_post_ipns_with_timeout(post_id, timeout \\ 800) do
    case post_id do
      nil -> nil
      "" -> nil
      [] -> nil
      _ ->
        failure_key = {:ipns_failures, post_id}
        case :ets.lookup(@content_cache, failure_key) do
          [{_, failure_count, last_failure}] ->
            age = :erlang.system_time(:second) - last_failure
            if failure_count > 3 and age < 60 do
              IO.puts("🚫 Circuit breaker: skipping IPNS for #{post_id}")
              nil
            else
              fetch_ipns_with_task(post_id, timeout)
            end
          [] ->
            fetch_ipns_with_task(post_id, timeout)
        end
    end
  end

  defp fetch_ipns_with_task(post_id, timeout) do
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
    case :ets.lookup(@content_cache, failure_key) do
      [{_, count, _}] ->
        :ets.insert(@content_cache, {failure_key, count + 1, :erlang.system_time(:second)})
      [] ->
        :ets.insert(@content_cache, {failure_key, 1, :erlang.system_time(:second)})
    end
  end

  def handle_event("save-comment", %{"comment" => comment_params}, socket) do
    comment_params = Map.new(comment_params, fn {k, v} -> {String.to_atom(k), v} end)

    case comment_params do
      %{post_id: post_id, author: author, content: content} ->
        changeset = %Comment{}
          |> Comment.changeset(%{
            post_id: post_id,
            author: author,
            content: content
          })

        case Posts.create_comment(changeset) do
          {:ok, comment} ->
            optimistic_comment = %{
              id: comment.id,
              content: content,
              author: author,
              inserted_at: DateTime.utc_now(),
              like_comment_event: "like-comment",
              replies: []
            }

            current_comments = socket.assigns[:comments] || []
            updated_comments = [optimistic_comment | current_comments]

            cache_content(:comment, comment.id, content)

            {:noreply,
             socket
             |> assign(:comments, updated_comments)
             |> assign(:changeset, Comment.changeset(%Comment{}))}

          {:error, changeset} ->
            {:noreply, assign(socket, :changeset, changeset)}
        end
      _ ->
        {:noreply, socket |> put_flash(:error, "Missing required fields")}
    end
  end

  def handle_info({:background_update, post_id, field, value}, socket) do
    if socket.assigns[:post] && to_string(socket.assigns.post.id) == to_string(post_id) do
      {:noreply, assign(socket, field, value)}
    else
      {:noreply, socket}
    end
  end

  defp get_likes_count_with_timeout(post_id, timeout \\ 300) do
    cache_key = {:likes_count, post_id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, count, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp
        if age < 60 do
          count
        else
          spawn(fn -> refresh_likes_count_background(post_id) end)
          count
        end
      [] ->
        task = Task.async(fn -> get_likes_count(post_id) end)
        case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
          {:ok, count} ->
            :ets.insert(@content_cache, {cache_key, count, :erlang.system_time(:second)})
            count
          nil -> 0
        end
    end
  end

  defp refresh_likes_count_background(post_id) do
    try do
      count = get_likes_count(post_id)
      cache_key = {:likes_count, post_id}
      :ets.insert(@content_cache, {cache_key, count, :erlang.system_time(:second)})
    rescue
      _ -> :ok
    end
  end

  defp get_post_ipns_async(post_id) do
    cache_key = {:ipns, post_id}

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, ipns, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp
        if age < 300 do
          IO.puts("📦 IPNS Cache HIT for post #{post_id}")
          spawn_background_ipns_refresh(post_id)
          ipns
        else
          :ets.delete(@content_cache, cache_key)
          fetch_ipns_async_with_fallback(post_id)
        end
      [] ->
        fetch_ipns_async_with_fallback(post_id)
    end
  end

  defp fetch_ipns_async_with_fallback(post_id) do
    spawn_background_ipns_refresh(post_id)

    get_cached_ipns_or_nil(post_id)
  end

  defp spawn_background_ipns_refresh(post_id) do
    parent_pid = self()

    Task.start(fn ->
      case get_post_ipns_with_timeout(post_id, 1500) do
        ipns when not is_nil(ipns) ->
          cache_key = {:ipns, post_id}
          timestamp = :erlang.system_time(:second)
          :ets.insert(@content_cache, {cache_key, ipns, timestamp})

          send(parent_pid, {:ipns_updated, post_id, ipns})
          IO.puts("✅ Background IPNS refresh completed for #{post_id}")

        nil ->
          IO.puts("⚠️ Background IPNS fetch failed for #{post_id}")
      end
    end)
  end

  def handle_info({:ipns_updated, post_id, ipns}, socket) do
    if socket.assigns[:post] && to_string(socket.assigns.post.id) == to_string(post_id) do
      IO.puts("🔄 Updating IPNS for displayed post #{post_id}")
      {:noreply, assign(socket, :ipns_id, ipns)}
    else
      {:noreply, socket}
    end
  end

  def warm_ipns_cache_async(recent_post_ids) when is_list(recent_post_ids) do
    Task.start(fn ->
      IO.puts("🔥 Starting async IPNS cache warming for #{length(recent_post_ids)} posts")

      recent_post_ids
      |> Enum.chunk_every(3)
      |> Enum.each(fn batch ->
        tasks = Enum.map(batch, fn post_id ->
          Task.async(fn ->
            spawn_background_ipns_refresh(post_id)
            :timer.sleep(100)
          end)
        end)

        Task.yield_many(tasks, 50)

        :timer.sleep(200)
      end)

      IO.puts("✅ IPNS cache warming initiated for all posts")
    end)
  end

    defp await_tasks_with_fallbacks_improved(tasks, post_id) do
      %{
        comments: await_with_fallback(tasks.comments, 3000, [], "comments", post_id),
        ipns_id: await_with_fallback_async(tasks.ipns, 150, nil, "ipns", post_id),
        likes_count: await_with_fallback(tasks.likes, 300, 0, "likes", post_id),
        post_content_cached: await_with_fallback(tasks.content, 1000, "Content loading...", "content", post_id)
      }
    end

    defp await_with_fallback_async(task, timeout, fallback, task_name, post_id) do
      case Task.yield(task, timeout) do
        {:ok, result} ->
          IO.puts("✅ #{task_name} task completed quickly for post #{post_id}")
          result
        nil ->
          IO.puts("⚡ #{task_name} continuing in background for post #{post_id} - using fallback")
          fallback
      end
    end

  defp get_cached_ipns_or_nil(post_id) do
    cache_key = {:ipns, post_id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, ipns, _timestamp}] -> ipns
      [] -> nil
    end
  end

  def handle_info({:ipns_fetched, post_id, ipns}, socket) do
    if socket.assigns[:post] && to_string(socket.assigns.post.id) == to_string(post_id) do
      IO.puts("🔄 Updating IPNS for displayed post #{post_id}")
      {:noreply, assign(socket, :ipns_id, ipns)}
    else

      {:noreply, socket}
    end
  end

  defp await_tasks_with_fallbacks_improved(tasks, post_id) do
    %{
      comments: await_with_fallback(tasks.comments, 3000, [], "comments", post_id),
      ipns_id: await_with_fallback(tasks.ipns, 300, nil, "ipns", post_id),
      likes_count: await_with_fallback(tasks.likes, 300, 0, "likes", post_id),
      post_content_cached: await_with_fallback(tasks.content, 1000, "Content loading...", "content", post_id)
    }
  end

  def warm_ipns_cache(recent_post_ids) do
    Task.start(fn ->
      IO.puts("🔥 Warming IPNS cache for #{length(recent_post_ids)} posts")

      recent_post_ids
      |> Enum.chunk_every(5)
      |> Enum.each(fn batch ->
        batch
        |> Enum.map(fn post_id ->
          Task.async(fn -> get_post_ipns_async(post_id) end)
        end)
        |> Task.await_many(2000)

        Process.sleep(500)
      end)

      IO.puts("✅ IPNS cache warming completed")
    end)
  end

  defp await_tasks_with_fallbacks_improved(tasks, post_id) do
    %{
      comments: await_with_fallback(tasks.comments, 3000, [], "comments", post_id),
      ipns_id: await_with_fallback(tasks.ipns, 500, nil, "ipns", post_id),
      likes_count: await_with_fallback(tasks.likes, 300, 0, "likes", post_id),
      post_content_cached: await_with_fallback(tasks.content, 1000, "Content loading...", "content", post_id)
    }
  end

  defp await_with_fallback(task, timeout, fallback, task_name, post_id) do
    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} ->
        IO.puts("✅ #{task_name} task completed for post #{post_id}")
        result
      nil ->
        IO.puts("⚠️ #{task_name} task timeout (#{timeout}ms) for post #{post_id} - using fallback")
        fallback
    end
  end

  defp basic_assigns(assigns) do
    %{
      follow_event: follow_event(assigns.current_user.id, assigns.post.author),
      follow_text: follow_text(assigns.current_user.id, assigns.post.author),
      like_icon: like_icon(assigns.current_user.id, assigns.post.id),
      like_event: like_event(assigns.current_user.id, assigns.post.id),
      changeset: Comment.changeset(%Comment{}),
      update_comment_changeset: Comment.changeset(%Comment{}),
      update_post_changeset: Post.changeset(%Post{}),
      comments: [],
      report_action: false,
      like_action: false,
      is_liked: false,
      ipns_id: nil,
      likes_count: 0,
      post_content_cached: "Content loading..."
    }
  end

  defp get_comment_content_reliable(comment_id) do
    case get_cached_content(:comment, comment_id) do
      nil ->
        fetch_comment_content_with_retries(comment_id, 3)
      content ->
        content
    end
  end

  defp fetch_comment_content_with_retries(comment_id, retries_left) when retries_left > 0 do
    case fetch_content_with_timeout(:comment, comment_id, 2000) do
      {:ok, content} when is_binary(content) and content != "" ->
        cache_content(:comment, comment_id, content)
      {:ok, content} when is_list(content) ->
        string_content = List.to_string(content)
        if string_content != "" do
          cache_content(:comment, comment_id, string_content)
        else
          retry_or_fallback(comment_id, retries_left - 1)
        end
      {:error, :content_processing} ->
        Process.send_after(self(), {:refresh_comment_content, comment_id}, 2000)
        cache_content(:comment, comment_id, "Content is being processed...")
      {:timeout, _} ->
        IO.puts("⚠️ Comment content fetch timeout for #{comment_id}, retrying...")
        retry_or_fallback(comment_id, retries_left - 1)
      {:error, reason} ->
        IO.puts("❌ Error fetching comment content: #{inspect(reason)}")
        retry_or_fallback(comment_id, retries_left - 1)
    end
  end

  defp fetch_comment_content_with_retries(comment_id, 0) do
    IO.puts("❌ All retries exhausted for comment #{comment_id}")
    cache_content(:comment, comment_id, "Content temporarily unavailable")
  end

  defp retry_or_fallback(comment_id, retries_left) when retries_left > 0 do
    Process.sleep(500)
    fetch_comment_content_with_retries(comment_id, retries_left)
  end

  defp retry_or_fallback(comment_id, 0) do
    cache_content(:comment, comment_id, "Content loading failed")
  end


  defp get_post_content_with_timeout(post_id, timeout \\ 1500) do
    task = Task.async(fn ->
      try do
        case Core.PostClient.get_post_content_by_id(post_id) do
          content when is_binary(content) and content != "" ->
            {:ok, content}
          content when is_list(content) ->
            case List.to_string(content) do
              "" -> {:ok, "No content available"}
              str -> {:ok, str}
            end
          _ ->
            {:ok, "No content available"}
        end
      catch
        type, reason ->
          IO.puts("❌ Error fetching post content: #{inspect({type, reason})}")
          {:error, {type, reason}}
      end
    end)

    case Task.yield(task, timeout) || Task.shutdown(task) do
      {:ok, result} -> result
      nil -> {:timeout, "Timeout after #{timeout}ms"}
    end
  end

  defp get_fallback_content(post_id) do
    try do
      case PostClient.get_by_id(post_id) do
        post_data when is_tuple(post_data) ->
          case Mazaryn.Schema.Post.erl_changeset(post_data) |> Mazaryn.Schema.Post.build() do
            {:ok, post} ->
              case post.content do
                content when is_binary(content) and content != "" -> content
                content when is_list(content) -> List.to_string(content)
                _ -> "Content loading..."
              end
            _ -> "Content loading..."
          end
        _ -> "Content loading..."
      end
    catch
      _, _ -> "Content loading..."
    end
  end

  defp get_comments_with_content(post_id) do
    try do
      IO.puts("🔍 Fetching comments for post #{post_id}")
      comments_start = :erlang.system_time(:millisecond)

      comments = log_performance("Posts.get_comment_by_post_id", fn ->
        Posts.get_comment_by_post_id(post_id)
      end)

      IO.inspect(length(comments), label: "Number of comments found")

      processed_comments = comments
      |> Enum.take(5)
      |> Enum.with_index()
      |> Enum.map(fn {comment, index} ->
        comment_start = :erlang.system_time(:millisecond)
        IO.puts("Processing comment #{index + 1}/#{min(length(comments), 5)} (ID: #{comment.id})")

        content = log_performance("get comment content for #{comment.id}", fn ->
          get_cached_content(:comment, comment.id) ||
          fetch_comment_content_from_ipfs(comment.id)
        end)

        like_event = log_performance("like_comment_event_cached for #{comment.id}", fn ->
          like_comment_event_cached(comment.id)
        end)

        result = comment
        |> Map.put(:content, content)
        |> Map.put(:like_comment_event, like_event)
        |> then(fn c ->
          log_performance("add_replies_optimized for comment #{comment.id}", fn ->
            add_replies_optimized(c)
          end)
        end)

        comment_end = :erlang.system_time(:millisecond)
        IO.puts("✅ Comment #{comment.id} processed in #{comment_end - comment_start}ms")

        result
      end)

      comments_end = :erlang.system_time(:millisecond)
      IO.puts("✅ All comments processed in #{comments_end - comments_start}ms")

      processed_comments
    rescue
      e ->
        IO.puts("❌ Error getting comments: #{inspect(e)}")
        IO.puts("📍 Error stacktrace: #{inspect(__STACKTRACE__)}")
        []
    end
  end

  defp add_replies_optimized(comment) do
    try do
      IO.puts("🔄 Processing replies for comment #{comment.id}")
      replies_start = :erlang.system_time(:millisecond)

      replies = log_performance("postdb.get_comment_replies for #{comment.id}", fn ->
        :postdb.get_comment_replies(comment.id |> to_charlist)
      end)

      IO.inspect(length(replies), label: "Number of replies found")

      list_replies =
        replies
        |> Enum.take(5)
        |> Enum.with_index()
        |> Enum.map(fn {reply, index} ->
          IO.puts("Processing reply #{index + 1}/#{min(length(replies), 5)}")

          case reply |> Mazaryn.Schema.Reply.erl_changeset() |> Mazaryn.Schema.Reply.build() do
            {:ok, built_reply} ->
              content = log_performance("get reply content for #{built_reply.id}", fn ->
                get_cached_content(:reply, built_reply.id) ||
                fetch_reply_content_from_ipfs(built_reply.id)
              end)
              Map.put(built_reply, :content, content)
            {:error, error} ->
              IO.puts("❌ Error building reply: #{inspect(error)}")
              nil
          end
        end)
        |> Enum.filter(&(&1 != nil))

      replies_end = :erlang.system_time(:millisecond)
      IO.puts("✅ Replies processed in #{replies_end - replies_start}ms")

      Map.put(comment, :replies, list_replies)
    rescue
      e ->
        IO.puts("❌ Error processing replies: #{inspect(e)}")
        IO.puts("📍 Error stacktrace: #{inspect(__STACKTRACE__)}")
        Map.put(comment, :replies, [])
    end
  end

  defp get_cached_content(type, id) do
    cache_key = {type, id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, content, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp
        if age < 300 do
          IO.puts("📦 Cache HIT for #{type}:#{id} (age: #{age}s)")
          content
        else
          IO.puts("⏰ Cache EXPIRED for #{type}:#{id} (age: #{age}s)")
          :ets.delete(@content_cache, cache_key)
          nil
        end
      [] ->
        IO.puts("❌ Cache MISS for #{type}:#{id}")
        nil
    end
  end

  defp cache_content(type, id, content) do
    if content not in ["Loading...", "Loading content...", "Content loading...",
                    "Content temporarily unavailable", "Content loading failed"] do
      cache_key = {type, id}
      timestamp = :erlang.system_time(:second)
      :ets.insert(@content_cache, {cache_key, content, timestamp})
      IO.puts("💾 Cached content for #{type}:#{id}")
    else
      IO.puts("⚠️ Skipping cache for loading state: #{content}")
    end
    content
  end


  defp like_comment_event_cached(comment_id) do
    "like-comment"
  end

  defp get_likes_count(post_id) do
    try do
      likes_start = :erlang.system_time(:millisecond)

      count = post_id
      |> to_charlist
      |> PostClient.get_likes()
      |> Enum.count()

      likes_end = :erlang.system_time(:millisecond)
      duration = likes_end - likes_start
      IO.puts("📊 PostClient.get_likes() for #{post_id} took #{duration}ms, found #{count} likes")

      count
    rescue
      e ->
        IO.puts("❌ Error getting likes count: #{inspect(e)}")
        0
    end
  end

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> assign(:editing_post, false)
     |> assign(:editing_comment, false)
     |> assign(:editing_comment_id, nil)
     |> assign(:reply_comment, false)
     |> assign(:replying_to_comment_id, nil)
     |> assign(:ipns_id, nil)
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  def handle_event("edit-comment", %{"comment-id" => comment_id}, socket) do
    IO.puts("📝 Starting to edit comment #{comment_id}")

    {:noreply,
     socket
     |> assign(:editing_comment, true)
     |> assign(:editing_comment_id, comment_id)}
  end

  def handle_event("cancel-comment-edit", _params, socket) do
    IO.puts("❌ Cancelling comment edit")

    {:noreply,
     socket
     |> assign(:editing_comment, false)
     |> assign(:editing_comment_id, nil)}
  end

  defp get_post_ipns(post_id) do
    case post_id do
      nil -> nil
      "" -> nil
      [] -> nil
      _ ->
        try do
          ipns_start = :erlang.system_time(:millisecond)
          post_id_charlist = if is_binary(post_id), do: to_charlist(post_id), else: post_id

          result = Core.PostClient.get_ipns_from_post(post_id_charlist)

          ipns_end = :erlang.system_time(:millisecond)
          duration = ipns_end - ipns_start
          IO.puts("🔗 Core.PostClient.get_ipns_from_post() for #{post_id} took #{duration}ms")

          if duration > 500 do
            IO.puts("🚨 VERY SLOW IPNS CALL: #{duration}ms for post #{post_id}")
          end

          result
        catch
          type, reason ->
            IO.puts("❌ Error in get_post_ipns: #{inspect({type, reason})}")
            nil
        rescue
          e ->
            IO.puts("❌ Rescue in get_post_ipns: #{inspect(e)}")
            nil
        end
    end
  end

  defp fetch_comment_content_from_ipfs(comment_id) do
    cached_content = get_cached_content(:comment, comment_id)
    if cached_content do
      cached_content
    else
      case fetch_content_with_timeout(:comment, comment_id, 3000) do
        {:ok, content} ->
          cache_content(:comment, comment_id, content)
        {:timeout, _} ->
          IO.puts("⚠️ Comment content fetch timeout for #{comment_id}")
          case check_content_status(:comment, comment_id) do
            :processing ->
              cache_content(:comment, comment_id, "Content is being processed...")
            :ready ->
              case fetch_content_with_timeout(:comment, comment_id, 5000) do
                {:ok, content} -> cache_content(:comment, comment_id, content)
                _ -> cache_content(:comment, comment_id, "Content loading...")
              end
            _ ->
              cache_content(:comment, comment_id, "Content loading...")
          end
        {:error, :content_processing} ->
          cache_content(:comment, comment_id, "Content is being processed...")
        {:error, :content_cache_missing} ->
          cache_content(:comment, comment_id, "Content temporarily unavailable")
        {:error, _} ->
          cache_content(:comment, comment_id, "Content unavailable")
      end
    end
  end

  defp fetch_reply_content_from_ipfs(reply_id) do
    cached_content = get_cached_content(:reply, reply_id)
    if cached_content do
      cached_content
    else
      case fetch_content_with_timeout(:reply, reply_id, 3000) do
        {:ok, content} ->
          cache_content(:reply, reply_id, content)
        {:timeout, _} ->
          IO.puts("⚠️ Reply content fetch timeout for #{reply_id}")
          case check_content_status(:reply, reply_id) do
            :processing ->
              cache_content(:reply, reply_id, "Content is being processed...")
            :ready ->
              case fetch_content_with_timeout(:reply, reply_id, 5000) do
                {:ok, content} -> cache_content(:reply, reply_id, content)
                _ -> cache_content(:reply, reply_id, "Content loading...")
              end
            _ ->
              cache_content(:reply, reply_id, "Content loading...")
          end
        {:error, :content_processing} ->
          cache_content(:reply, reply_id, "Content is being processed...")
        {:error, :content_cache_missing} ->
          cache_content(:reply, reply_id, "Content temporarily unavailable")
        {:error, _} ->
          cache_content(:reply, reply_id, "Content unavailable")
      end
    end
  end

  defp check_content_status(type, id) do
    try do
      case type do
        :comment ->
          case Core.PostClient.get_comment_status(id) do
            :processing -> :processing
            :ready -> :ready
            _ -> :unknown
          end
        :reply ->
          case Core.PostClient.get_reply_status(id) do
            :processing -> :processing
            :ready -> :ready
            _ -> :unknown
          end
      end
    catch
      _, _ -> :unknown
    end
  end

  defp fetch_content_with_timeout(type, id, timeout) do
    task = Task.async(fn ->
      try do
        ipfs_start = :erlang.system_time(:millisecond)

        content = case type do
          :comment -> Core.PostClient.get_comment_content(id)
          :reply -> Core.PostClient.get_reply_content(id)
        end

        result = case content do
          {:error, :content_processing} ->
            {:error, :content_processing}
          {:error, :content_cache_missing} ->
            {:error, :content_cache_missing}
          content when is_binary(content) and content != "" ->
            {:ok, content}
          content when is_list(content) ->
            case List.to_string(content) do
              "" -> {:error, :empty_content}
              str -> {:ok, str}
            end
          _ ->
            {:error, :invalid_content}
        end

        ipfs_end = :erlang.system_time(:millisecond)
        duration = ipfs_end - ipfs_start
        IO.puts("📡 IPFS #{type} content fetch for #{id} took #{duration}ms")

        if duration > 2000 do
          IO.puts("🚨 SLOW IPFS #{String.upcase(to_string(type))} FETCH: #{duration}ms for #{type} #{id}")
        end

        result
      catch
        _, e ->
          IO.puts("❌ Error fetching #{type} content from IPFS: #{inspect(e)}")
          {:error, e}
      end
    end)

    case Task.yield(task, timeout) || Task.shutdown(task) do
      {:ok, result} -> result
      nil -> {:timeout, "Timeout after #{timeout}ms"}
    end
  end

  defp clear_content_cache(type, id) do
    cache_key = {type, id}
    :ets.delete(@content_cache, cache_key)
  end

  defp ensure_cache_table do
    unless :ets.whereis(@content_cache) != :undefined do
      :ets.new(@content_cache, [:set, :public, :named_table, {:read_concurrency, true}])
    end
  end

  @impl true
  def handle_event("delete-post", %{"post-id" => post_id} = _params, socket) do
    post_id = post_id |> to_charlist
    PostClient.delete_post(post_id)

    clear_content_cache(:post, post_id)
    clear_content_cache(:ipns, post_id)
    clear_content_cache(:likes_count, post_id)
    clear_content_cache(:post_content, post_id)

    send(self(), :reload_posts)
    {:noreply, socket}
  end

  def handle_event("delete-comment", %{"comment-id" => comment_id, "post-id" => post_id} = _params, socket) do
    post_id = post_id |> to_charlist
    comment_id = comment_id |> to_charlist

    :postdb.delete_comment_from_mnesia(comment_id)

    clear_content_cache(:comment, comment_id)
    clear_content_cache(:comment_status, comment_id)

    post = rebuild_post(post_id)
    comments = get_comments_with_content(post.id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments)}
  end

  def handle_event("validate-update-comment", %{"comment" => comment_params} = _params, socket) do
    changeset =
      %Comment{}
      |> Comment.update_changeset(comment_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :update_comment_changeset, changeset)}
  end

  def handle_info(:refresh_processing_content, socket) do
    comments = get_comments_with_content(socket.assigns.post.id)

    processing_content = Enum.any?(comments, fn comment ->
      comment.content in ["Content is being processed...", "Content loading..."]
    end)

    if processing_content do
      Process.send_after(self(), :refresh_processing_content, 3000)
    end

    {:noreply, assign(socket, :comments, comments)}
  end

  def handle_event("edit_post", %{"post-id" => post_id}, socket) do
    post_id = post_id |> to_charlist
    post = PostClient.get_post_content_by_id(post_id)
    {:noreply, socket |> assign(:editing_post, post) |> assign(:edit_post_id, post_id)}
  end

  def handle_event("update-post", %{"post" => post_params}, socket) do
    post_id = socket.assigns.edit_post_id
    post_id = if is_binary(post_id), do: :erlang.binary_to_list(post_id), else: post_id

    changeset = %Post{} |> Post.changeset(post_params)

    case changeset.valid? do
      true ->
        new_content = post_params["content"]

        case PostClient.update_post(post_id, new_content) do
          :ok ->
            clear_content_cache(:post, post_id)
            clear_content_cache(:ipns, post_id)

            try do
              retrieved_post = rebuild_post(post_id)
              ipns_id = get_post_ipns(post_id)
              send(self(), :reload_posts)

              {:noreply,
               socket
               |> assign(:editing_post, false)
               |> assign(:edit_post_id, nil)
               |> assign(:ipns_id, ipns_id)}
            catch
              _, e ->
                IO.puts("Error retrieving post after update: #{inspect(e)}")
                {:noreply,
                 socket
                 |> assign(:editing_post, false)
                 |> assign(:edit_post_id, nil)}
            end

          error ->
            IO.puts("Error updating post: #{inspect(error)}")
            {:noreply,
             socket
             |> put_flash(:error, "Failed to update post: #{inspect(error)}")}
        end

      false ->
        {:noreply, assign(socket, :update_post_changeset, %{changeset | action: :validate})}
    end
  end

  def handle_event("cancel-edit", _params, socket) do
    {:noreply, assign(socket, :editing_post, false)}
  end

  def handle_event("update-comment", %{"comment" => comment_params} = _params, socket) do
    IO.puts("📝 UPDATE-COMMENT EVENT TRIGGERED")

    comment_id = comment_params["id"]
    new_content = comment_params["content"]

    comment =
      %Comment{}
      |> Comment.update_changeset(comment_params)
      |> Posts.update_comment()

    case comment do
      {:ok, updated_comment} ->
        if comment_id && new_content do
          cache_content(:comment, comment_id, new_content)
          IO.puts("💾 Immediately cached new content for comment #{comment_id}")
        end

        updated_comments = update_comment_content_optimistically(socket.assigns.comments, comment_id, new_content)
        post = updated_comment.post_id |> rebuild_post()

        {:noreply,
         socket
         |> assign(:post, post)
         |> assign(:comments, updated_comments)
         |> assign(:update_comment_changeset, Comment.changeset(%Comment{}))
         |> assign(:editing_comment, false)
         |> assign(:editing_comment_id, nil)}

      {:error, changeset} ->
        IO.puts("❌ Error updating comment: #{inspect(changeset.errors)}")
        {:noreply, assign(socket, :update_comment_changeset, changeset)}
    end
  end

  defp update_comment_content_optimistically(comments, comment_id, new_content) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        IO.puts("✅ Optimistically updating comment #{comment_id} with new content")
        Map.put(comment, :content, new_content)
      else
        comment
      end
    end)
  end

  def handle_event("validate-comment", %{"comment" => comment_params} = _params, socket) do
    changeset =
      %Comment{}
      |> Comment.changeset(comment_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  def handle_event("save-comment", %{"comment" => comment_params}, socket) do
    IO.puts("💾 SAVE-COMMENT EVENT TRIGGERED")
    IO.inspect(comment_params, label: "Comment params received")

    comment_params = Map.new(comment_params, fn {k, v} -> {String.to_atom(k), v} end)

    handle_save_comment(comment_params, socket)
  end

  def handle_event("save-comment", params, socket) do
    IO.puts("❌ Unexpected params format for save-comment: #{inspect(params)}")
    {:noreply, socket |> put_flash(:error, "Invalid comment data format")}
  end

  defp handle_save_comment(%{post_id: post_id, author: author, content: content} = comment_params, socket) do
    changeset = %Comment{}
      |> Comment.changeset(%{
        post_id: post_id,
        author: author,
        content: content
      })

    case Posts.create_comment(changeset) do
      {:ok, comment} ->
        cache_content(:comment, comment.id, content)
        post = rebuild_post(post_id)
        comments = get_comments_with_content_reliable(post.id)

        {:noreply,
         socket
         |> assign(:post, post)
         |> assign(:comments, comments)
         |> assign(:changeset, Comment.changeset(%Comment{}))}

      {:error, changeset} ->
        IO.puts("❌ Error creating comment: #{inspect(changeset.errors)}")
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  defp handle_save_comment(comment_params, socket) do
    IO.puts("❌ Missing required comment parameters in: #{inspect(comment_params)}")
    {:noreply, socket |> put_flash(:error, "Missing required fields")}
  end

  defp get_comments_with_content_reliable(post_id) do
    try do
      IO.puts("🔍 Fetching comments with reliable content for post #{post_id}")
      comments_start = :erlang.system_time(:millisecond)

      comments = Posts.get_comment_by_post_id(post_id)
      IO.inspect(length(comments), label: "Number of comments found")

      processed_comments = comments
      |> Enum.take(5)
      |> Enum.with_index()
      |> Enum.map(fn {comment, index} ->
        comment_start = :erlang.system_time(:millisecond)
        IO.puts("Processing comment #{index + 1}/#{min(length(comments), 5)} (ID: #{comment.id})")

        content = get_comment_content_reliable(comment.id)

        like_event = like_comment_event_cached(comment.id)

        result = comment
        |> Map.put(:content, content)
        |> Map.put(:like_comment_event, like_event)
        |> add_replies_optimized()

        comment_end = :erlang.system_time(:millisecond)
        IO.puts("✅ Comment #{comment.id} processed in #{comment_end - comment_start}ms")

        result
      end)

      comments_end = :erlang.system_time(:millisecond)
      IO.puts("✅ All comments processed in #{comments_end - comments_start}ms")

      processed_comments
    rescue
      e ->
        IO.puts("❌ Error getting comments: #{inspect(e)}")
        IO.puts("📍 Error stacktrace: #{inspect(__STACKTRACE__)}")
        []
    end
  end

  def handle_event("reply_comment_content", %{"comment" => comment_params} = _params, socket) do
    %{"comment_id" => comment_id, "content" => content} = comment_params
    user_id = socket.assigns.current_user.id

    IO.puts("🚀 Creating reply for comment #{comment_id} with content: #{content}")

    temp_reply = create_temp_reply(user_id, comment_id, content)

    updated_comments = add_reply_to_comment(socket.assigns.comments, comment_id, temp_reply)

    Task.start(fn ->
      case PostClient.reply_comment(user_id, to_charlist(comment_id), content) do
        {:ok, reply} ->
          IO.puts("✅ Reply saved successfully: #{inspect(reply.id)}")
          if reply && reply.id do
            cache_content(:reply, reply.id, content)
          end
          send(self(), {:reply_saved, comment_id, reply, temp_reply.id})

        error ->
          IO.puts("❌ Error saving reply: #{inspect(error)}")
          send(self(), {:reply_failed, comment_id, temp_reply.id})
      end
    end)

    {:noreply,
     socket
     |> assign(:comments, updated_comments)
     |> assign(:reply_comment, false)
     |> assign(:replying_to_comment_id, nil)}
  end


  def handle_info({:reply_saved, comment_id, real_reply, temp_id}, socket) do
    IO.puts("🔄 Updating temporary reply #{temp_id} with real reply #{real_reply.id}")

    updated_comments = replace_temp_reply_with_real(socket.assigns.comments, comment_id, temp_id, real_reply)

    {:noreply, assign(socket, :comments, updated_comments)}
  end

  def handle_info({:reply_failed, comment_id, temp_id}, socket) do
    IO.puts("❌ Removing failed reply #{temp_id}")

    updated_comments = remove_temp_reply(socket.assigns.comments, comment_id, temp_id)

    {:noreply,
     socket
     |> assign(:comments, updated_comments)
     |> put_flash(:error, "Failed to save reply. Please try again.")}
  end

  defp create_temp_reply(user_id, comment_id, content) do
    temp_id = "temp_" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))

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

  defp add_reply_to_comment(comments, comment_id, new_reply) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        existing_replies = comment.replies || []
        updated_replies = existing_replies ++ [new_reply]

        IO.puts("📝 Added reply to comment #{comment_id}. Total replies: #{length(updated_replies)}")
        Map.put(comment, :replies, updated_replies)
      else
        comment
      end
    end)
  end

  defp replace_temp_reply_with_real(comments, comment_id, temp_id, real_reply) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        updated_replies = Enum.map(comment.replies || [], fn reply ->
          if reply.id == temp_id do
            %{
              id: real_reply.id,
              content: reply.content,
              user_id: real_reply.user_id,
              comment_id: comment_id,
              inserted_at: real_reply.inserted_at || reply.inserted_at,
              updated_at: real_reply.updated_at || reply.updated_at,
              is_temp: false
            }
          else
            reply
          end
        end)

        Map.put(comment, :replies, updated_replies)
      else
        comment
      end
    end)
  end

  defp remove_temp_reply(comments, comment_id, temp_id) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        updated_replies = Enum.reject(comment.replies || [], fn reply ->
          reply.id == temp_id
        end)

        Map.put(comment, :replies, updated_replies)
      else
        comment
      end
    end)
  end

  def handle_event("reply_comment", %{"comment-id" => comment_id}, socket) do
    comment_id_charlist = comment_id |> to_charlist()
    IO.puts("📨 Setting reply state for comment: #{comment_id}")

    {:noreply,
     socket
     |> assign(:reply_comment, true)
     |> assign(:replying_to_comment_id, comment_id_charlist)}
  end

  def handle_event("cancel-comment-reply", _params, socket) do
    IO.puts("❌ Cancelling comment reply - resetting state")
    {:noreply,
     socket
     |> assign(:reply_comment, false)
     |> assign(:replying_to_comment_id, nil)}
  end

  def handle_info({:refresh_comment_content, comment_id}, socket) do
    case fetch_content_with_timeout(:comment, comment_id, 3000) do
      {:ok, content} ->
        cache_content(:comment, comment_id, content)
        comments = get_comments_with_content_reliable(socket.assigns.post.id)
        {:noreply, assign(socket, :comments, comments)}
      _ ->
        {:noreply, socket}
    end
  end

  def handle_event("delete-reply", %{"reply-id" => reply_id, "comment-id" => comment_id}, socket) do
    reply_id = reply_id |> to_charlist
    comment_id = comment_id |> to_charlist

    :postdb.delete_reply_from_mnesia(reply_id)

    clear_content_cache(:reply, reply_id)

    post = rebuild_post(socket.assigns.post.id)
    comments = get_comments_with_content(post.id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments)}
  end

  def handle_event("reply_comment", %{"comment-id" => comment_id}, socket) do
    comment_id_charlist = comment_id |> to_charlist()
    IO.puts("Setting reply state for comment: #{comment_id}")

    {:noreply,
     socket
     |> assign(:reply_comment, true)
     |> assign(:replying_to_comment_id, comment_id_charlist)}
  end

  def handle_event("cancel-comment-reply", _params, socket) do
    IO.puts("Cancelling comment reply - resetting state")
    {:noreply,
     socket
     |> assign(:reply_comment, false)
     |> assign(:replying_to_comment_id, nil)}
  end

  def handle_event("show-comments", %{"id" => post_id}, socket) do
    comments = get_comments_with_content(post_id)
    {:noreply, socket |> assign(:comments, comments)}
  end

  def handle_event("like-comment", %{"comment-id" => comment_id}, socket) do
    comment_id = comment_id |> to_charlist
    user_id = socket.assigns.current_user.id

    PostClient.like_comment(user_id, comment_id)

    post_id = socket.assigns.post.id |> to_charlist
    post = rebuild_post(post_id)
    comments = get_comments_with_content(List.to_string(post_id))

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments)}
  end

  def handle_event("unlike-comment", %{"comment-id" => comment_id}, socket) do
    comment_id = comment_id |> to_charlist
    user_id = socket.assigns.current_user.id |> to_charlist
    comments = socket.assigns.comments

    comment = Enum.find(comments, fn comment -> comment.id == comment_id |> to_charlist end)

    like =
      comment_id
      |> PostClient.get_comment_likes()
      |> Enum.map(&(&1 |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)))
      |> Enum.filter(&(&1.user_id == user_id))
      |> hd()

    updated_likes = Enum.filter(comment.likes, fn like_item -> like_item != like.id end)
    :postdb.update_comment_likes(comment_id, updated_likes)

    post_id = socket.assigns.post.id |> to_charlist
    post = rebuild_post(post_id)
    comments = get_comments_with_content(post_id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments)}
  end

  def handle_event("follow_user", %{"username" => username}, socket) do
    user_id = socket.assigns.current_user.id
    UserClient.follow(user_id, username)
    {:noreply, handle_assigns(socket, user_id, username)}
  end

  def handle_event("unfollow_user", %{"username" => username}, socket) do
    user_id = socket.assigns.current_user.id
    UserClient.unfollow(user_id, username)
    {:noreply, handle_assigns(socket, user_id, username)}
  end

  def handle_event("like_post", %{"post-id" => post_id}, socket) do
    post_id = post_id |> to_charlist
    user_id = socket.assigns.current_user.id
    PostClient.like_post(user_id, post_id)

    clear_content_cache(:likes_count, post_id)

    post = rebuild_post(post_id)
    likes_count = get_likes_count(post_id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:like_icon, like_icon(user_id, post_id))
     |> assign(:like_event, like_event(user_id, post_id))
     |> assign(:likes_count, likes_count)
     |> assign(:is_liked, true)}
  end

  def handle_event("unlike_post", %{"post-id" => post_id}, socket) do
    post_id = post_id |> to_charlist
    user_id = socket.assigns.current_user.id

    like =
      post_id
      |> PostClient.get_likes()
      |> Enum.map(&(&1 |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)))
      |> Enum.filter(&(&1.user_id == user_id))
      |> hd()

    PostClient.unlike_post(like.id, post_id)

    clear_content_cache(:likes_count, post_id)

    post = rebuild_post(post_id)
    likes_count = get_likes_count(post_id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:like_icon, like_icon(user_id, post_id))
     |> assign(:like_event, like_event(user_id, post_id))
     |> assign(:likes_count, likes_count)}
  end

  def handle_event("show_likes", %{"post-id" => post_id}, socket) do
    post_id = post_id |> to_charlist

    users =
      post_id
      |> PostClient.get_likes()
      |> Enum.map(&(&1 |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)))
      |> Enum.map(fn like -> like.user_id |> Core.UserClient.get_user_by_id() end)
      |> Enum.map(&(&1 |> elem(2) |> Users.one_by_username()))

    {:noreply, assign(socket, users: users)}
  end

  def handle_event("open_modal", %{"action" => "report-post"}, socket) do
    {:noreply,
     socket
     |> assign(
       like_action: false,
       report_action: true,
       edit_action: false,
       follower_action: false,
       follows_action: false
     )}
  end

  def handle_event("open_modal", %{"action" => "like-post"}, socket) do
    {:noreply,
     socket
     |> assign(
       like_action: true,
       report_action: false,
       edit_action: false,
       follower_action: false,
       follows_action: false
     )}
  end

  def get_user_avatar(author) do
    case Account.Users.one_by_username(author) do
      {:ok, user} ->
        avatar_cid = user.avatar_url
        if avatar_cid do
          Mazaryn.config([:media, :ipfs_gateway]) <> avatar_cid
        else
          ~p"/images/default-user.svg"
        end
      {:error, _changeset} -> ""
    end
  end

  defp rebuild_post(post_id) do
    {:ok, post} =
      PostClient.get_by_id(post_id)
      |> Mazaryn.Schema.Post.erl_changeset()
      |> Mazaryn.Schema.Post.build()
    post
  end

  defp activate_content_characters(post, socket) do
    try do
      content_str = cond do
        ipfs_content = Core.PostClient.get_post_content_by_id(post.id) ->
          cond do
            is_binary(ipfs_content) -> ipfs_content
            is_list(ipfs_content) -> List.to_string(ipfs_content)
            true ->
              case post.content do
                content when is_binary(content) -> content
                content when is_list(content) -> List.to_string(content)
                _ -> "No content available"
              end
          end
        true ->
          case post.content do
            content when is_binary(content) -> content
            content when is_list(content) -> List.to_string(content)
            _ -> "No content available"
          end
      end

      if content_str == "" do
        "No content available"
      else
        link_regex = ~r/([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#\.]?[\w-]+)*\/?/

        content_str
        |> String.split()
        |> Enum.map(fn con ->
          case {check_regex(con, ~r/@\S[a-zA-Z]*/), check_regex(con, ~r/#\S[a-zA-Z]*/),
                check_regex(con, link_regex)} do
            {[[mention]], [], []} ->
              activate_mention_only(mention, socket)
            {[], [[hashtag]], []} ->
              activate_hashtag_only(hashtag, socket)
            {[], [], [[url | _rest]]} ->
              activate_url_only(url)
            {[[mention]], [[hashtag]], [[url | _rest]]} ->
              activate_mention_only(mention, socket)
              activate_hashtag_only(hashtag, socket)
              activate_url_only(url)
            _ ->
              escape_char(con)
          end
        end)
        |> Enum.join(" ")
        |> Earmark.as_html!(compact_output: true)
        |> apply_styles()
      end
    catch
      type, reason ->
        IO.puts("Unexpected error in content processing: #{inspect({type, reason})}")
        ("Error processing content" |> Earmark.as_html!(compact_output: true) |> apply_styles())
    end
  end

  defp apply_styles(html) do
    html
    |> String.replace("<a", "<a class=\"text-blue-500\"")
    |> String.replace("</a>", "</a>")
  end

  defp activate_hashtag_only(hashtag, socket) do
    locale = Gettext.get_locale(MazarynWeb.Gettext)
    path = Routes.live_path(socket, MazarynWeb.HashtagLive.Index, locale, hashtag)
    markdown = "[\ #{hashtag}](#{path})"
    String.replace(hashtag, hashtag, markdown)
  end

  defp activate_mention_only(mention, socket) do
    path =
      mention
      |> String.replace("@", "")
      |> create_user_path(socket)
    markdown = "[\ #{mention}](#{path})"
    String.replace(mention, mention, markdown)
  end

  defp activate_url_only("http" <> _rest = url), do: url
  defp activate_url_only(url) do
    path = "https" <> ":" <> "//" <> "#{url}"
    "[\ #{url}](#{path})"
  end

  defp escape_char("#"), do: "\\#"
  defp escape_char(con), do: con

  defp check_regex(con, regex) do
    cond do
      con == "#" -> "#"
      con == "@" -> "@"
      true -> regex |> Regex.scan(con)
    end
  end

  defp create_user_path(username, socket) do
    case Users.one_by_username(username) do
      :ok -> "#"
      {:ok, _user} ->
        locale = Gettext.get_locale(MazarynWeb.Gettext)
        Routes.live_path(socket, MazarynWeb.UserLive.Profile, locale, username)
    end
  end

  defp handle_assigns(socket, user_id, username) do
    socket
    |> assign(:follow_event, follow_event(user_id, username))
    |> assign(:follow_text, follow_text(user_id, username))
  end

  defp one_of_following?(id, username) do
    id
    |> UserClient.get_following()
    |> Enum.any?(&(&1 == username))
  end

  defp follow_text(id, username) do
    if one_of_following?(id, username),
      do: "Unfollow",
      else: "Follow"
  end

  defp follow_event(id, username) do
    if one_of_following?(id, username),
      do: "unfollow_user",
      else: "follow_user"
  end

  defp one_of_likes?(user_id, post_id) do
    post_id
    |> PostClient.get_likes()
    |> Enum.map(fn like ->
      like
      |> Home.Like.erl_changeset()
      |> Home.Like.build()
      |> elem(1)
    end)
    |> Enum.any?(&(&1.user_id == user_id))
  end

  defp like_icon(user_id, post_id) do
    if one_of_likes?(user_id, post_id),
      do: "hand-thumb-down",
      else: "hand-thumb-up"
  end

  defp like_event(user_id, post_id) do
    if one_of_likes?(user_id, post_id),
      do: "unlike_post",
      else: "like_post"
  end

  defp verified?(author) do
    case Users.one_by_username(author) do
      {:ok, user} -> user.verified
      _ -> false
    end
  end

  defp one_of_comment_likes?(user_id, comment_id) do
    comment_id
    |> PostClient.get_comment_likes()
    |> Enum.map(fn like ->
      like
      |> Home.Like.erl_changeset()
      |> Home.Like.build()
      |> elem(1)
    end)
    |> Enum.any?(&(&1.user_id == user_id))
  end

  defp comment_like_color(user_id, comment_id) do
    if one_of_comment_likes?(user_id, comment_id),
      do: "text-blue-500",
      else: "text-gray-500"
  end

  def get_image_url(post_id) do
     cid = PostClient.get_media_cid(post_id)
     Mazaryn.config([:media, :ipfs_gateway]) <> cid
  end

end
