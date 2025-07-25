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
  alias MazarynWeb.HomeLive.CommentHandler
  alias Home.Like

  @content_cache :post_content_cache
  @batch_size 5
  @max_concurrent_tasks 8

  defp get_post_content_optimized(post_id) do
    cache_key = {:post_content, post_id}
    fetch_start = :erlang.system_time(:millisecond)
    IO.puts("📄 Starting content fetch for post #{post_id}")

    result = case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, content, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp
        if age < 600 do
          IO.puts("📦 Content Cache HIT for post #{post_id} (age: #{age}s)")
          content
        else
          IO.puts("⏰ Content Cache EXPIRED for post #{post_id} (age: #{age}s)")
          spawn_content_refresh(post_id)
          content
        end
      [] ->
        IO.puts("❌ Content Cache MISS for post #{post_id}")
        case get_post_content_with_timeout(post_id, 800) do
          {:ok, content} ->
            cache_key = {:post_content, post_id}
            timestamp = :erlang.system_time(:second)
            :ets.insert(@content_cache, {cache_key, content, timestamp})
            IO.puts("💾 Cached content for post #{post_id}")
            content
          {:timeout, msg} ->
            IO.puts("⏰ Timeout fetching content for post #{post_id}: #{msg}")
            "Content loading..."
          {:error, reason} ->
            IO.puts("❌ Error fetching content for post #{post_id}: #{inspect(reason)}")
            "Content loading..."
        end
    end

    fetch_end = :erlang.system_time(:millisecond)
    IO.puts("📄 Content fetch for post #{post_id} completed in #{fetch_end - fetch_start}ms")
    result
  end

  defp get_post_content_with_timeout(post_id, timeout) do
    fetch_start = :erlang.system_time(:millisecond)
    IO.puts("📡 Starting IPFS content fetch for post #{post_id} with timeout #{timeout}ms")

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
          IO.puts("❌ Error fetching post content for post #{post_id}: #{inspect({type, reason})}")
          {:error, {type, reason}}
      end
    end)

    result = case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} ->
        IO.puts("✅ Successfully fetched content for post #{post_id}")
        result
      nil ->
        IO.puts("⏰ Timeout after #{timeout}ms fetching content for post #{post_id}")
        {:timeout, "Timeout after #{timeout}ms"}
    end

    fetch_end = :erlang.system_time(:millisecond)
    IO.puts("📡 IPFS content fetch for post #{post_id} took #{fetch_end - fetch_start}ms")
    result
  end

  def handle_event("save-comment", params, socket) do
    save_start = :erlang.system_time(:millisecond)
    IO.puts("💾 Starting save-comment event for post #{socket.assigns.post.id}")

    result = case CommentHandler.handle_save_comment(params) do
      {:ok, %{post: post, comments: comments, changeset: changeset, flash: {flash_type, message}}} ->
        IO.puts("✅ Comment saved successfully for post #{socket.assigns.post.id}")
        {:noreply,
         socket
         |> assign(:post, post)
         |> assign(:comments, comments)
         |> assign(:changeset, changeset)
         |> put_flash(flash_type, message)}

      {:error, :missing_post_id} ->
        IO.puts("❌ Missing post ID in save-comment")
        {:noreply, socket |> put_flash(:error, "Missing post ID")}

      {:error, :missing_author} ->
        IO.puts("❌ Missing author in save-comment")
        {:noreply, socket |> put_flash(:error, "Missing author")}

      {:error, {:validation, changeset}} ->
        IO.puts("❌ Validation error in save-comment: #{inspect(changeset.errors)}")
        {:noreply, assign(socket, :changeset, changeset)}

      {:error, {:changeset, changeset}} ->
        IO.puts("❌ Changeset error in save-comment: #{inspect(changeset.errors)}")
        {:noreply, assign(socket, :changeset, changeset)}

      {:error, :invalid_params} ->
        IO.puts("❌ Invalid params in save-comment")
        {:noreply, socket |> put_flash(:error, "Invalid form data")}

      {:error, reason} ->
        IO.puts("❌ Failed to save comment: #{inspect(reason)}")
        {:noreply, socket |> put_flash(:error, "Failed to save comment")}
    end

    save_end = :erlang.system_time(:millisecond)
    IO.puts("💾 Save-comment event for post #{socket.assigns.post.id} completed in #{save_end - save_start}ms")
    result
  end

  def handle_info(:refresh_processing_content, socket) do
    refresh_start = :erlang.system_time(:millisecond)
    IO.puts("🔄 Starting refresh_processing_content for post #{socket.assigns.post.id}")

    comments = CommentHandler.get_comments_with_content(socket.assigns.post.id)

    processing_content = Enum.any?(comments, fn comment ->
      comment.content in ["Content is being processed...", "Content loading..."]
    end)

    if processing_content do
      IO.puts("🔄 Scheduling next refresh for post #{socket.assigns.post.id} in 2000ms")
      Process.send_after(self(), :refresh_processing_content, 2000)
    else
      IO.puts("✅ No processing content found for post #{socket.assigns.post.id}, stopping refresh")
    end

    refresh_end = :erlang.system_time(:millisecond)
    IO.puts("🔄 refresh_processing_content for post #{socket.assigns.post.id} completed in #{refresh_end - refresh_start}ms")

    {:noreply, assign(socket, :comments, comments)}
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

  defp process_single_post(assigns, socket, changeset, update_comment_changeset, update_post_changeset) do
    try do
      post_start = :erlang.system_time(:millisecond)
      IO.puts("--- Processing post #{assigns.post.id} ---")

      tasks = %{
        comments: Task.async(fn ->
          comment_start = :erlang.system_time(:millisecond)
          result = CommentHandler.get_comments_with_content_optimized(assigns.post.id)
          comment_end = :erlang.system_time(:millisecond)
          IO.puts("📝 Comments fetch for post #{assigns.post.id} took #{comment_end - comment_start}ms")
          result
        end),
        ipns: Task.async(fn ->
          ipns_start = :erlang.system_time(:millisecond)
          result = get_post_ipns_fast(assigns.post.id)
          ipns_end = :erlang.system_time(:millisecond)
          IO.puts("🔗 IPNS fetch for post #{assigns.post.id} took #{ipns_end - ipns_start}ms")
          result
        end),
        likes: Task.async(fn ->
          likes_start = :erlang.system_time(:millisecond)
          result = get_likes_count_cached(assigns.post.id)
          likes_end = :erlang.system_time(:millisecond)
          IO.puts("📊 Likes count fetch for post #{assigns.post.id} took #{likes_end - likes_start}ms")
          result
        end),
        content: Task.async(fn ->
          content_start = :erlang.system_time(:millisecond)
          result = get_post_content_optimized(assigns.post.id)
          content_end = :erlang.system_time(:millisecond)
          IO.puts("📄 Post content fetch for post #{assigns.post.id} took #{content_end - content_start}ms")
          result
        end)
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

  def handle_info({:comment_content_updated, comment_id, content}, socket) do
    updated_comments = CommentHandler.update_comment_content_in_list(socket.assigns[:comments] || [], comment_id, content)

    if updated_comments != socket.assigns[:comments] do
      IO.puts("🔄 Updating comment #{comment_id} with fresh content")
      {:noreply, assign(socket, :comments, updated_comments)}
    else
      {:noreply, socket}
    end
  end

  def handle_info({:reply_content_updated, reply_id, content}, socket) do
    updated_comments = CommentHandler.update_reply_content_in_comments(socket.assigns[:comments] || [], reply_id, content)

    if updated_comments != socket.assigns[:comments] do
      IO.puts("🔄 Updating reply #{reply_id} with fresh content")
      {:noreply, assign(socket, :comments, updated_comments)}
    else
      {:noreply, socket}
    end
  end

  def handle_info({:reply_saved, comment_id, real_reply, temp_id}, socket) do
    IO.puts("🔄 Updating temporary reply #{temp_id} with real reply #{real_reply.id}")

    updated_comments = CommentHandler.replace_temp_reply_with_real(socket.assigns.comments, comment_id, temp_id, real_reply)

    {:noreply, assign(socket, :comments, updated_comments)}
  end

  def handle_info({:reply_failed, comment_id, temp_id}, socket) do
    IO.puts("❌ Removing failed reply #{temp_id}")

    updated_comments = CommentHandler.remove_temp_reply(socket.assigns.comments, comment_id, temp_id)

    {:noreply,
     socket
     |> assign(:comments, updated_comments)
     |> put_flash(:error, "Failed to save reply. Please try again.")}
  end

  def handle_info({:refresh_comment_content, comment_id}, socket) do
    case CommentHandler.fetch_content_with_timeout(:comment, comment_id, 3000) do
      {:ok, content} ->
        CommentHandler.cache_content(:comment, comment_id, content)
        comments = CommentHandler.get_comments_with_content_reliable(socket.assigns.post.id)
        {:noreply, assign(socket, :comments, comments)}
      _ ->
        {:noreply, socket}
    end
  end

  def handle_info({:comment_deleted_success, comment_id}, socket) do
    IO.puts("✅ Comment #{comment_id} deletion confirmed")
    {:noreply, socket}
  end

  def handle_info({:comment_deletion_failed, comment_id, comment_to_delete}, socket) do
    IO.puts("🔄 Restoring comment #{comment_id} due to deletion failure")

    if comment_to_delete do
      updated_comments = CommentHandler.restore_comment_to_list(socket.assigns.comments, comment_to_delete)

      {:noreply,
       socket
       |> assign(:comments, updated_comments)
       |> put_flash(:error, "Failed to delete comment. Please try again.")}
    else
      {:noreply, socket |> put_flash(:error, "Failed to delete comment.")}
    end
  end

  def handle_info({:reply_deleted_success, reply_id, comment_id}, socket) do
    IO.puts("✅ Reply #{reply_id} deletion confirmed")
    {:noreply, socket}
  end

  def handle_info({:reply_deletion_failed, reply_id, comment_id, reply_to_delete}, socket) do
    IO.puts("🔄 Restoring reply #{reply_id} due to deletion failure")

    if reply_to_delete do
      updated_comments = CommentHandler.restore_reply_to_comments(
        socket.assigns.comments,
        comment_id,
        reply_to_delete
      )

      {:noreply,
       socket
       |> assign(:comments, updated_comments)
       |> put_flash(:error, "Failed to delete reply. Please try again.")}
    else
      {:noreply, socket |> put_flash(:error, "Failed to delete reply.")}
    end
  end

  def handle_event("save-comment", params, socket) do
    case CommentHandler.handle_save_comment(params) do
      {:ok, %{post: post, comments: comments, changeset: changeset, flash: {flash_type, message}}} ->
        {:noreply,
         socket
         |> assign(:post, post)
         |> assign(:comments, comments)
         |> assign(:changeset, changeset)
         |> put_flash(flash_type, message)}

      {:error, :missing_post_id} ->
        {:noreply, socket |> put_flash(:error, "Missing post ID")}

      {:error, :missing_author} ->
        {:noreply, socket |> put_flash(:error, "Missing author")}

      {:error, {:validation, changeset}} ->
        {:noreply, assign(socket, :changeset, changeset)}

      {:error, {:changeset, changeset}} ->
        {:noreply, assign(socket, :changeset, changeset)}

      {:error, :invalid_params} ->
        {:noreply, socket |> put_flash(:error, "Invalid form data")}

      {:error, _} ->
        {:noreply, socket |> put_flash(:error, "Failed to save comment")}
    end
  end

  def handle_event("update-comment", params, socket) do
    post_id = socket.assigns.post.id

    case CommentHandler.handle_update_comment(params, post_id) do
      {:ok, %{comments: comments, editing_comment: editing_comment, editing_comment_id: editing_comment_id,
              update_comment_changeset: changeset, flash: {flash_type, message}}} ->
        {:noreply,
         socket
         |> assign(:comments, comments)
         |> assign(:editing_comment, editing_comment)
         |> assign(:editing_comment_id, editing_comment_id)
         |> assign(:update_comment_changeset, changeset)
         |> put_flash(flash_type, message)}

      {:error, %{update_comment_changeset: changeset, flash: {flash_type, message}}} ->
        {:noreply,
         socket
         |> assign(:update_comment_changeset, changeset)
         |> put_flash(flash_type, message)}

      {:error, %{flash: {flash_type, message}, editing_comment: editing_comment, editing_comment_id: editing_comment_id}} ->
        {:noreply,
         socket
         |> assign(:editing_comment, editing_comment)
         |> assign(:editing_comment_id, editing_comment_id)
         |> put_flash(flash_type, message)}

      {:error, %{flash: {flash_type, message}}} ->
        {:noreply, socket |> put_flash(flash_type, message)}
    end
  end

  def handle_event("validate-comment", params, socket) do
    case CommentHandler.handle_validate_comment(params) do
      {:ok, %{changeset: changeset}} ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  def handle_event("validate-update-comment", params, socket) do
    case CommentHandler.handle_validate_update_comment(params) do
      {:ok, %{update_comment_changeset: changeset}} ->
        {:noreply, assign(socket, :update_comment_changeset, changeset)}
    end
  end

  def handle_event("edit-comment", params, socket) do
    case CommentHandler.handle_edit_comment(params) do
      {:ok, %{editing_comment: editing_comment, editing_comment_id: editing_comment_id}} ->
        {:noreply,
         socket
         |> assign(:editing_comment, editing_comment)
         |> assign(:editing_comment_id, editing_comment_id)}
    end
  end

  def handle_event("cancel-comment-edit", params, socket) do
    case CommentHandler.handle_cancel_comment_edit(params) do
      {:ok, %{editing_comment: editing_comment, editing_comment_id: editing_comment_id}} ->
        {:noreply,
         socket
         |> assign(:editing_comment, editing_comment)
         |> assign(:editing_comment_id, editing_comment_id)}
    end
  end

  def handle_event("reply_comment", params, socket) do
    case CommentHandler.handle_reply_comment(params) do
      {:ok, %{reply_comment: reply_comment, replying_to_comment_id: replying_to_comment_id}} ->
        {:noreply,
         socket
         |> assign(:reply_comment, reply_comment)
         |> assign(:replying_to_comment_id, replying_to_comment_id)}
    end
  end

  def handle_event("cancel-comment-reply", params, socket) do
    case CommentHandler.handle_cancel_comment_reply(params) do
      {:ok, %{reply_comment: reply_comment, replying_to_comment_id: replying_to_comment_id}} ->
        {:noreply,
         socket
         |> assign(:reply_comment, reply_comment)
         |> assign(:replying_to_comment_id, replying_to_comment_id)}
    end
  end

  def handle_event("reply_comment_content", params, socket) do
    user_id = socket.assigns.current_user.id
    comments = socket.assigns.comments

    case CommentHandler.handle_reply_comment_content(params, user_id, comments) do
      {:ok, %{comments: updated_comments, reply_comment: reply_comment, replying_to_comment_id: replying_to_comment_id}} ->
        {:noreply,
         socket
         |> assign(:comments, updated_comments)
         |> assign(:reply_comment, reply_comment)
         |> assign(:replying_to_comment_id, replying_to_comment_id)}

      {:error, %{flash: {flash_type, message}}} ->
        {:noreply, socket |> put_flash(flash_type, message)}
    end
  end

  def handle_event("delete-comment", params, socket) do
    comments = socket.assigns.comments

    case CommentHandler.handle_delete_comment(params, comments) do
      {:ok, %{comments: updated_comments}} ->
        {:noreply, assign(socket, :comments, updated_comments)}
    end
  end

  def handle_event("delete-reply", params, socket) do
    comments = socket.assigns.comments

    case CommentHandler.handle_delete_reply(params, comments) do
      {:ok, %{comments: updated_comments}} ->
        {:noreply, assign(socket, :comments, updated_comments)}
    end
  end

  def handle_event("show-comments", params, socket) do
    case CommentHandler.handle_show_comments(params) do
      {:ok, %{comments: comments}} ->
        {:noreply, assign(socket, :comments, comments)}
    end
  end

  def handle_event("like-comment", params, socket) do
    post_id = socket.assigns.post.id
    user_id = socket.assigns.current_user.id

    case CommentHandler.handle_like_comment(params, post_id, user_id) do
      {:ok, %{post: post, comments: comments}} ->
        {:noreply,
         socket
         |> assign(:post, post)
         |> assign(:comments, comments)}
    end
  end

  def handle_event("unlike-comment", params, socket) do
    post_id = socket.assigns.post.id
    user_id = socket.assigns.current_user.id
    comments = socket.assigns.comments

    case CommentHandler.handle_unlike_comment(params, post_id, user_id, comments) do
      {:ok, %{post: post, comments: comments}} ->
        {:noreply,
         socket
         |> assign(:post, post)
         |> assign(:comments, comments)}
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

  defp get_post_ipns_with_timeout(post_id, timeout) do
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

  def handle_info({:background_update, post_id, field, value}, socket) do
    if socket.assigns[:post] && to_string(socket.assigns.post.id) == to_string(post_id) do
      {:noreply, assign(socket, field, value)}
    else
      {:noreply, socket}
    end
  end

  defp get_likes_count_with_timeout(post_id, timeout) do
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

  defp get_post_content_with_timeout(post_id, timeout) do
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
     |> assign(:show_likes_modal, false)
     |> assign(:liked_users, [])
     |> assign(:likes_loading, false)
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  def handle_event("open_likes_modal", %{"post-id" => post_id}, socket) do
    IO.puts("🔍 Opening likes modal for post #{post_id}")

    socket = socket
    |> assign(:show_likes_modal, true)
    |> assign(:likes_loading, true)
    |> assign(:liked_users, [])

    try do
      post_id_charlist = if is_binary(post_id), do: to_charlist(post_id), else: post_id

      users_task = Task.async(fn ->
        raw_likes = PostClient.get_likes(post_id_charlist)
        IO.puts("📦 Found #{length(raw_likes)} likes")

        user_ids = raw_likes
        |> Enum.map(fn like ->
          case Like.erl_changeset(like) |> Like.build() do
            {:ok, like_struct} -> like_struct.user_id
            _ -> nil
          end
        end)
        |> Enum.filter(&(&1 != nil))

        users = user_ids
        |> Enum.map(fn user_id ->
          try do
            user_tuple = Core.UserClient.get_user_by_id(user_id)
            if tuple_size(user_tuple) >= 9 do
              username = elem(user_tuple, 8)
              username_str = if is_list(username), do: List.to_string(username), else: username

              case Users.one_by_username(username_str) do
                {:ok, user} -> user
                _ -> nil
              end
            else
              nil
            end
          rescue
            _ -> nil
          end
        end)
        |> Enum.filter(&(&1 != nil))

        IO.puts("✅ Successfully processed #{length(users)} users: #{inspect(Enum.map(users, & &1.username))}")
        users
      end)

      case Task.yield(users_task, 3000) || Task.shutdown(users_task, :brutal_kill) do
        {:ok, users} ->
          IO.puts("🎉 Got users, updating modal: #{length(users)} users")
          {:noreply,
           socket
           |> assign(:liked_users, users)
           |> assign(:likes_loading, false)}

        nil ->
          IO.puts("⏰ Timeout fetching users")
          {:noreply,
           socket
           |> assign(:liked_users, [])
           |> assign(:likes_loading, false)
           |> put_flash(:error, "Timeout loading likes")}
      end

    rescue
      e ->
        IO.puts("❌ Error in open_likes_modal: #{inspect(e)}")
        {:noreply,
         socket
         |> assign(:liked_users, [])
         |> assign(:likes_loading, false)
         |> put_flash(:error, "Failed to load likes")}
    end
  end

  def handle_info({:likes_fetched, users}, socket) do
    IO.puts("📨 PostComponent: Received #{length(users)} users for likes modal")
    IO.puts("📨 Users: #{inspect(Enum.map(users, & &1.username))}")

    {:noreply,
     socket
     |> assign(:liked_users, users)
     |> assign(:likes_loading, false)}
  end

  def handle_info({:likes_fetch_error, error}, socket) do
    IO.puts("📨 PostComponent: Error fetching likes: #{inspect(error)}")

    {:noreply,
     socket
     |> assign(:liked_users, [])
     |> assign(:likes_loading, false)
     |> put_flash(:error, "Failed to load likes")}
  end

  def handle_event("close_likes_modal", _params, socket) do
    IO.puts("❌ Closing likes modal")

    {:noreply,
     socket
     |> assign(:show_likes_modal, false)
     |> assign(:liked_users, [])
     |> assign(:likes_loading, false)}
  end

  def handle_event("view_profile", %{"username" => username}, socket) do
    IO.puts("🔍 Navigating to profile for user: #{username}")

    locale = socket.assigns[:locale] || "en"

    profile_path = Routes.live_path(socket, MazarynWeb.UserLive.Profile, locale, username)

    socket = if socket.assigns[:show_likes_modal] do
      socket
      |> assign(:show_likes_modal, false)
      |> assign(:liked_users, [])
      |> assign(:likes_loading, false)
    else
      socket
    end

    {:noreply, socket |> push_navigate(to: profile_path)}
  end

  def handle_info(:refresh_processing_content, socket) do
    comments = CommentHandler.get_comments_with_content(socket.assigns.post.id)

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
      |> Enum.map(&(&1 |> Like.erl_changeset() |> Like.build() |> elem(1)))
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
      |> Enum.map(&(&1 |> Like.erl_changeset() |> Like.build() |> elem(1)))
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

  def handle_event("close_modal", _params, socket) do
    {:noreply, assign(socket, :users, [])}
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
  process_start = :erlang.system_time(:millisecond)
  IO.puts("📝 Starting content processing for post #{post.id}")

  result = try do
    content_str = cond do
      ipfs_content = Core.PostClient.get_post_content_by_id(post.id) ->
        fetch_start = :erlang.system_time(:millisecond)
        content = cond do
          is_binary(ipfs_content) ->
            IO.puts("✅ IPFS content fetched for post #{post.id}")
            ipfs_content
          is_list(ipfs_content) ->
            IO.puts("🔄 Converting IPFS content list to string for post #{post.id}")
            List.to_string(ipfs_content)
          true ->
            IO.puts("⚠️ Falling back to post content for post #{post.id}")
            case post.content do
              content when is_binary(content) -> content
              content when is_list(content) -> List.to_string(content)
              _ -> "No content available"
            end
        end
        fetch_end = :erlang.system_time(:millisecond)
        IO.puts("📡 IPFS content fetch in activate_content_characters for post #{post.id} took #{fetch_end - fetch_start}ms")
        content
      true ->
        IO.puts("⚠️ No IPFS content, using post content for post #{post.id}")
        case post.content do
          content when is_binary(content) -> content
          content when is_list(content) -> List.to_string(content)
          _ -> "No content available"
        end
    end

    if content_str == "" do
      IO.puts("⚠️ Empty content for post #{post.id}")
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
      IO.puts("❌ Unexpected error in content processing for post #{post.id}: #{inspect({type, reason})}")
      ("Error processing content" |> Earmark.as_html!(compact_output: true) |> apply_styles())
  end

  process_end = :erlang.system_time(:millisecond)
  IO.puts("📝 Content processing for post #{post.id} completed in #{process_end - process_start}ms")
  result
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
      |> Like.erl_changeset()
      |> Like.build()
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
      |> Like.erl_changeset()
      |> Like.build()
      |> elem(1)
    end)
    |> Enum.any?(&(&1.user_id == user_id))
  end

  defp comment_like_color(user_id, comment_id) do
    if one_of_comment_likes?(user_id, comment_id),
      do: "text-blue-500",
      else: "text-gray-500"
  end

  defp post_has_media?(post_id) do
    try do
      case PostClient.get_media_cid(post_id) do
        cid when is_binary(cid) and byte_size(cid) > 0 ->
          String.starts_with?(cid, "Qm") and String.length(cid) == 46
        cid when is_list(cid) ->
          case List.to_string(cid) do
            str when byte_size(str) > 0 ->
              String.starts_with?(str, "Qm") and String.length(str) == 46
            _ ->
              false
          end
        _ ->
          false
      end
    rescue
      _ -> false
    catch
      _, _ -> false
    end
  end

  def get_image_url(post_id) do
     cid = PostClient.get_media_cid(post_id)
     Mazaryn.config([:media, :ipfs_gateway]) <> cid
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

  defp clear_content_cache(type, id) do
    cache_key = {type, id}
    :ets.delete(@content_cache, cache_key)
  end

  defp ensure_cache_table do
    unless :ets.whereis(@content_cache) != :undefined do
      :ets.new(@content_cache, [:set, :public, :named_table, {:read_concurrency, true}])
    end
  end
end
