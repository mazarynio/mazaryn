defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  import MazarynWeb.Live.Helper

  alias Account.Users
  alias Core.{UserClient, PostClient}
  alias Mazaryn.{Posts, Schema.Comment, Schema.Post}
  alias Phoenix.LiveView.JS
  alias MazarynWeb.HomeLive.{CommentHandler, IpnsManager}
  alias Home.Like

  @content_cache :post_content_cache
  @batch_size 5

  @impl true
  def mount(socket) do
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "post_updates")

    {:ok,
     socket
     |> assign_default_state()
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  @impl Phoenix.LiveComponent
  def update_many(list_of_assigns) do
    IO.puts("=== OPTIMIZED UPDATE_MANY called with #{length(list_of_assigns)} posts ===")

    total_start = :erlang.system_time(:millisecond)
    ensure_cache_table()

    {changeset, update_comment_changeset, update_post_changeset} = prepare_changesets()

    result = list_of_assigns
    |> process_posts_in_batches(changeset, update_comment_changeset, update_post_changeset)

    total_duration = :erlang.system_time(:millisecond) - total_start
    IO.puts("🏁 TOTAL UPDATE_MANY completed in #{total_duration}ms")

    result
  end

  def update(%{ipns_id: ipns_id} = assigns, socket) when not is_nil(ipns_id) do
    IO.puts("🔄 PostComponent received IPNS update: #{inspect(ipns_id)}")

    updated_assigns = Map.delete(assigns, :ipns_id)

    socket = socket
    |> assign(:ipns_id, ipns_id)
    |> assign(updated_assigns)

    {:ok, socket}
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  def handle_event("save-comment", params, socket) do
    case CommentHandler.handle_save_comment(params) do
      {:ok, %{post: post, comments: comments, changeset: changeset, flash: {flash_type, message}}} ->
        {:noreply, socket |> assign_comment_success(post, comments, changeset) |> put_flash(flash_type, message)}

      {:error, reason} ->
        {:noreply, socket |> handle_comment_error(reason)}
    end
  end

  def handle_event("update-comment", params, socket) do
    post_id = socket.assigns.post.id

    case CommentHandler.handle_update_comment(params, post_id) do
      {:ok, result} ->
        {:noreply, socket |> assign_comment_update_success(result)}

      {:error, result} ->
        {:noreply, socket |> assign_comment_update_error(result)}
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
        {:noreply, socket |> assign(:editing_comment, editing_comment) |> assign(:editing_comment_id, editing_comment_id)}
    end
  end

  def handle_event("cancel-comment-edit", params, socket) do
    case CommentHandler.handle_cancel_comment_edit(params) do
      {:ok, %{editing_comment: editing_comment, editing_comment_id: editing_comment_id}} ->
        {:noreply, socket |> assign(:editing_comment, editing_comment) |> assign(:editing_comment_id, editing_comment_id)}
    end
  end

  def handle_event("reply_comment", params, socket) do
    case CommentHandler.handle_reply_comment(params) do
      {:ok, %{reply_comment: reply_comment, replying_to_comment_id: replying_to_comment_id}} ->
        {:noreply, socket |> assign(:reply_comment, reply_comment) |> assign(:replying_to_comment_id, replying_to_comment_id)}
    end
  end

  def handle_event("cancel-comment-reply", params, socket) do
    case CommentHandler.handle_cancel_comment_reply(params) do
      {:ok, %{reply_comment: reply_comment, replying_to_comment_id: replying_to_comment_id}} ->
        {:noreply, socket |> assign(:reply_comment, reply_comment) |> assign(:replying_to_comment_id, replying_to_comment_id)}
    end
  end

  def handle_event("reply_comment_content", params, socket) do
    user_id = socket.assigns.current_user.id
    comments = socket.assigns.comments

    case CommentHandler.handle_reply_comment_content(params, user_id, comments) do
      {:ok, %{comments: updated_comments, reply_comment: reply_comment, replying_to_comment_id: replying_to_comment_id}} ->
        {:noreply, socket |> assign(:comments, updated_comments) |> assign(:reply_comment, reply_comment) |> assign(:replying_to_comment_id, replying_to_comment_id)}

      {:error, %{flash: {flash_type, message}}} ->
        {:noreply, socket |> put_flash(flash_type, message)}
    end
  end

  def handle_event("delete-comment", params, socket) do
    case CommentHandler.handle_delete_comment(params, socket.assigns.comments) do
      {:ok, %{comments: updated_comments}} ->
        {:noreply, assign(socket, :comments, updated_comments)}
    end
  end

  def handle_event("delete-reply", params, socket) do
    case CommentHandler.handle_delete_reply(params, socket.assigns.comments) do
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
        {:noreply, socket |> assign(:post, post) |> assign(:comments, comments)}
    end
  end

  def handle_event("unlike-comment", params, socket) do
    post_id = socket.assigns.post.id
    user_id = socket.assigns.current_user.id
    comments = socket.assigns.comments

    case CommentHandler.handle_unlike_comment(params, post_id, user_id, comments) do
      {:ok, %{post: post, comments: comments}} ->
        {:noreply, socket |> assign(:post, post) |> assign(:comments, comments)}
    end
  end

  def handle_event("follow_user", %{"username" => username}, socket) do
    user_id = socket.assigns.current_user.id
    UserClient.follow(user_id, username)
    {:noreply, update_follow_assigns(socket, user_id, username)}
  end

  def handle_event("unfollow_user", %{"username" => username}, socket) do
    user_id = socket.assigns.current_user.id
    UserClient.unfollow(user_id, username)
    {:noreply, update_follow_assigns(socket, user_id, username)}
  end

  def handle_event("like_post", %{"post-id" => post_id}, socket) do
    post_id = to_charlist(post_id)
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
    post_id = to_charlist(post_id)
    user_id = socket.assigns.current_user.id

    like = find_user_like(post_id, user_id)
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

  def handle_event("open_likes_modal", %{"post-id" => post_id}, socket) do
    IO.puts("🔍 Opening likes modal for post #{post_id}")

    socket = socket
    |> assign(:show_likes_modal, true)
    |> assign(:likes_loading, true)
    |> assign(:liked_users, [])

    load_liked_users_async(post_id, self())

    {:noreply, socket}
  end

  def handle_event("close_likes_modal", _params, socket) do
    {:noreply, socket |> assign(:show_likes_modal, false) |> assign(:liked_users, []) |> assign(:likes_loading, false)}
  end

  def handle_event("view_profile", %{"username" => username}, socket) do
    locale = socket.assigns[:locale] || "en"
    profile_path = Routes.live_path(socket, MazarynWeb.UserLive.Profile, locale, username)

    socket = if socket.assigns[:show_likes_modal] do
      socket |> assign(:show_likes_modal, false) |> assign(:liked_users, []) |> assign(:likes_loading, false)
    else
      socket
    end

    {:noreply, socket |> push_navigate(to: profile_path)}
  end

  def handle_event("edit_post", %{"post-id" => post_id}, socket) do
    post_id = to_charlist(post_id)
    post = PostClient.get_post_content_by_id(post_id)
    {:noreply, socket |> assign(:editing_post, post) |> assign(:edit_post_id, post_id)}
  end

  def handle_event("update-post", %{"post" => post_params}, socket) do
    post_id = socket.assigns.edit_post_id
    post_id = if is_binary(post_id), do: :erlang.binary_to_list(post_id), else: post_id

    changeset = %Post{} |> Post.changeset(post_params)

    case changeset.valid? do
      true ->
        handle_valid_post_update(socket, post_id, post_params["content"])
      false ->
        {:noreply, assign(socket, :update_post_changeset, %{changeset | action: :validate})}
    end
  end

  def handle_event("cancel-edit", _params, socket) do
    {:noreply, assign(socket, :editing_post, false)}
  end

  def handle_event("delete-post", %{"post-id" => post_id}, socket) do
    post_id = to_charlist(post_id)
    PostClient.delete_post(post_id)

    clear_all_cache_for_post(post_id)
    send(self(), :reload_posts)

    {:noreply, socket}
  end

  def handle_event("open_modal", %{"action" => action}, socket) do
    modal_assigns = get_modal_assigns(action)
    {:noreply, assign(socket, modal_assigns)}
  end

  def handle_event("close_modal", _params, socket) do
    {:noreply, assign(socket, :users, [])}
  end

  def handle_info({:temp_comment_saved, temp_id, real_comment}, socket) do
    IO.puts("🔄 Replacing temp comment #{temp_id} with real comment #{real_comment.id}")
    updated_comments = replace_temp_comment_with_real(socket.assigns.comments, temp_id, real_comment)
    {:noreply, assign(socket, comments: updated_comments)}
  end

  def handle_info({:temp_comment_save_failed, temp_id, reason}, socket) do
    IO.puts("❌ Temp comment #{temp_id} save failed: #{inspect(reason)}")
    {:noreply, socket}
  end

  def handle_info({:comments_synced, post_id, fresh_comments}, socket) do
    if socket.assigns.post && socket.assigns.post.id == post_id do
      {:noreply, assign(socket, comments: fresh_comments)}
    else
      {:noreply, socket}
    end
  end

  def handle_info({:ipns_ready, post_id, ipns}, socket) do
    if socket.assigns[:post] && to_string(socket.assigns.post.id) == to_string(post_id) do
      IO.puts("🎉 Received scheduled IPNS update for displayed post #{post_id}")
      {:noreply, assign(socket, :ipns_id, ipns)}
    else
      {:noreply, socket}
    end
  end

  def handle_info({:ipns_updated, post_id, ipns}, socket) do
    if socket.assigns[:post] && to_string(socket.assigns.post.id) == to_string(post_id) do
      {:noreply, assign(socket, :ipns_id, ipns)}
    else
      {:noreply, socket}
    end
  end

  def handle_info({:likes_fetched, users}, socket) do
    {:noreply, socket |> assign(:liked_users, users) |> assign(:likes_loading, false)}
  end

  def handle_info({:likes_fetch_error, _error}, socket) do
    {:noreply, socket |> assign(:liked_users, []) |> assign(:likes_loading, false) |> put_flash(:error, "Failed to load likes")}
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

  def handle_info({:comment_content_updated, comment_id, content}, socket) do
    updated_comments = CommentHandler.update_comment_content_in_list(socket.assigns[:comments] || [], comment_id, content)

    if updated_comments != socket.assigns[:comments] do
      {:noreply, assign(socket, :comments, updated_comments)}
    else
      {:noreply, socket}
    end
  end

  def handle_info({:reply_content_updated, reply_id, content}, socket) do
    updated_comments = CommentHandler.update_reply_content_in_comments(socket.assigns[:comments] || [], reply_id, content)

    if updated_comments != socket.assigns[:comments] do
      {:noreply, assign(socket, :comments, updated_comments)}
    else
      {:noreply, socket}
    end
  end

  def handle_info({:reply_saved, comment_id, real_reply, temp_id}, socket) do
    updated_comments = CommentHandler.replace_temp_reply_with_real(socket.assigns.comments, comment_id, temp_id, real_reply)
    {:noreply, assign(socket, :comments, updated_comments)}
  end

  def handle_info({:reply_failed, comment_id, temp_id}, socket) do
    updated_comments = CommentHandler.remove_temp_reply(socket.assigns.comments, comment_id, temp_id)
    {:noreply, socket |> assign(:comments, updated_comments) |> put_flash(:error, "Failed to save reply. Please try again.")}
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  def warm_cache_for_recent_posts(recent_post_ids) when is_list(recent_post_ids) do
    IpnsManager.warm_cache_async(recent_post_ids)
    warm_likes_cache(recent_post_ids)
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

  defp assign_default_state(socket) do
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
  end

  defp prepare_changesets do
    changeset = Comment.changeset(%Comment{})
    update_comment_changeset = Comment.changeset(%Comment{})
    update_post_changeset = Post.changeset(%Post{})
    {changeset, update_comment_changeset, update_post_changeset}
  end

  defp process_posts_in_batches(list_of_assigns, changeset, update_comment_changeset, update_post_changeset) do
    list_of_assigns
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
  end

  defp process_single_post(assigns, socket, changeset, update_comment_changeset, update_post_changeset) do
    try do
      post_start = :erlang.system_time(:millisecond)
      IO.puts("--- Processing post #{assigns.post.id} ---")

      tasks = create_async_tasks(assigns.post.id)
      results = await_tasks_with_fallbacks(tasks, assigns.post.id)

      final_assigns = build_final_assigns(assigns, results, changeset, update_comment_changeset, update_post_changeset)

      post_end = :erlang.system_time(:millisecond)
      IO.puts("✅ Post #{assigns.post.id} completed in #{post_end - post_start}ms")

      assign(socket, final_assigns)
    rescue
      e ->
        IO.puts("❌ Error processing post #{assigns.post.id}: #{inspect(e)}")
        assign(socket, basic_assigns(assigns))
    end
  end

  defp create_async_tasks(post_id) do
    %{
      comments: Task.async(fn ->
        comment_start = :erlang.system_time(:millisecond)
        result = CommentHandler.get_comments_with_content_optimized(post_id)
        comment_end = :erlang.system_time(:millisecond)
        IO.puts("📝 Comments fetch for post #{post_id} took #{comment_end - comment_start}ms")
        result
      end),
      ipns: Task.async(fn ->
        ipns_start = :erlang.system_time(:millisecond)
        result = IpnsManager.get_ipns_fast(post_id)
        ipns_end = :erlang.system_time(:millisecond)
        IO.puts("🔗 IPNS fetch for post #{post_id} took #{ipns_end - ipns_start}ms")
        result
      end),
      likes: Task.async(fn ->
        likes_start = :erlang.system_time(:millisecond)
        result = get_likes_count_cached(post_id)
        likes_end = :erlang.system_time(:millisecond)
        IO.puts("📊 Likes count fetch for post #{post_id} took #{likes_end - likes_start}ms")
        result
      end),
      content: Task.async(fn ->
        content_start = :erlang.system_time(:millisecond)
        result = get_post_content_optimized(post_id)
        content_end = :erlang.system_time(:millisecond)
        IO.puts("📄 Post content fetch for post #{post_id} took #{content_end - content_start}ms")
        result
      end)
    }
  end

  defp await_tasks_with_fallbacks(tasks, post_id) do
    %{
      comments: await_with_fallback(tasks.comments, 2000, [], "comments", post_id),
      ipns_id: await_with_fallback_async(tasks.ipns, 200, nil, "ipns", post_id),
      likes_count: await_with_fallback(tasks.likes, 500, 0, "likes", post_id),
      post_content_cached: await_with_fallback(tasks.content, 1500, "Content loading...", "content", post_id)
    }
  end

  defp build_final_assigns(assigns, results, changeset, update_comment_changeset, update_post_changeset) do
    assigns
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

  defp assign_comment_success(socket, post, comments, changeset) do
    socket
    |> assign(:post, post)
    |> assign(:comments, comments)
    |> assign(:changeset, changeset)
  end

  defp handle_comment_error(socket, reason) do
    case reason do
      :missing_post_id -> socket |> put_flash(:error, "Missing post ID")
      :missing_author -> socket |> put_flash(:error, "Missing author")
      {:validation, changeset} -> assign(socket, :changeset, changeset)
      {:changeset, changeset} -> assign(socket, :changeset, changeset)
      :invalid_params -> socket |> put_flash(:error, "Invalid form data")
      _ -> socket |> put_flash(:error, "Failed to save comment")
    end
  end

  defp assign_comment_update_success(socket, result) do
    socket
    |> assign(:comments, result.comments)
    |> assign(:editing_comment, result.editing_comment)
    |> assign(:editing_comment_id, result.editing_comment_id)
    |> assign(:update_comment_changeset, result.update_comment_changeset)
    |> put_flash(elem(result.flash, 0), elem(result.flash, 1))
  end

  defp assign_comment_update_error(socket, result) do
    socket = case Map.get(result, :update_comment_changeset) do
      nil -> socket
      changeset -> assign(socket, :update_comment_changeset, changeset)
    end

    socket = case Map.get(result, :editing_comment) do
      nil -> socket
      editing_comment -> socket |> assign(:editing_comment, editing_comment) |> assign(:editing_comment_id, result.editing_comment_id)
    end

    {flash_type, message} = result.flash
    put_flash(socket, flash_type, message)
  end

  defp handle_valid_post_update(socket, post_id, new_content) do
    case PostClient.update_post(post_id, new_content) do
      :ok ->
        clear_all_cache_for_post(post_id)

        try do
          retrieved_post = rebuild_post(post_id)
          ipns_id = IpnsManager.get_ipns_fast(post_id)
          send(self(), :reload_posts)

          {:noreply,
           socket
           |> assign(:editing_post, false)
           |> assign(:edit_post_id, nil)
           |> assign(:ipns_id, ipns_id)}
        catch
          _, e ->
            IO.puts("Error retrieving post after update: #{inspect(e)}")
            {:noreply, socket |> assign(:editing_post, false) |> assign(:edit_post_id, nil)}
        end

      error ->
        IO.puts("Error updating post: #{inspect(error)}")
        {:noreply, socket |> put_flash(:error, "Failed to update post: #{inspect(error)}")}
    end
  end

  defp update_follow_assigns(socket, user_id, username) do
    socket
    |> assign(:follow_event, follow_event(user_id, username))
    |> assign(:follow_text, follow_text(user_id, username))
  end

  defp get_modal_assigns("report-post") do
    %{like_action: false, report_action: true, edit_action: false, follower_action: false, follows_action: false}
  end

  defp get_modal_assigns("like-post") do
    %{like_action: true, report_action: false, edit_action: false, follower_action: false, follows_action: false}
  end

  defp get_modal_assigns(_) do
    %{like_action: false, report_action: false, edit_action: false, follower_action: false, follows_action: false}
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

  defp get_likes_count_with_timeout(post_id, timeout) do
    task = Task.async(fn -> get_likes_count(post_id) end)
    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, count} -> count
      nil -> 0
    end
  end

  defp get_likes_count(post_id) do
    try do
      post_id
      |> to_charlist
      |> PostClient.get_likes()
      |> Enum.count()
    rescue
      e ->
        IO.puts("❌ Error getting likes count: #{inspect(e)}")
        0
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

  defp warm_likes_cache(recent_post_ids) do
    Task.start(fn ->
      recent_post_ids
      |> Enum.take(10)
      |> Enum.each(fn post_id ->
        spawn_likes_refresh(post_id)
        Process.sleep(100)
      end)
    end)
  end

  defp load_liked_users_async(post_id, parent_pid) do
    Task.start(fn ->
      try do
        post_id_charlist = if is_binary(post_id), do: to_charlist(post_id), else: post_id

        users = post_id_charlist
        |> PostClient.get_likes()
        |> Enum.map(fn like ->
          case Like.erl_changeset(like) |> Like.build() do
            {:ok, like_struct} -> like_struct.user_id
            _ -> nil
          end
        end)
        |> Enum.filter(&(&1 != nil))
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

        send(parent_pid, {:likes_fetched, users})
      rescue
        e -> send(parent_pid, {:likes_fetch_error, e})
      end
    end)
  end

  defp find_user_like(post_id, user_id) do
    post_id
    |> PostClient.get_likes()
    |> Enum.map(&(&1 |> Like.erl_changeset() |> Like.build() |> elem(1)))
    |> Enum.find(&(&1.user_id == user_id))
  end

  defp rebuild_post(post_id) do
    {:ok, post} =
      PostClient.get_by_id(post_id)
      |> Mazaryn.Schema.Post.erl_changeset()
      |> Mazaryn.Schema.Post.build()
    post
  end

  defp replace_temp_comment_with_real(comments, temp_id, real_comment) do
    Enum.map(comments, fn comment ->
      if comment.id == temp_id do
        %{
          id: real_comment.id,
          post_id: real_comment.post_id || comment.post_id,
          author: real_comment.author || comment.author,
          content: comment.content,
          inserted_at: real_comment.inserted_at || comment.inserted_at,
          updated_at: real_comment.updated_at || comment.updated_at,
          likes: real_comment.likes || [],
          replies: [],
          like_comment_event: "like-comment",
          is_temp: false
        }
      else
        comment
      end
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

  defp clear_content_cache(type, id) do
    cache_key = {type, id}
    :ets.delete(@content_cache, cache_key)
  end

  defp clear_all_cache_for_post(post_id) do
    clear_content_cache(:post, post_id)
    clear_content_cache(:ipns, post_id)
    clear_content_cache(:likes_count, post_id)
    clear_content_cache(:post_content, post_id)
    IpnsManager.clear_cache(post_id)
  end

  defp ensure_cache_table do
    unless :ets.whereis(@content_cache) != :undefined do
      :ets.new(@content_cache, [:set, :public, :named_table, {:read_concurrency, true}])
    end
  end

  defp one_of_following?(id, username) do
    id
    |> UserClient.get_following()
    |> Enum.any?(&(&1 == username))
  end

  defp follow_text(id, username) do
    if one_of_following?(id, username), do: "Unfollow", else: "Follow"
  end

  defp follow_event(id, username) do
    if one_of_following?(id, username), do: "unfollow_user", else: "follow_user"
  end

  defp one_of_likes?(user_id, post_id) do
    post_id
    |> PostClient.get_likes()
    |> Enum.map(fn like ->
      like |> Like.erl_changeset() |> Like.build() |> elem(1)
    end)
    |> Enum.any?(&(&1.user_id == user_id))
  end

  defp like_icon(user_id, post_id) do
    if one_of_likes?(user_id, post_id), do: "hand-thumb-down", else: "hand-thumb-up"
  end

  defp like_event(user_id, post_id) do
    if one_of_likes?(user_id, post_id), do: "unlike_post", else: "like_post"
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
      like |> Like.erl_changeset() |> Like.build() |> elem(1)
    end)
    |> Enum.any?(&(&1.user_id == user_id))
  end

  defp comment_like_color(user_id, comment_id) do
    if one_of_comment_likes?(user_id, comment_id), do: "text-blue-500", else: "text-gray-500"
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
            _ -> false
          end
        _ -> false
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

  def activate_content_characters(post, socket) do
    process_start = :erlang.system_time(:millisecond)
    IO.puts("📝 Starting content processing for post #{post.id}")

    result = try do
      content_str = get_processed_content(post)

      if content_str == "" do
        "No content available"
      else
        content_str
        |> String.split()
        |> Enum.map(&process_content_token(&1, socket))
        |> Enum.join(" ")
        |> Earmark.as_html!(compact_output: true)
        |> apply_styles()
      end
    catch
      type, reason ->
        IO.puts("❌ Unexpected error in content processing: #{inspect({type, reason})}")
        "Error processing content" |> Earmark.as_html!(compact_output: true) |> apply_styles()
    end

    process_end = :erlang.system_time(:millisecond)
    IO.puts("📝 Content processing completed in #{process_end - process_start}ms")
    result
  end

  defp get_processed_content(post) do
    cond do
      ipfs_content = Core.PostClient.get_post_content_by_id(post.id) ->
        process_ipfs_content(ipfs_content)
      true ->
        process_post_content(post.content)
    end
  end

  defp process_ipfs_content(ipfs_content) do
    cond do
      is_binary(ipfs_content) -> ipfs_content
      is_list(ipfs_content) -> List.to_string(ipfs_content)
      true -> "No content available"
    end
  end

  defp process_post_content(content) do
    case content do
      content when is_binary(content) -> content
      content when is_list(content) -> List.to_string(content)
      _ -> "No content available"
    end
  end

  defp process_content_token(token, socket) do
    link_regex = ~r/([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#\.]?[\w-]+)*\/?/

    case {check_regex(token, ~r/@\S[a-zA-Z]*/), check_regex(token, ~r/#\S[a-zA-Z]*/), check_regex(token, link_regex)} do
      {[[mention]], [], []} -> activate_mention_only(mention, socket)
      {[], [[hashtag]], []} -> activate_hashtag_only(hashtag, socket)
      {[], [], [[url | _rest]]} -> activate_url_only(url)
      {[[mention]], [[hashtag]], [[url | _rest]]} ->
        activate_mention_only(mention, socket)
        activate_hashtag_only(hashtag, socket)
        activate_url_only(url)
      _ -> escape_char(token)
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
    path = mention |> String.replace("@", "") |> create_user_path(socket)
    markdown = "[\ #{mention}](#{path})"
    String.replace(mention, mention, markdown)
  end

  defp activate_url_only("http" <> _rest = url), do: url
  defp activate_url_only(url) do
    path = "https://#{url}"
    "[\ #{url}](#{path})"
  end

  defp escape_char("#"), do: "\\#"
  defp escape_char(con), do: con

  defp check_regex(con, regex) do
    cond do
      con == "#" -> "#"
      con == "@" -> "@"
      true -> Regex.scan(regex, con)
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
end
