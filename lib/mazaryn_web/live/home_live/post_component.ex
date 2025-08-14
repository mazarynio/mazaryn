defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  import MazarynWeb.Live.Helper

  alias Account.Users
  alias Core.{UserClient, PostClient}
  alias Mazaryn.{Posts, Schema.Comment, Schema.Post}
  alias Phoenix.LiveView.JS
  alias MazarynWeb.HomeLive.{CommentHandler, IpnsManager}
  alias Home.Like
  alias Mazaryn.Translator
  alias Mazaryn.Translator.Cache, as: TranslatorCache

  @supported_translation_langs Translator.supported_targets()
  @default_src_lang Translator.default_src()

  @content_cache :post_content_cache
  @batch_size 5
  @cache_ttl %{content: 600, likes: 300, ipns: 300}
  @timeouts %{content: 800, likes: 300, ipns: 200, comment: 2000, post_update: 1500}

  # =============================================================================
  # LIFECYCLE CALLBACKS
  # =============================================================================

  def handle_event("translate-post", %{"post-id" => post_id, "target" => target}, socket) do
    post = socket.assigns.post
    post_id = normalize_post_id(post_id)

  with true <- target in @supported_translation_langs,
       {:ok, content} <- select_post_plaintext(socket, post_id, post),
       {:ok, translated} <- translate_with_cache(post_id, content, target) do
    {:noreply, assign(socket, translated_text: translated, translated_lang: target)}
  else
    false ->
      {:noreply, put_flash(socket, :error, "Unsupported language")}
    {:error, :no_content} ->
      {:noreply, put_flash(socket, :error, "Nothing to translate")}
    {:error, :translator_failed} ->
      {:noreply, put_flash(socket, :error, "Translation failed")}
    {:error, reason} ->
      {:noreply, put_flash(socket, :error, "Translation error: #{inspect(reason)}")}
  end
end

def handle_event("clear-translation", _params, socket) do
  {:noreply, assign(socket, translated_text: nil, translated_lang: nil)}
end

defp select_post_plaintext(socket, post_id, post) do
  case Map.get(socket.assigns, :post_content_cached) do
    bin when is_binary(bin) ->
      trimmed = String.trim(bin)
      if trimmed != "", do: {:ok, trimmed}, else: try_all_backends(post_id, post)
    _ ->
      try_all_backends(post_id, post)
  end
end

defp try_all_backends(post_id, post) do
  case get_post_content_with_timeout(post_id, 800) do
    {:ok, content} when is_binary(content) ->
      trimmed = String.trim(content)
      if trimmed != "", do: {:ok, trimmed}, else: do_fallbacks(post)
    {:ok, list} when is_list(list) ->
      trimmed = list |> List.to_string() |> String.trim()
      if trimmed != "", do: {:ok, trimmed}, else: do_fallbacks(post)
    _ ->
      do_fallbacks(post)
  end
end

defp do_fallbacks(post) do
  content_from_author =
    if function_exported?(Core.PostClient, :get_post_content_by_author, 1) do
      safe_to_bin(Core.PostClient.get_post_content_by_author(to_charlist(post.author)))
    else
      nil
    end

  cond do
    is_present?(content_from_author) ->
      {:ok, content_from_author}

    function_exported?(:post_server, :get_posts_content_by_author, 1) ->
      case :post_server.get_posts_content_by_author(String.to_charlist(post.author)) do
        bin when is_binary(bin) ->
          trim_non_empty(bin)
        list when is_list(list) ->
          list
          |> Enum.map(&safe_to_bin/1)
          |> Enum.find(&is_present?/1)
          |> case do
            nil -> {:error, :no_content}
            good -> {:ok, good}
          end
        other ->
          trim_non_empty(other)
      end

    function_exported?(:post_server, :get_posts_content_by_user_id, 1) and Map.get(post, :author_id) ->
      case :post_server.get_posts_content_by_user_id(post.author_id) do
        bin when is_binary(bin) -> trim_non_empty(bin)
        list when is_list(list) ->
          list
          |> Enum.map(&safe_to_bin/1)
          |> Enum.find(&is_present?/1)
          |> case do
            nil -> {:error, :no_content}
            good -> {:ok, good}
          end
        other -> trim_non_empty(other)
      end

    true ->
      {:error, :no_content}
  end
end

defp trim_non_empty(val) do
  val = safe_to_bin(val) |> String.trim()
  if val == "", do: {:error, :no_content}, else: {:ok, val}
end

defp safe_to_bin(nil), do: ""
defp safe_to_bin(bin) when is_binary(bin), do: bin
defp safe_to_bin(list) when is_list(list), do: List.to_string(list)
defp safe_to_bin(other), do: to_string(other)

defp is_present?(str) when is_binary(str), do: String.trim(str) != ""
defp is_present?(_), do: false

defp translate_with_cache(post_id, text, target) do
  key_id =
    case post_id do
      b when is_binary(b) -> b
      l when is_list(l) -> List.to_string(l)
      other -> to_string(other)
    end

  case TranslatorCache.get(key_id, target) do
    {:hit, cached} when is_binary(cached) and cached != "" ->
      {:ok, cached}

    _miss ->
      case Translator.translate(text, @default_src_lang, target) do
        {:ok, translated} ->
          final = String.trim(translated)
          if final != "" do
            :ok = TranslatorCache.put(key_id, target, final)
            {:ok, final}
          else
            {:error, :translator_failed}
          end

        {:error, reason} ->
          {:error, reason}
      end
  end
end


defp fetch_post_plaintext(post_id) do
  case get_post_content_with_timeout(post_id, 800) do
    {:ok, content} when is_binary(content) ->
      trimmed = String.trim(content)
      if trimmed != "" do
        {:ok, trimmed}
      else
        {:error, :no_content}
      end

    {:ok, _other} ->
      {:error, :no_content}

    {:timeout, _} ->
      {:error, :timeout}

    {:error, _} ->
      {:error, :fetch_failed}
  end
end

  def handle_event("translate-post", %{"post-id" => post_id, "target" => target}, socket) do
    post_id = normalize_post_id(post_id)

    with true <- target in @supported_translation_langs,
         {:ok, content} <- fetch_post_plaintext(post_id),
         {:ok, translated} <- translate_with_cache(post_id, content, target) do
      {:noreply, assign(socket, translated_text: translated, translated_lang: target)}
    else
      false ->
        {:noreply, socket |> put_flash(:error, "Unsupported language")}
      {:error, :no_content} ->
        {:noreply, socket |> put_flash(:error, "Nothing to translate")}
      {:error, :translator_failed} ->
        {:noreply, socket |> put_flash(:error, "Translation failed")}
      _ ->
        {:noreply, socket |> put_flash(:error, "Translation error")}
    end
  end

  def handle_event("clear-translation", _params, socket) do
    {:noreply, assign(socket, translated_text: nil, translated_lang: nil)}
  end

  defp translate_with_cache(post_id, text, target) do
    case TranslatorCache.get(post_id, target) do
      {:hit, cached} when is_binary(cached) and cached != "" ->
        {:ok, cached}

      _miss ->
        case Translator.translate(text, @default_src_lang, target) do
          {:ok, translated} ->
            final = translated |> to_string() |> String.trim()
            if final != "" do
              :ok = TranslatorCache.put(post_id, target, final)
              {:ok, final}
            else
              {:error, :translator_failed}
            end

          {:error, _} ->
            {:error, :translator_failed}
        end
    end
  end


    defp assign_default_state(socket) do
    default_assigns = %{
      uploaded_files: [],
      editing_post: false,
      editing_comment: false,
      editing_comment_id: nil,
      reply_comment: false,
      replying_to_comment_id: nil,
      ipns_id: nil,
      show_likes_modal: false,
      liked_users: [],
      likes_loading: false,
      translated_text: nil,
      translated_lang: nil,
      supported_translation_langs: @supported_translation_langs
    }

    assign(socket, default_assigns)
  end

      defp build_final_assigns(assigns, results, changesets) do
    base_assigns = %{
      follow_event: follow_event(assigns.current_user.id, assigns.post.author),
      follow_text: follow_text(assigns.current_user.id, assigns.post.author),
      like_icon: like_icon(assigns.current_user.id, assigns.post.id),
      like_event: like_event(assigns.current_user.id, assigns.post.id),
      report_action: false,
      like_action: false,
      is_liked: false,
      supported_translation_langs: @supported_translation_langs
    }

    assigns
    |> Map.merge(base_assigns)
    |> Map.merge(results)
    |> Map.merge(changesets)
  end

  @impl true
  def mount(socket) do
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "post_updates")
    {:ok, socket |> assign_default_state() |> setup_uploads()}
  end

  @impl Phoenix.LiveComponent
  def update_many(list_of_assigns) do
    IO.puts("=== OPTIMIZED UPDATE_MANY called with #{length(list_of_assigns)} posts ===")

    total_start = :erlang.system_time(:millisecond)
    ensure_cache_table()
    changesets = prepare_changesets()

    result = list_of_assigns |> process_posts_optimized(changesets)

    total_duration = :erlang.system_time(:millisecond) - total_start
    IO.puts("🏁 TOTAL UPDATE_MANY completed in #{total_duration}ms")
    result
  end

  def update(%{ipns_id: ipns_id} = assigns, socket) when not is_nil(ipns_id) do
    IO.puts("🔄 PostComponent received IPNS update: #{inspect(ipns_id)}")
    updated_assigns = Map.delete(assigns, :ipns_id)
    socket = socket |> assign(:ipns_id, ipns_id) |> assign(updated_assigns)
    {:ok, socket}
  end

  def update(assigns, socket), do: {:ok, assign(socket, assigns)}

  # =============================================================================
  # EVENT HANDLERS - COMMENTS
  # =============================================================================

  def handle_event("save-comment", params, socket) do
    case CommentHandler.handle_save_comment(params) do
      {:ok, %{post: post, comments: comments, changeset: changeset, flash: {type, msg}}} ->
        {:noreply, socket |> assign_comment_success(post, comments, changeset) |> put_flash(type, msg)}
      {:error, reason} ->
        {:noreply, handle_comment_error(socket, reason)}
    end
  end

  def handle_event("update-comment", params, socket) do
    handle_comment_operation(socket, fn ->
      CommentHandler.handle_update_comment(params, socket.assigns.post.id)
    end, &assign_comment_update_result/2)
  end

  def handle_event("validate-comment", params, socket) do
    case CommentHandler.handle_validate_comment(params) do
      {:ok, %{changeset: changeset}} -> {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  def handle_event("validate-update-comment", params, socket) do
    case CommentHandler.handle_validate_update_comment(params) do
      {:ok, %{update_comment_changeset: changeset}} ->
        {:noreply, assign(socket, :update_comment_changeset, changeset)}
    end
  end

  # Batch comment event handlers
  for action <- ~w(edit-comment cancel-comment-edit reply_comment cancel-comment-reply)a do
    def handle_event(unquote(to_string(action)), params, socket) do
      handle_simple_comment_action(socket, unquote(action), params)
    end
  end

  def handle_event("reply_comment_content", params, socket) do
    user_id = socket.assigns.current_user.id
    comments = socket.assigns.comments

    case CommentHandler.handle_reply_comment_content(params, user_id, comments) do
      {:ok, result} ->
        {:noreply, socket |> assign(:comments, result.comments)
                          |> assign(:reply_comment, result.reply_comment)
                          |> assign(:replying_to_comment_id, result.replying_to_comment_id)}
      {:error, %{flash: {type, msg}}} ->
        {:noreply, put_flash(socket, type, msg)}
    end
  end

  # Batch comment CRUD handlers
  for action <- ~w(delete-comment delete-reply show-comments)a do
    def handle_event(unquote(to_string(action)), params, socket) do
      handle_comment_crud(socket, unquote(action), params)
    end
  end

  # Comment like handlers
  def handle_event("like-comment", params, socket) do
    handle_comment_like_action(socket, :like, params)
  end

  def handle_event("unlike-comment", params, socket) do
    handle_comment_like_action(socket, :unlike, params)
  end

  # =============================================================================
  # EVENT HANDLERS - POSTS & USERS
  # =============================================================================

  def handle_event("follow_user", %{"username" => username}, socket) do
    handle_user_follow_action(socket, :follow, username)
  end

  def handle_event("unfollow_user", %{"username" => username}, socket) do
    handle_user_follow_action(socket, :unfollow, username)
  end

  def handle_event("like_post", %{"post-id" => post_id}, socket) do
    handle_post_like_action(socket, :like, post_id)
  end

  def handle_event("unlike_post", %{"post-id" => post_id}, socket) do
    handle_post_like_action(socket, :unlike, post_id)
  end

  # Post editing handlers
  def handle_event("edit_post", %{"post-id" => post_id}, socket) do
    post = PostClient.get_post_content_by_id(to_charlist(post_id))
    {:noreply, socket |> assign(:editing_post, post) |> assign(:edit_post_id, to_charlist(post_id))}
  end

  def handle_event("update-post", %{"post" => post_params}, socket) do
    post_id = normalize_post_id(socket.assigns.edit_post_id)
    changeset = Post.changeset(%Post{}, post_params)

    if changeset.valid? do
      handle_valid_post_update(socket, post_id, post_params["content"])
    else
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

  # =============================================================================
  # EVENT HANDLERS - MODALS & UI
  # =============================================================================

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
    {:noreply, assign(socket, %{show_likes_modal: false, liked_users: [], likes_loading: false})}
  end

  def handle_event("view_profile", %{"username" => username}, socket) do
    locale = socket.assigns[:locale] || "en"
    profile_path = Routes.live_path(socket, MazarynWeb.UserLive.Profile, locale, username)

    socket = maybe_close_likes_modal(socket)
    {:noreply, push_navigate(socket, to: profile_path)}
  end

  def handle_event("open_modal", %{"action" => action}, socket) do
    {:noreply, assign(socket, get_modal_assigns(action))}
  end

  def handle_event("close_modal", _params, socket) do
    {:noreply, assign(socket, :users, [])}
  end

  # =============================================================================
  # INFO HANDLERS
  # =============================================================================

  # Temp comment handlers
  def handle_info({:temp_comment_saved, temp_id, real_comment}, socket) do
    IO.puts("🔄 Replacing temp comment #{temp_id} with real comment #{real_comment.id}")
    updated_comments = replace_temp_comment_with_real(socket.assigns.comments, temp_id, real_comment)
    {:noreply, assign(socket, comments: updated_comments)}
  end

  def handle_info({:temp_comment_save_failed, temp_id, reason}, socket) do
    IO.puts("❌ Temp comment #{temp_id} save failed: #{inspect(reason)}")
    {:noreply, socket}
  end

  # Content sync handlers
  def handle_info({:comments_synced, post_id, fresh_comments}, socket) do
    if socket.assigns.post && socket.assigns.post.id == post_id do
      {:noreply, assign(socket, comments: fresh_comments)}
    else
      {:noreply, socket}
    end
  end

  # IPNS handlers
  def handle_info({:ipns_ready, post_id, ipns}, socket) do
    handle_ipns_update(socket, post_id, ipns, "scheduled")
  end

  def handle_info({:ipns_updated, post_id, ipns}, socket) do
    handle_ipns_update(socket, post_id, ipns, "updated")
  end

  # Likes handlers
  def handle_info({:likes_fetched, users}, socket) do
    {:noreply, assign(socket, %{liked_users: users, likes_loading: false})}
  end

  def handle_info({:likes_fetch_error, _error}, socket) do
    {:noreply, assign(socket, %{liked_users: [], likes_loading: false})
              |> put_flash(:error, "Failed to load likes")}
  end

  # Content refresh handlers
  def handle_info(:refresh_processing_content, socket) do
    comments = CommentHandler.get_comments_with_content(socket.assigns.post.id)
    processing_content = Enum.any?(comments, &(&1.content in ["Content is being processed...", "Content loading..."]))

    if processing_content do
      Process.send_after(self(), :refresh_processing_content, 3000)
    end

    {:noreply, assign(socket, :comments, comments)}
  end

  # Content update handlers
  def handle_info({:comment_content_updated, comment_id, content}, socket) do
    handle_content_update(socket, :comment, comment_id, content)
  end

  def handle_info({:reply_content_updated, reply_id, content}, socket) do
    handle_content_update(socket, :reply, reply_id, content)
  end

  # Reply handlers
  def handle_info({:reply_saved, comment_id, real_reply, temp_id}, socket) do
    updated_comments = CommentHandler.replace_temp_reply_with_real(socket.assigns.comments, comment_id, temp_id, real_reply)
    {:noreply, assign(socket, :comments, updated_comments)}
  end

  def handle_info({:reply_failed, comment_id, temp_id}, socket) do
    updated_comments = CommentHandler.remove_temp_reply(socket.assigns.comments, comment_id, temp_id)
    {:noreply, assign(socket, :comments, updated_comments) |> put_flash(:error, "Failed to save reply. Please try again.")}
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  # =============================================================================
  # PUBLIC FUNCTIONS
  # =============================================================================

  def warm_cache_for_recent_posts(recent_post_ids) when is_list(recent_post_ids) do
    IpnsManager.warm_cache_async(recent_post_ids)
    warm_likes_cache(recent_post_ids)
  end

  def get_user_avatar(author) do
    case Account.Users.one_by_username(author) do
      {:ok, user} ->
        if user.avatar_url do
          Mazaryn.config([:media, :ipfs_gateway]) <> user.avatar_url
        else
          ~p"/images/default-user.svg"
        end
      {:error, _} -> ""
    end
  end

  def activate_content_characters(post, socket) do
    process_start = :erlang.system_time(:millisecond)
    IO.puts("📝 Starting content processing for post #{post.id}")

    result = try do
      content_str = get_processed_content(post)
      process_content_string(content_str, socket)
    catch
      type, reason ->
        IO.puts("❌ Unexpected error in content processing: #{inspect({type, reason})}")
        "Error processing content" |> Earmark.as_html!(compact_output: true) |> apply_styles()
    end

    process_end = :erlang.system_time(:millisecond)
    IO.puts("📝 Content processing completed in #{process_end - process_start}ms")
    result
  end

  def get_image_url(post_id) do
    cid = PostClient.get_media_cid(post_id)
    Mazaryn.config([:media, :ipfs_gateway]) <> cid
  end

  # =============================================================================
  # PRIVATE HELPER FUNCTIONS
  # =============================================================================

  # Initial setup helpers
  defp assign_default_state(socket) do
    default_assigns = %{
      uploaded_files: [], editing_post: false, editing_comment: false,
      editing_comment_id: nil, reply_comment: false, replying_to_comment_id: nil,
      ipns_id: nil, show_likes_modal: false, liked_users: [], likes_loading: false
    }
    assign(socket, default_assigns)
  end

  defp setup_uploads(socket) do
    allow_upload(socket, :media, accept: ~w(.png .jpg .jpeg), max_entries: 2)
  end

  defp prepare_changesets do
    %{
      changeset: Comment.changeset(%Comment{}),
      update_comment_changeset: Comment.changeset(%Comment{}),
      update_post_changeset: Post.changeset(%Post{})
    }
  end

  # Post processing helpers
  defp process_posts_optimized(list_of_assigns, changesets) do
    list_of_assigns
    |> Enum.chunk_every(@batch_size)
    |> Enum.flat_map(&process_batch(&1, changesets))
  end

  defp process_batch(batch, changesets) do
    IO.puts("Processing batch of #{length(batch)} posts")

    batch
    |> Task.async_stream(&process_single_post(&1, changesets),
                        max_concurrency: 4, timeout: 5000, on_timeout: :kill_task)
    |> Enum.map(&handle_task_result(&1, batch))
  end

  defp handle_task_result({:ok, result}, _batch), do: result
  defp handle_task_result({:exit, reason}, batch) do
    IO.puts("❌ Task failed with reason: #{inspect(reason)}")
    {assigns, socket} = hd(batch)
    assign(socket, basic_assigns(assigns))
  end

  defp process_single_post({assigns, socket}, changesets) do
    try do
      post_start = :erlang.system_time(:millisecond)
      IO.puts("--- Processing post #{assigns.post.id} ---")

      tasks = create_async_tasks(assigns.post.id)
      results = await_tasks_with_fallbacks(tasks, assigns.post.id)
      final_assigns = build_final_assigns(assigns, results, changesets)

      post_end = :erlang.system_time(:millisecond)
      IO.puts("✅ Post #{assigns.post.id} completed in #{post_end - post_start}ms")

      assign(socket, final_assigns)
    rescue
      e ->
        IO.puts("❌ Error processing post #{assigns.post.id}: #{inspect(e)}")
        assign(socket, basic_assigns(assigns))
    end
  end

  # Async task management
  defp create_async_tasks(post_id) do
    task_definitions = [
      {:comments, fn -> CommentHandler.get_comments_with_content_optimized(post_id) end},
      {:ipns, fn -> IpnsManager.get_ipns_fast(post_id) end},
      {:likes, fn -> get_likes_count_cached(post_id) end},
      {:content, fn -> get_post_content_optimized(post_id) end}
    ]

    Enum.into(task_definitions, %{}, fn {name, func} ->
      {name, Task.async(fn ->
        start = :erlang.system_time(:millisecond)
        result = func.()
        duration = :erlang.system_time(:millisecond) - start
        IO.puts("#{task_emoji(name)} #{String.capitalize(to_string(name))} fetch for post #{post_id} took #{duration}ms")
        result
      end)}
    end)
  end

  defp task_emoji(:comments), do: "📝"
  defp task_emoji(:ipns), do: "🔗"
  defp task_emoji(:likes), do: "📊"
  defp task_emoji(:content), do: "📄"

  defp await_tasks_with_fallbacks(tasks, post_id) do
    fallbacks = %{comments: [], ipns_id: nil, likes_count: 0, post_content_cached: "Content loading..."}
    timeouts = %{comments: 2000, ipns_id: 200, likes_count: 500, post_content_cached: 1500}

    Enum.into(tasks, %{}, fn {key, task} ->
      timeout = Map.get(timeouts, key, 1000)
      fallback = Map.get(fallbacks, key, nil)
      result_key = if key == :ipns, do: :ipns_id, else: key
      result_key = if key == :likes, do: :likes_count, else: result_key
      result_key = if key == :content, do: :post_content_cached, else: result_key

      {result_key, await_with_fallback(task, timeout, fallback, to_string(key), post_id)}
    end)
  end

  defp build_final_assigns(assigns, results, changesets) do
    base_assigns = %{
      follow_event: follow_event(assigns.current_user.id, assigns.post.author),
      follow_text: follow_text(assigns.current_user.id, assigns.post.author),
      like_icon: like_icon(assigns.current_user.id, assigns.post.id),
      like_event: like_event(assigns.current_user.id, assigns.post.id),
      report_action: false, like_action: false, is_liked: false
    }

    assigns |> Map.merge(base_assigns) |> Map.merge(results) |> Map.merge(changesets)
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
      comments: [], report_action: false, like_action: false, is_liked: false,
      ipns_id: nil, likes_count: 0, post_content_cached: "Content loading..."
    }
  end

  # Event handler helpers
  defp handle_comment_operation(socket, operation_func, result_handler) do
    case operation_func.() do
      {:ok, result} -> {:noreply, result_handler.(socket, result)}
      {:error, result} -> {:noreply, assign_comment_update_error(socket, result)}
    end
  end

  defp handle_simple_comment_action(socket, action, params) do
    action_map = %{
      "edit-comment": :handle_edit_comment,
      "cancel-comment-edit": :handle_cancel_comment_edit,
      "reply_comment": :handle_reply_comment,
      "cancel-comment-reply": :handle_cancel_comment_reply
    }

    handler = Map.get(action_map, action)
    case apply(CommentHandler, handler, [params]) do
      {:ok, result} -> {:noreply, assign_comment_action_result(socket, result)}
    end
  end

  defp handle_comment_crud(socket, action, params) do
    comments = socket.assigns.comments

    case action do
      "delete-comment" ->
        {:ok, %{comments: updated}} = CommentHandler.handle_delete_comment(params, comments)
        {:noreply, assign(socket, :comments, updated)}
      "delete-reply" ->
        {:ok, %{comments: updated}} = CommentHandler.handle_delete_reply(params, comments)
        {:noreply, assign(socket, :comments, updated)}
      "show-comments" ->
        {:ok, %{comments: updated}} = CommentHandler.handle_show_comments(params)
        {:noreply, assign(socket, :comments, updated)}
    end
  end

  defp handle_comment_like_action(socket, action, params) do
    post_id = socket.assigns.post.id
    user_id = socket.assigns.current_user.id
    comments = socket.assigns.comments

    result = case action do
      :like -> CommentHandler.handle_like_comment(params, post_id, user_id)
      :unlike -> CommentHandler.handle_unlike_comment(params, post_id, user_id, comments)
    end

    case result do
      {:ok, %{post: post, comments: comments}} ->
        {:noreply, assign(socket, %{post: post, comments: comments})}
    end
  end

  defp handle_user_follow_action(socket, action, username) do
    user_id = socket.assigns.current_user.id

    case action do
      :follow -> UserClient.follow(user_id, username)
      :unfollow -> UserClient.unfollow(user_id, username)
    end

    {:noreply, update_follow_assigns(socket, user_id, username)}
  end

  defp handle_post_like_action(socket, action, post_id) do
    post_id = to_charlist(post_id)
    user_id = socket.assigns.current_user.id

    case action do
      :like -> PostClient.like_post(user_id, post_id)
      :unlike ->
        like = find_user_like(post_id, user_id)
        PostClient.unlike_post(like.id, post_id)
    end

    clear_content_cache(:likes_count, post_id)
    post = rebuild_post(post_id)
    likes_count = get_likes_count(post_id)

    assigns = %{
      post: post,
      like_icon: like_icon(user_id, post_id),
      like_event: like_event(user_id, post_id),
      likes_count: likes_count
    }

    assigns = if action == :like, do: Map.put(assigns, :is_liked, true), else: assigns
    {:noreply, assign(socket, assigns)}
  end

  # Assignment helpers
  defp assign_comment_success(socket, post, comments, changeset) do
    assign(socket, %{post: post, comments: comments, changeset: changeset})
  end

  defp assign_comment_update_result(socket, result) do
    socket
    |> assign(:comments, result.comments)
    |> assign(:editing_comment, result.editing_comment)
    |> assign(:editing_comment_id, result.editing_comment_id)
    |> assign(:update_comment_changeset, result.update_comment_changeset)
    |> put_flash(elem(result.flash, 0), elem(result.flash, 1))
  end

  defp assign_comment_action_result(socket, result) do
    Enum.reduce(result, socket, fn {key, value}, acc ->
      assign(acc, key, value)
    end)
  end

  defp handle_comment_error(socket, reason) do
    case reason do
      :missing_post_id -> put_flash(socket, :error, "Missing post ID")
      :missing_author -> put_flash(socket, :error, "Missing author")
      {:validation, changeset} -> assign(socket, :changeset, changeset)
      {:changeset, changeset} -> assign(socket, :changeset, changeset)
      :invalid_params -> put_flash(socket, :error, "Invalid form data")
      _ -> put_flash(socket, :error, "Failed to save comment")
    end
  end

  defp assign_comment_update_error(socket, result) do
    socket = case Map.get(result, :update_comment_changeset) do
      nil -> socket
      changeset -> assign(socket, :update_comment_changeset, changeset)
    end

    socket = case Map.get(result, :editing_comment) do
      nil -> socket
      editing_comment -> assign(socket, %{editing_comment: editing_comment, editing_comment_id: result.editing_comment_id})
    end

    {flash_type, message} = result.flash
    put_flash(socket, flash_type, message)
  end

  # Content processing helpers
  defp process_content_string(content_str, socket) do
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
  end

  defp get_processed_content(post) do
    case Core.PostClient.get_post_content_by_id(post.id) do
      nil -> process_post_content(post.content)
      ipfs_content -> process_ipfs_content(ipfs_content)
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
    patterns = %{
      mention: ~r/@\S[a-zA-Z]*/,
      hashtag: ~r/#\S[a-zA-Z]*/,
      url: ~r/([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#\.]?[\w-]+)*\/?/
    }

    matches = Enum.into(patterns, %{}, fn {type, regex} ->
      {type, check_regex(token, regex)}
    end)

    case matches do
      %{mention: [[mention]], hashtag: [], url: []} -> activate_mention_only(mention, socket)
      %{mention: [], hashtag: [[hashtag]], url: []} -> activate_hashtag_only(hashtag, socket)
      %{mention: [], hashtag: [], url: [[url | _]]} -> activate_url_only(url)
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

  # Info handler helpers
  defp handle_ipns_update(socket, post_id, ipns, type) do
    if socket.assigns[:post] && to_string(socket.assigns.post.id) == to_string(post_id) do
      if type == "scheduled" do
        IO.puts("🎉 Received scheduled IPNS update for displayed post #{post_id}")
      end
      {:noreply, assign(socket, :ipns_id, ipns)}
    else
      {:noreply, socket}
    end
  end

  defp handle_content_update(socket, type, id, content) do
    comments = socket.assigns[:comments] || []

    updated_comments = case type do
      :comment -> CommentHandler.update_comment_content_in_list(comments, id, content)
      :reply -> CommentHandler.update_reply_content_in_comments(comments, id, content)
    end

    if updated_comments != comments do
      {:noreply, assign(socket, :comments, updated_comments)}
    else
      {:noreply, socket}
    end
  end

  defp maybe_close_likes_modal(socket) do
    if socket.assigns[:show_likes_modal] do
      assign(socket, %{show_likes_modal: false, liked_users: [], likes_loading: false})
    else
      socket
    end
  end

  # Post update helpers
  defp handle_valid_post_update(socket, post_id, new_content) do
    case PostClient.update_post(post_id, new_content) do
      :ok ->
        clear_all_cache_for_post(post_id)

        try do
          retrieved_post = rebuild_post(post_id)
          ipns_id = IpnsManager.get_ipns_fast(post_id)
          send(self(), :reload_posts)

          {:noreply, assign(socket, %{editing_post: false, edit_post_id: nil, ipns_id: ipns_id})}
        catch
          _, e ->
            IO.puts("Error retrieving post after update: #{inspect(e)}")
            {:noreply, assign(socket, %{editing_post: false, edit_post_id: nil})}
        end

      error ->
        IO.puts("Error updating post: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to update post: #{inspect(error)}")}
    end
  end

  defp normalize_post_id(post_id) do
    if is_binary(post_id), do: :erlang.binary_to_list(post_id), else: post_id
  end

  defp update_follow_assigns(socket, user_id, username) do
    assign(socket, %{
      follow_event: follow_event(user_id, username),
      follow_text: follow_text(user_id, username)
    })
  end

  # Modal helpers
  defp get_modal_assigns(action) do
    base_assigns = %{like_action: false, report_action: false, edit_action: false,
                     follower_action: false, follows_action: false}

    case action do
      "report-post" -> %{base_assigns | report_action: true}
      "like-post" -> %{base_assigns | like_action: true}
      _ -> base_assigns
    end
  end

  # =============================================================================
  # CACHE MANAGEMENT
  # =============================================================================

  defp ensure_cache_table do
    unless :ets.whereis(@content_cache) != :undefined do
      :ets.new(@content_cache, [:set, :public, :named_table, {:read_concurrency, true}])
    end
  end

  defp get_post_content_optimized(post_id) do
    cache_key = {:post_content, post_id}

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, content, timestamp}] ->
        if cache_valid?(timestamp, @cache_ttl.content) do
          IO.puts("📦 Content Cache HIT for post #{post_id}")
          content
        else
          spawn_content_refresh(post_id)
          content
        end
      [] ->
        case get_post_content_with_timeout(post_id, @timeouts.content) do
          {:ok, content} ->
            cache_content(cache_key, content)
            content
          _ ->
            "Content loading..."
        end
    end
  end

  defp get_likes_count_cached(post_id) do
    cache_key = {:likes_count, post_id}

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, count, timestamp}] ->
        if cache_valid?(timestamp, @cache_ttl.likes) do
          IO.puts("📦 Likes Cache HIT for post #{post_id}")
          count
        else
          spawn_likes_refresh(post_id)
          count
        end
      [] ->
        case get_likes_count_with_timeout(post_id, @timeouts.likes) do
          count when is_integer(count) ->
            cache_likes_count(cache_key, count)
            count
          _ -> 0
        end
    end
  end

  defp cache_valid?(timestamp, ttl_seconds) do
    age = :erlang.system_time(:second) - timestamp
    age < ttl_seconds
  end

  defp cache_content(cache_key, content) do
    timestamp = :erlang.system_time(:second)
    :ets.insert(@content_cache, {cache_key, content, timestamp})
  end

  defp cache_likes_count(cache_key, count) do
    timestamp = :erlang.system_time(:second)
    :ets.insert(@content_cache, {cache_key, count, timestamp})
  end

  defp spawn_content_refresh(post_id) do
    Task.start(fn ->
      case get_post_content_with_timeout(post_id, @timeouts.post_update) do
        {:ok, content} -> cache_content({:post_content, post_id}, content)
        _ -> :ok
      end
    end)
  end

  defp spawn_likes_refresh(post_id) do
    Task.start(fn ->
      try do
        count = get_likes_count(post_id)
        cache_likes_count({:likes_count, post_id}, count)
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

  defp clear_content_cache(type, id) do
    cache_key = {type, id}
    :ets.delete(@content_cache, cache_key)
  end

  defp clear_all_cache_for_post(post_id) do
    [:post, :ipns, :likes_count, :post_content]
    |> Enum.each(&clear_content_cache(&1, post_id))
    IpnsManager.clear_cache(post_id)
  end

  # =============================================================================
  # DATA FETCHING WITH TIMEOUTS
  # =============================================================================

  defp get_post_content_with_timeout(post_id, timeout) do
    task = Task.async(fn ->
      try do
        case Core.PostClient.get_post_content_by_id(post_id) do
          content when is_binary(content) and content != "" -> {:ok, content}
          content when is_list(content) ->
            case List.to_string(content) do
              "" -> {:ok, "No content available"}
              str -> {:ok, str}
            end
          _ -> {:ok, "No content available"}
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

  # =============================================================================
  # ASYNC OPERATIONS
  # =============================================================================

  defp load_liked_users_async(post_id, parent_pid) do
    Task.start(fn ->
      try do
        post_id_charlist = if is_binary(post_id), do: to_charlist(post_id), else: post_id

        users = post_id_charlist
        |> PostClient.get_likes()
        |> Enum.map(&extract_user_from_like/1)
        |> Enum.filter(&(&1 != nil))
        |> Enum.map(&get_user_details/1)
        |> Enum.filter(&(&1 != nil))

        send(parent_pid, {:likes_fetched, users})
      rescue
        e -> send(parent_pid, {:likes_fetch_error, e})
      end
    end)
  end

  defp extract_user_from_like(like) do
    case Like.erl_changeset(like) |> Like.build() do
      {:ok, like_struct} -> like_struct.user_id
      _ -> nil
    end
  end

  defp get_user_details(user_id) do
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
  end

  # =============================================================================
  # UTILITY FUNCTIONS
  # =============================================================================

  defp find_user_like(post_id, user_id) do
    post_id
    |> PostClient.get_likes()
    |> Enum.map(&(&1 |> Like.erl_changeset() |> Like.build() |> elem(1)))
    |> Enum.find(&(&1.user_id == user_id))
  end

  defp rebuild_post(post_id) do
    {:ok, post} = PostClient.get_by_id(post_id)
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

  # =============================================================================
  # USER INTERACTION HELPERS
  # =============================================================================

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
end
