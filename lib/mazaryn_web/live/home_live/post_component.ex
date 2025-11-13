defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  require Logger

  import MazarynWeb.Live.Helper
  alias MazarynWeb.Router.Helpers, as: Routes

  alias Account.Users
  alias Core.{UserClient, PostClient}
  alias Mazaryn.Schema.{Comment, Post}
  alias Phoenix.LiveView.JS
  alias MazarynWeb.HomeLive.{CommentHandler, IpnsManager}
  alias Home.Like
  alias Mazaryn.Translator
  alias Mazaryn.Translator.Cache, as: TranslatorCache

  alias MazarynWeb.HomeLive.PostComponent.Helper, as: PCH

  defdelegate fetch_likers(post_id_charlist), to: PCH
  defdelegate extract_user_from_like(tuple), to: PCH
  defdelegate get_user_details(user_id), to: PCH

  defdelegate like_icon(user_id, post_id), to: PCH
  defdelegate like_event(user_id, post_id), to: PCH
  defdelegate find_user_like(post_id, user_id), to: PCH
  defdelegate get_likes_count(post_id), to: PCH
  defdelegate rebuild_post(post_id), to: PCH

  defdelegate get_repost_count(post_id), to: PCH
  defdelegate has_user_reposted?(user_id, post_id), to: PCH
  defdelegate get_repost_icon(user_id, post_id), to: PCH
  defdelegate format_repost_display(post), to: PCH

  defdelegate verified?(author), to: PCH
  defdelegate one_of_comment_likes?(user_id, comment_id), to: PCH
  defdelegate comment_like_color(user_id, comment_id), to: PCH
  defdelegate get_user_avatar(author), to: PCH
  defdelegate maybe_close_likes_modal(socket), to: PCH

  defdelegate post_has_media?(post_id), to: PCH
  defdelegate get_image_url(post_id), to: PCH
  defdelegate activate_content_characters(post, socket), to: PCH

  defdelegate setup_uploads(socket), to: PCH
  defdelegate prepare_changesets(), to: PCH

  defdelegate process_posts_optimized(list_of_assigns, changesets), to: PCH
  defdelegate ensure_cache_table(), to: PCH
  defdelegate clear_content_cache(key), to: PCH
  defdelegate clear_all_cache_for_post(post_id), to: PCH

  defdelegate is_repost?(post), to: PCH
  defdelegate get_original_post(post), to: PCH
  defdelegate get_repost_comment_content(post), to: PCH

  defdelegate is_post_saved?(user_id, post_id), to: PCH
  defdelegate get_save_icon(user_id, post_id), to: PCH
  defdelegate get_save_event(user_id, post_id), to: PCH

  defdelegate like_icon(user_id, post_id), to: PCH
  defdelegate like_event(user_id, post_id), to: PCH
  defdelegate find_user_like(post_id, user_id), to: PCH
  defdelegate find_user_reaction(post_id, user_id), to: PCH
  defdelegate get_likes_count(post_id), to: PCH
  defdelegate rebuild_post(post_id), to: PCH

  @supported_translation_langs Translator.supported_targets()
  @default_src_lang Translator.default_src()

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
      all_reaction_users: %{},
      current_reaction_type: "like",
      likes_loading: false,
      likes_task: nil,
      likes_task_ref: nil,
      translated_text: nil,
      translated_lang: nil,
      supported_translation_langs: @supported_translation_langs,
      show_repost_modal: false,
      reposting_post_id: nil,
      repost_comment: "",
      repost_count: 0,
      user_has_reposted: false,
      user_reaction: nil,
      reaction_counts: %{like: 0, celebrate: 0, support: 0, love: 0, insightful: 0, funny: 0},
      total_reactions: 0
    }

    assign(socket, default_assigns)
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

  def handle_event("open_likes_modal", %{"post-id" => post_id}, socket) do
    post_id_charlist =
      case post_id do
        b when is_binary(b) -> String.to_charlist(b)
        l when is_list(l) -> l
        other -> to_charlist(to_string(other))
      end

    users =
      try do
        fetch_likers(post_id_charlist)
      rescue
        e ->
          IO.puts("❌ open_likes_modal fetch error: #{inspect(e)}")
          []
      end

    {:noreply,
     socket
     |> assign(:show_likes_modal, true)
     |> assign(:likes_loading, false)
     |> assign(:liked_users, users)}
  end

  def handle_event("close_likes_modal", _params, socket) do
    {:noreply,
     assign(socket, %{
       show_likes_modal: false,
       liked_users: [],
       all_reaction_users: %{},
       current_reaction_type: "like",
       likes_loading: false
     })}
  end

  def handle_event("view_post_details", %{"post-id" => post_id}, socket) do
    locale = socket.assigns[:locale] || "en"
    post_path = Routes.live_path(socket, MazarynWeb.PostLive.Show, locale, post_id)
    {:noreply, push_navigate(socket, to: post_path)}
  end

  def handle_event("open_repost_modal", %{"post-id" => post_id}, socket) do
    post = socket.assigns.post

    if post.author == socket.assigns.current_user.username do
      {:noreply, put_flash(socket, :error, "You cannot share your own post")}
    else
      {:noreply,
       assign(socket, %{
         show_repost_modal: true,
         reposting_post_id: post_id,
         repost_comment: ""
       })}
    end
  end

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

  def handle_event("save_post", %{"post-id" => post_id}, socket) do
    user_id = socket.assigns.current_user.id
    post_id_charlist = to_charlist(post_id)
    user_id_charlist = to_charlist(user_id)

    case Core.PostClient.save_post(user_id_charlist, post_id_charlist) do
      :ok ->
        clear_saved_posts_cache(user_id)

        if socket.assigns[:current_filter] == "interest" do
          send(self(), {:refresh_saved_posts_view})
        end

        {:noreply,
         socket
         |> assign(:is_saved, true)
         |> assign(:save_event, "unsave_post")
         |> put_flash(:info, "Post saved successfully")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to save post: #{inspect(reason)}")}
    end
  end

  def handle_event("unsave_post", %{"post-id" => post_id}, socket) do
    user_id = socket.assigns.current_user.id
    post_id_charlist = to_charlist(post_id)
    user_id_charlist = to_charlist(user_id)

    case Core.PostClient.unsave_post(user_id_charlist, post_id_charlist) do
      result when result in [:ok, {:atomic, :ok}] ->
        clear_saved_posts_cache(user_id)

        if socket.assigns[:current_filter] == "interest" do
          send(self(), {:refresh_saved_posts_view})
        end

        {:noreply,
         socket
         |> assign(:is_saved, false)
         |> assign(:save_event, "save_post")
         |> put_flash(:info, "Post unsaved successfully")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to unsave post: #{inspect(reason)}")}
    end
  end

  defp clear_saved_posts_cache(user_id) do
    try do
      cache_key = {:saved_posts, user_id}
      :ets.delete(:posts_cache, cache_key)
      Logger.info("🗑️ Cleared saved posts cache for user #{user_id}")
    rescue
      ArgumentError ->
        Logger.debug("Cache table not found, skipping cache clear")
        :ok
    end
  end

  def handle_event("clear-translation", _params, socket) do
    {:noreply, assign(socket, translated_text: nil, translated_lang: nil)}
  end

  defp select_post_plaintext(socket, _post_id, post) do
    case Map.get(socket.assigns, :post_content_cached) do
      bin when is_binary(bin) ->
        trimmed = String.trim(bin)
        if trimmed != "", do: {:ok, trimmed}, else: try_all_backends(post.id, post)

      _ ->
        try_all_backends(post.id, post)
    end
  end

  defp try_all_backends(post_id, post) do
    case PCH.get_post_content_with_timeout(post_id, 800) do
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

      function_exported?(:post_server, :get_posts_content_by_user_id, 1) and
          Map.get(post, :author_id) ->
        case :post_server.get_posts_content_by_user_id(post.author_id) do
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
            final = translated |> to_string() |> String.trim()

            if final != "" do
              TranslatorCache.put(key_id, target, final)
              {:ok, final}
            else
              {:error, :translator_failed}
            end

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  def handle_event("like_post", %{"post-id" => post_id}, socket) do
    handle_post_like_action(socket, :like, post_id)
  end

  def handle_event("unlike_post", %{"post-id" => post_id}, socket) do
    handle_post_like_action(socket, :unlike, post_id)
  end

  defp handle_post_like_action(socket, action, post_id) do
    post_id_charlist = to_charlist(post_id)
    user_id = socket.assigns.current_user.id

    case action do
      :like ->
        PostClient.like_post(user_id, post_id_charlist)

      :unlike ->
        like = find_user_like(post_id_charlist, user_id)
        PostClient.unlike_post(like.id, post_id_charlist)
    end

    clear_content_cache({:likes_count, post_id_charlist})
    post = rebuild_post(post_id_charlist)
    likes_count = get_likes_count(post_id_charlist)

    assigns = %{
      post: post,
      like_icon: like_icon(user_id, post_id_charlist),
      like_event: like_event(user_id, post_id_charlist),
      likes_count: likes_count
    }

    assigns = if action == :like, do: Map.put(assigns, :is_liked, true), else: assigns
    {:noreply, assign(socket, assigns)}
  end

  def handle_event("edit_post", %{"post-id" => post_id}, socket) do
    post = PostClient.get_post_content_by_id(to_charlist(post_id))

    {:noreply,
     socket |> assign(:editing_post, post) |> assign(:edit_post_id, to_charlist(post_id))}
  end

  def handle_event("update-post", %{"post" => post_params}, socket) do
    post_id = normalize_post_id(socket.assigns.edit_post_id)
    changeset = Post.changeset(%Post{}, post_params)

    if changeset.valid? do
      case PostClient.update_post(post_id, post_params["content"]) do
        :ok ->
          clear_all_cache_for_post(post_id)

          ipns_id = IpnsManager.get_ipns_fast(post_id)

          spawn(fn ->
            Process.sleep(3000)
            IpnsManager.ensure_ipns(post_id)
          end)

          send(self(), :reload_posts)
          {:noreply, assign(socket, %{editing_post: false, edit_post_id: nil, ipns_id: ipns_id})}

        error ->
          IO.puts("Error updating post: #{inspect(error)}")
          {:noreply, put_flash(socket, :error, "Failed to update post: #{inspect(error)}")}
      end
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

  def handle_event("open_repost_modal", %{"post-id" => post_id}, socket) do
    {:noreply,
     assign(socket, %{
       show_repost_modal: true,
       reposting_post_id: post_id,
       repost_comment: ""
     })}
  end

  def handle_event("close_repost_modal", _params, socket) do
    {:noreply,
     assign(socket, %{
       show_repost_modal: false,
       reposting_post_id: nil,
       repost_comment: ""
     })}
  end

  def handle_event("simple_repost", %{"post-id" => post_id}, socket) do
    user_id = socket.assigns.current_user.id
    post_id_charlist = to_charlist(post_id)

    user_id_charlist =
      case user_id do
        id when is_binary(id) -> String.to_charlist(id)
        id when is_list(id) -> id
        _ -> to_charlist(user_id)
      end

    case Core.RepostClient.simple_repost(user_id_charlist, post_id_charlist) do
      {:ok, _repost_id} ->
        send(self(), :reload_posts)

        {:noreply,
         socket
         |> assign(:show_repost_modal, false)
         |> put_flash(:info, "Post shared successfully!")}

      {:error, :already_reposted} ->
        {:noreply, put_flash(socket, :error, "You've already shared this post")}

      {:error, :cannot_repost_own_post} ->
        {:noreply, put_flash(socket, :error, "You cannot share your own post")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to share: #{inspect(reason)}")}
    end
  end

  def handle_event("validate_repost_comment", %{"repost" => %{"comment" => comment}}, socket) do
    {:noreply, assign(socket, :repost_comment, comment)}
  end

  def handle_event("repost_with_comment_submit", %{"repost" => %{"comment" => comment}}, socket) do
    post_id = socket.assigns.reposting_post_id
    user_id = socket.assigns.current_user.id

    if String.trim(comment) == "" do
      {:noreply, put_flash(socket, :error, "Please add your thoughts")}
    else
      post_id_charlist = to_charlist(post_id)

      user_id_charlist =
        case user_id do
          id when is_binary(id) -> String.to_charlist(id)
          id when is_list(id) -> id
          _ -> to_charlist(user_id)
        end

      comment_charlist = String.to_charlist(comment)

      case Core.RepostClient.repost_with_comment(
             user_id_charlist,
             post_id_charlist,
             comment_charlist
           ) do
        {:ok, _repost_id} ->
          send(self(), :reload_posts)

          {:noreply,
           socket
           |> assign(%{show_repost_modal: false, repost_comment: "", reposting_post_id: nil})
           |> put_flash(:info, "Post shared with your thoughts!")}

        {:error, :already_reposted} ->
          {:noreply, put_flash(socket, :error, "You've already shared this post")}

        {:error, :cannot_repost_own_post} ->
          {:noreply, put_flash(socket, :error, "You cannot share your own post")}

        {:error, reason} ->
          {:noreply, put_flash(socket, :error, "Failed to share: #{inspect(reason)}")}
      end
    end
  end

  def handle_event("confirm_unrepost", %{"post-id" => post_id}, socket) do
    user_id = socket.assigns.current_user.id

    user_id_charlist =
      case user_id do
        id when is_binary(id) -> String.to_charlist(id)
        id when is_list(id) -> id
        _ -> to_charlist(user_id)
      end

    case Core.RepostClient.unrepost(user_id_charlist, to_charlist(post_id)) do
      :ok ->
        send(self(), :reload_posts)
        {:noreply, put_flash(socket, :info, "Post unshared successfully")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to unshare: #{inspect(reason)}")}
    end
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

  defp get_modal_assigns(action) do
    base_assigns = %{
      like_action: false,
      report_action: false,
      edit_action: false,
      follower_action: false,
      follows_action: false
    }

    case action do
      "report-post" -> %{base_assigns | report_action: true}
      "like-post" -> %{base_assigns | like_action: true}
      _ -> base_assigns
    end
  end

  def handle_event("save-comment", params, socket) do
    case CommentHandler.handle_save_comment(params) do
      {:ok, %{post: post, comments: comments, changeset: changeset, flash: {type, msg}}} ->
        {:noreply,
         socket |> assign_comment_success(post, comments, changeset) |> put_flash(type, msg)}

      {:error, reason} ->
        {:noreply, handle_comment_error(socket, reason)}
    end
  end

  defp handle_comment_error(socket, reason) do
    case reason do
      :missing_post_id -> put_flash(socket, :error, "Missing post ID")
      :missing_author -> put_flash(socket, :error, "Missing author")
      {:validation, changeset} -> assign(socket, :changeset, changeset)
      {:changeset, changeset} -> assign(socket, :changeset, changeset)
      :invalid_params -> put_flash(socket, :error, "Invalid form data")
      other -> put_flash(socket, :error, "Failed to save comment: #{inspect(other)}")
    end
  end

  def handle_event("update-comment", params, socket) do
    handle_comment_operation(
      socket,
      fn ->
        CommentHandler.handle_update_comment(params, socket.assigns.post.id)
      end,
      &assign_comment_update_result/2
    )
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

  for action <- ~w(edit-comment cancel-comment-edit reply_comment cancel-comment-reply)a do
    def handle_event(unquote(to_string(action)), params, socket) do
      handle_simple_comment_action(socket, unquote(action), params)
    end
  end

  def handle_event("reply_comment_content", params, socket) do
    user_id = socket.assigns.current_user.id
    comments = socket.assigns.comments || []

    case CommentHandler.handle_reply_comment_content(params, user_id, comments) do
      {:ok, result} ->
        {:noreply,
         socket
         |> assign(:comments, result.comments)
         |> assign(:reply_comment, result.reply_comment)
         |> assign(:replying_to_comment_id, result.replying_to_comment_id)}

      {:error, %{flash: {type, msg}}} ->
        {:noreply, put_flash(socket, type, msg)}
    end
  end

  for action <- ~w(delete-comment delete-reply show-comments) do
    def handle_event(unquote(action), params, socket) do
      handle_comment_crud(socket, unquote(action), params)
    end
  end

  def handle_event("like-comment", params, socket) do
    handle_comment_like_action(socket, :like, params)
  end

  def handle_event("unlike-comment", params, socket) do
    handle_comment_like_action(socket, :unlike, params)
  end

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
      reply_comment: :handle_reply_comment,
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

    result =
      case action do
        :like -> CommentHandler.handle_like_comment(params, post_id, user_id)
        :unlike -> CommentHandler.handle_unlike_comment(params, post_id, user_id, comments)
      end

    case result do
      {:ok, %{post: post, comments: comments}} ->
        {:noreply, assign(socket, %{post: post, comments: comments})}
    end
  end

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

  defp assign_comment_update_error(socket, result) do
    socket =
      case Map.get(result, :update_comment_changeset) do
        nil -> socket
        changeset -> assign(socket, :update_comment_changeset, changeset)
      end

    socket =
      case Map.get(result, :editing_comment) do
        nil ->
          socket

        editing_comment ->
          assign(socket, %{
            editing_comment: editing_comment,
            editing_comment_id: result.editing_comment_id
          })
      end

    {flash_type, message} = result.flash
    put_flash(socket, flash_type, message)
  end

  defp assign_comment_action_result(socket, result) do
    Enum.reduce(result, socket, fn {key, value}, acc -> assign(acc, key, value) end)
  end

  def handle_info({:temp_comment_saved, temp_id, real_comment}, socket) do
    IO.puts("🔄 Replacing temp comment #{temp_id} with real comment #{real_comment.id}")

    updated_comments =
      replace_temp_comment_with_real(socket.assigns.comments, temp_id, real_comment)

    {:noreply, assign(socket, comments: updated_comments)}
  end

  def handle_info({:temp_comment_save_failed, _temp_id, _reason}, socket), do: {:noreply, socket}

  def handle_info({:comments_synced, post_id, fresh_comments}, socket) do
    if socket.assigns.post && socket.assigns.post.id == post_id do
      {:noreply, assign(socket, comments: fresh_comments)}
    else
      {:noreply, socket}
    end
  end

  def handle_info({:ipns_ready, post_id, ipns}, socket),
    do: handle_ipns_update(socket, post_id, ipns, "scheduled")

  def handle_info({:ipns_updated, post_id, ipns}, socket),
    do: handle_ipns_update(socket, post_id, ipns, "updated")

  def handle_info(:refresh_processing_content, socket) do
    comments = CommentHandler.get_comments_with_content(socket.assigns.post.id)

    processing_content =
      Enum.any?(
        comments,
        &(&1.content in ["Content is being processed...", "Content loading..."])
      )

    if processing_content, do: Process.send_after(self(), :refresh_processing_content, 3000)
    {:noreply, assign(socket, :comments, comments)}
  end

  def handle_info({:comment_content_updated, comment_id, content}, socket),
    do: handle_content_update(socket, :comment, comment_id, content)

  def handle_info({:reply_content_updated, reply_id, content}, socket),
    do: handle_content_update(socket, :reply, reply_id, content)

  def handle_info({:reply_saved, comment_id, real_reply, temp_id}, socket) do
    updated_comments =
      CommentHandler.replace_temp_reply_with_real(
        socket.assigns.comments,
        comment_id,
        temp_id,
        real_reply
      )

    {:noreply, assign(socket, :comments, updated_comments)}
  end

  def handle_info({:reply_failed, comment_id, temp_id}, socket) do
    updated_comments =
      CommentHandler.remove_temp_reply(socket.assigns.comments, comment_id, temp_id)

    {:noreply,
     assign(socket, :comments, updated_comments)
     |> put_flash(:error, "Failed to save reply. Please try again.")}
  end

  defp handle_ipns_update(socket, post_id, ipns, type) do
    if socket.assigns[:post] && to_string(socket.assigns.post.id) == to_string(post_id) do
      if type == "scheduled",
        do: IO.puts("🎉 Received scheduled IPNS update for displayed post #{post_id}")

      {:noreply, assign(socket, :ipns_id, ipns)}
    else
      {:noreply, socket}
    end
  end

  defp handle_content_update(socket, type, id, content) do
    comments = socket.assigns[:comments] || []

    updated_comments =
      case type do
        :comment -> CommentHandler.update_comment_content_in_list(comments, id, content)
        :reply -> CommentHandler.update_reply_content_in_comments(comments, id, content)
      end

    if updated_comments != comments do
      {:noreply, assign(socket, :comments, updated_comments)}
    else
      {:noreply, socket}
    end
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

  def handle_event("view_original_post", %{"post-id" => post_id}, socket) do
    locale = socket.assigns[:locale] || "en"
    post_path = Routes.live_path(socket, MazarynWeb.PostLive.Show, locale, post_id)

    {:noreply, push_navigate(socket, to: post_path)}
  end

  defp normalize_post_id(post_id),
    do: if(is_binary(post_id), do: :erlang.binary_to_list(post_id), else: post_id)

  def handle_event(
        "react_to_post",
        %{"post-id" => post_id, "reaction-type" => reaction_type},
        socket
      ) do
    try do
      post_id_charlist = to_charlist(post_id)
      user_id = socket.assigns.current_user.id
      user_id_charlist = to_charlist(user_id)
      reaction_atom = String.to_existing_atom(reaction_type)

      result = PostClient.react_to_post(user_id_charlist, post_id_charlist, reaction_atom)

      clear_content_cache({:likes_count, post_id_charlist})
      clear_content_cache({:reactions, post_id_charlist})

      Process.sleep(50)

      post = rebuild_post(post_id_charlist)

      raw_counts = PostClient.get_reaction_counts(post_id_charlist)
      reaction_counts = normalize_reaction_counts(raw_counts)

      user_reaction =
        case PostClient.get_user_reaction_type(user_id_charlist, post_id_charlist) do
          reaction when is_atom(reaction) and reaction != :undefined -> reaction
          _ -> nil
        end

      total_reactions = Map.values(reaction_counts) |> Enum.sum()

      {:noreply,
       assign(socket, %{
         post: post,
         reaction_counts: reaction_counts,
         user_reaction: user_reaction,
         total_reactions: total_reactions
       })}
    rescue
      e ->
        Logger.error("Error in react_to_post: #{inspect(e)}")
        {:noreply, put_flash(socket, :error, "Failed to react to post")}
    end
  end

  defp normalize_reaction_counts(counts) when is_map(counts) do
    IO.puts("🔧 normalize_reaction_counts input: #{inspect(counts)}")

    result = %{
      "like" => Map.get(counts, :like, Map.get(counts, "like", 0)),
      "celebrate" => Map.get(counts, :celebrate, Map.get(counts, "celebrate", 0)),
      "support" => Map.get(counts, :support, Map.get(counts, "support", 0)),
      "love" => Map.get(counts, :love, Map.get(counts, "love", 0)),
      "insightful" => Map.get(counts, :insightful, Map.get(counts, "insightful", 0)),
      "funny" => Map.get(counts, :funny, Map.get(counts, "funny", 0))
    }

    IO.puts("🔧 normalize_reaction_counts output: #{inspect(result)}")
    result
  end

  defp normalize_reaction_counts(nil) do
    IO.puts("🔧 ⚠️ normalize_reaction_counts received nil!")
    %{"like" => 0, "celebrate" => 0, "support" => 0, "love" => 0, "insightful" => 0, "funny" => 0}
  end

  defp normalize_reaction_counts(other) do
    IO.puts("🔧 ⚠️ normalize_reaction_counts received unexpected: #{inspect(other)}")
    %{"like" => 0, "celebrate" => 0, "support" => 0, "love" => 0, "insightful" => 0, "funny" => 0}
  end

  def handle_event("remove_reaction", %{"post-id" => post_id}, socket) do
    try do
      post_id_charlist = to_charlist(post_id)
      user_id = socket.assigns.current_user.id
      user_id_charlist = to_charlist(user_id)

      like = find_user_reaction(post_id_charlist, user_id)

      if like do
        PostClient.remove_reaction_from_post(like.id, post_id_charlist)
      end

      clear_content_cache({:likes_count, post_id_charlist})
      clear_content_cache({:reactions, post_id_charlist})

      Process.sleep(50)

      post = rebuild_post(post_id_charlist)

      reaction_counts =
        case PostClient.get_reaction_counts(post_id_charlist) do
          counts when is_map(counts) ->
            normalize_reaction_counts(counts)

          _ ->
            %{
              "like" => 0,
              "celebrate" => 0,
              "support" => 0,
              "love" => 0,
              "insightful" => 0,
              "funny" => 0
            }
        end

      total_reactions = Map.values(reaction_counts) |> Enum.sum()

      assigns = %{
        post: post,
        reaction_counts: reaction_counts,
        user_reaction: nil,
        total_reactions: total_reactions
      }

      {:noreply, assign(socket, assigns)}
    rescue
      e ->
        Logger.error("Error in remove_reaction: #{inspect(e)}")
        {:noreply, put_flash(socket, :error, "Failed to remove reaction")}
    end
  end

  def handle_event(
        "open_reactions_modal",
        %{"post-id" => post_id, "reaction-type" => reaction_type},
        socket
      ) do
    post_id_charlist = to_charlist(post_id)

    all_reactions_map =
      try do
        PostClient.get_all_users_by_reactions(post_id_charlist)
      rescue
        e ->
          IO.puts("❌ Error fetching all reactions: #{inspect(e)}")

          %{
            like: [],
            celebrate: [],
            support: [],
            love: [],
            insightful: [],
            funny: []
          }
      end

    grouped_users =
      Enum.into(all_reactions_map, %{}, fn {type, user_list} ->
        type_str = to_string(type)

        users =
          Enum.map(user_list, fn {username, _timestamp} ->
            username_str = if is_list(username), do: List.to_string(username), else: username

            case Users.one_by_username(username_str) do
              {:ok, user} -> user
              _ -> %{username: username_str, verified: false}
            end
          end)

        {type_str, users}
      end)

    {:noreply,
     socket
     |> assign(:show_likes_modal, true)
     |> assign(:likes_loading, false)
     |> assign(:current_reaction_type, reaction_type)
     |> assign(:all_reaction_users, grouped_users)}
  end

  defp normalize_post_id(post_id) do
    if is_binary(post_id) do
      :erlang.binary_to_list(post_id)
    else
      post_id
    end
  end

  defp extract_ipfs_hash(ipns_id) when is_binary(ipns_id) do
    ipns_id
    |> String.trim()
    |> String.replace(~r/^ipns:\/\//, "")
    |> String.replace(~r/\.ipns\.localhost.*$/, "")
    |> String.split("/")
    |> List.first()
  end

  defp extract_ipfs_hash(_), do: ""

  def handle_event(
        "react_to_post",
        %{"post-id" => post_id, "reaction-type" => reaction_type},
        socket
      ) do
    try do
      post_id_charlist = to_charlist(post_id)
      user_id = socket.assigns.current_user.id
      user_id_charlist = to_charlist(user_id)
      reaction_atom = String.to_existing_atom(reaction_type)

      result = PostClient.react_to_post(user_id_charlist, post_id_charlist, reaction_atom)

      clear_content_cache({:likes_count, post_id_charlist})
      clear_content_cache({:reactions, post_id_charlist})

      Process.sleep(50)

      post = rebuild_post(post_id_charlist)

      raw_counts = PostClient.get_reaction_counts(post_id_charlist)
      reaction_counts = normalize_reaction_counts(raw_counts)

      user_reaction =
        case PostClient.get_user_reaction_type(user_id_charlist, post_id_charlist) do
          reaction when is_atom(reaction) and reaction != :undefined -> reaction
          _ -> nil
        end

      total_reactions = Map.values(reaction_counts) |> Enum.sum()

      {:noreply,
       assign(socket, %{
         post: post,
         reaction_counts: reaction_counts,
         user_reaction: user_reaction,
         total_reactions: total_reactions
       })}
    rescue
      e ->
        Logger.error("Error in react_to_post: #{inspect(e)}")
        {:noreply, put_flash(socket, :error, "Failed to react to post")}
    end
  end
end
