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

  @impl Phoenix.LiveComponent
  def update_many(list_of_assigns) do
    IO.puts("=== UPDATE_MANY called ===")
    IO.inspect(length(list_of_assigns), label: "Number of assigns")

    unless :ets.whereis(@content_cache) != :undefined do
      :ets.new(@content_cache, [:set, :public, :named_table])
    end

    changeset = Comment.changeset(%Comment{})
    update_comment_changeset = Comment.changeset(%Comment{})
    update_post_changeset = Post.changeset(%Post{})

    Enum.map(list_of_assigns, fn {assigns, socket} ->
      try do
        IO.puts("--- Processing post ---")
        IO.inspect(assigns.post.id, label: "Post ID")

        comments_task = Task.async(fn ->
          get_comments_with_content(assigns.post.id)
        end)

        ipns_task = Task.async(fn ->
          get_post_ipns_cached(assigns.post.id)
        end)

        likes_task = Task.async(fn ->
          get_likes_count_cached(assigns.post.id)
        end)

        comments_with_replies = Task.await(comments_task, 50000)
        ipns_id = Task.await(ipns_task, 3000)
        likes_count = Task.await(likes_task, 2000)

        assigns =
          assigns
          |> Map.put(:follow_event, follow_event(assigns.current_user.id, assigns.post.author))
          |> Map.put(:follow_text, follow_text(assigns.current_user.id, assigns.post.author))
          |> Map.put(:like_icon, like_icon(assigns.current_user.id, assigns.post.id))
          |> Map.put(:like_event, like_event(assigns.current_user.id, assigns.post.id))
          |> Map.put(:changeset, changeset)
          |> Map.put(:update_comment_changeset, update_comment_changeset)
          |> Map.put(:comments, comments_with_replies)
          |> Map.put(:report_action, false)
          |> Map.put(:like_action, false)
          |> Map.put(:is_liked, false)
          |> Map.put(:update_post_changeset, update_post_changeset)
          |> Map.put(:ipns_id, ipns_id)
          |> Map.put(:likes_count, likes_count)

        assign(socket, assigns)
      rescue
        e ->
          IO.puts("Error in update_many: #{inspect(e)}")
          assign(socket, assigns |> Map.put(:comments, []) |> Map.put(:likes_count, 0))
      end
    end)
  end

  defp get_comments_with_content(post_id) do
    try do
      comments = Posts.get_comment_by_post_id(post_id)

      comments
      |> Enum.map(fn comment ->
        content = get_cached_content(:comment, comment.id) ||
                  fetch_comment_content_from_ipfs(comment.id)

        comment
        |> Map.put(:content, content)
        |> Map.put(:like_comment_event, like_comment_event_cached(comment.id))
        |> add_replies_optimized()
      end)
    rescue
      e ->
        IO.puts("Error getting comments: #{inspect(e)}")
        []
    end
  end

  defp add_replies_optimized(comment) do
    try do
      replies = :postdb.get_comment_replies(comment.id |> to_charlist)

      list_replies =
        replies
        |> Enum.take(10)
        |> Enum.map(fn reply ->
          case reply |> Mazaryn.Schema.Reply.erl_changeset() |> Mazaryn.Schema.Reply.build() do
            {:ok, built_reply} ->
              content = get_cached_content(:reply, built_reply.id) ||
                       fetch_reply_content_from_ipfs(built_reply.id)
              Map.put(built_reply, :content, content)
            {:error, _} ->
              nil
          end
        end)
        |> Enum.filter(&(&1 != nil))

      Map.put(comment, :replies, list_replies)
    rescue
      e ->
        IO.puts("Error processing replies: #{inspect(e)}")
        Map.put(comment, :replies, [])
    end
  end

  defp get_cached_content(type, id) do
    cache_key = {type, id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, content, timestamp}] ->
        if :erlang.system_time(:second) - timestamp < 300 do
          content
        else
          :ets.delete(@content_cache, cache_key)
          nil
        end
      [] -> nil
    end
  end

  defp cache_content(type, id, content) do
    cache_key = {type, id}
    timestamp = :erlang.system_time(:second)
    :ets.insert(@content_cache, {cache_key, content, timestamp})
    content
  end

  defp get_post_ipns_cached(post_id) do
    cache_key = {:ipns, post_id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, ipns, timestamp}] ->
        if :erlang.system_time(:second) - timestamp < 60 do
          ipns
        else
          :ets.delete(@content_cache, cache_key)
          get_post_ipns_fresh(post_id)
        end
      [] ->
        get_post_ipns_fresh(post_id)
    end
  end

  defp get_post_ipns_fresh(post_id) do
    case get_post_ipns(post_id) do
      nil -> nil
      ipns ->
        cache_key = {:ipns, post_id}
        timestamp = :erlang.system_time(:second)
        :ets.insert(@content_cache, {cache_key, ipns, timestamp})
        ipns
    end
  end

  defp get_likes_count_cached(post_id) do
    cache_key = {:likes_count, post_id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, count, timestamp}] ->
        if :erlang.system_time(:second) - timestamp < 30 do
          count
        else
          :ets.delete(@content_cache, cache_key)
          get_likes_count_fresh(post_id)
        end
      [] ->
        get_likes_count_fresh(post_id)
    end
  end

  defp get_likes_count_fresh(post_id) do
    count = get_likes_count(post_id)
    cache_key = {:likes_count, post_id}
    timestamp = :erlang.system_time(:second)
    :ets.insert(@content_cache, {cache_key, count, timestamp})
    count
  end

  defp like_comment_event_cached(comment_id) do
    "like-comment"
  end

  defp get_likes_count(post_id) do
    try do
      post_id
      |> to_charlist
      |> PostClient.get_likes()
      |> Enum.count()
    rescue
      _ -> 0
    end
  end

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> assign(:editing_post, false)
     |> assign(:reply_comment, false)
     |> assign(:replying_to_comment_id, nil)
     |> assign(:ipns_id, nil)
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  defp get_post_ipns(post_id) do
    case post_id do
      nil -> nil
      "" -> nil
      [] -> nil
      _ ->
        try do
          post_id_charlist = if is_binary(post_id), do: to_charlist(post_id), else: post_id
          Core.PostClient.get_ipns_from_post(post_id_charlist)
        catch
          _, _ -> nil
        rescue
          _ -> nil
        end
    end
  end

  defp fetch_comment_content_from_ipfs(comment_id) do
    cached_content = get_cached_content(:comment, comment_id)
    if cached_content do
      cached_content
    else
      try do
        content = case Core.PostClient.get_comment_content(comment_id) do
          content when is_binary(content) and content != "" -> content
          content when is_list(content) ->
            case List.to_string(content) do
              "" -> "Content unavailable"
              str -> str
            end
          _ -> "Content unavailable"
        end
        cache_content(:comment, comment_id, content)
      catch
        _, _ ->
          cache_content(:comment, comment_id, "Content unavailable")
      end
    end
  end

  defp fetch_reply_content_from_ipfs(reply_id) do
    cached_content = get_cached_content(:reply, reply_id)
    if cached_content do
      cached_content
    else
      try do
        content = case Core.PostClient.get_reply_content(reply_id) do
          content when is_binary(content) and content != "" -> content
          content when is_list(content) ->
            case List.to_string(content) do
              "" -> "Content unavailable"
              str -> str
            end
          _ -> "Content unavailable"
        end
        cache_content(:reply, reply_id, content)
      catch
        _, _ ->
          cache_content(:reply, reply_id, "Content unavailable")
      end
    end
  end

  defp clear_content_cache(type, id) do
    cache_key = {type, id}
    :ets.delete(@content_cache, cache_key)
  end

  @impl true
  def handle_event("delete-post", %{"post-id" => post_id} = _params, socket) do
    post_id = post_id |> to_charlist
    PostClient.delete_post(post_id)

    clear_content_cache(:post, post_id)
    clear_content_cache(:ipns, post_id)
    clear_content_cache(:likes_count, post_id)

    send(self(), :reload_posts)
    {:noreply, socket}
  end

  def handle_event("delete-comment", %{"comment-id" => comment_id, "post-id" => post_id} = _params, socket) do
    post_id = post_id |> to_charlist
    comment_id = comment_id |> to_charlist

    :postdb.delete_comment_from_mnesia(comment_id)

    clear_content_cache(:comment, comment_id)

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

    comment =
      %Comment{}
      |> Comment.update_changeset(comment_params)
      |> Posts.update_comment()

    post = comment.changes.post_id |> rebuild_post()

    clear_content_cache(:comment, comment.changes.id)

    comments = get_comments_with_content(post.id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments)
     |> assign(:update_comment_changeset, Comment.changeset(%Comment{}))}
  end

  def handle_event("validate-comment", %{"comment" => comment_params} = _params, socket) do
    changeset =
      %Comment{}
      |> Comment.changeset(comment_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  def handle_event("save-comment", %{"comment" => comment_params} = _params, socket) do
    IO.puts("💾 SAVE-COMMENT EVENT TRIGGERED")

    changeset = %Comment{} |> Comment.changeset(comment_params)
    comment_result = Posts.create_comment(changeset)

    post_id_charlist = comment_params["post_id"] |> to_charlist
    post = rebuild_post(post_id_charlist)

    comments = get_comments_with_content(post.id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments)
     |> assign(:changeset, Comment.changeset(%Comment{}))}
  end

  def handle_event("reply_comment_content", %{"comment" => comment_params} = _params, socket) do
    %{"comment_id" => comment_id, "content" => content} = comment_params
    user_id = socket.assigns.current_user.id

    PostClient.reply_comment(user_id, to_charlist(comment_id), content)

    post = rebuild_post(socket.assigns.post.id)
    comments = get_comments_with_content(post.id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments)}
  end

  def handle_event("delete-reply", %{"reply-id" => reply_id, "comment-id" => comment_id}, socket) do
    reply_id = reply_id |> to_charlist
    comment_id = comment_id |> to_charlist

    :postdb.delete_reply_from_mnesia(reply_id)

    # Clear reply cache
    clear_content_cache(:reply, reply_id)

    post = rebuild_post(socket.assigns.post.id)
    comments = get_comments_with_content(post.id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments)}
  end

  def handle_event("reply_comment", %{"comment-id" => comment_id}, socket) do
    {:noreply, socket |> assign(:reply_comment, true) |> assign(:replying_to_comment_id, comment_id |> to_charlist)}
  end

  def handle_event("cancel-comment-reply", _, socket) do
    {:noreply, socket |> assign(:reply_comment, false) |> assign(:replying_to_comment_id, nil)}
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

  defp like_comment_event(user_id, comment_id) do
    if one_of_comment_likes?(user_id, comment_id),
      do: "unlike-comment",
      else: "like-comment"
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
      # Blue color when liked
      do: "text-blue-500",
      # Gray color when unliked
      else: "text-gray-500"
  end

  def get_image_url(post_id) do
     cid = PostClient.get_media_cid(post_id)
     Mazaryn.config([:media, :ipfs_gateway]) <> cid
  end

end
