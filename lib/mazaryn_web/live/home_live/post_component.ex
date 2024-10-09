defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Live.Helper
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.UserClient
  alias Core.PostClient
  alias Mazaryn.Schema.Comment
  alias Mazaryn.Posts
  alias Phoenix.LiveView.JS

  # TODO: revert to the deprecated `preload/1` if this doesn't work; I think it works
  @impl Phoenix.LiveComponent
  def update_many(list_of_assigns) do
    changeset = Comment.changeset(%Comment{})
    update_comment_changeset = Comment.changeset(%Comment{})

    Enum.map(list_of_assigns, fn {assigns, socket} ->
      assigns =
        assigns
        |> Map.put(:follow_event, follow_event(assigns.current_user.id, assigns.post.author))
        |> Map.put(:follow_text, follow_text(assigns.current_user.id, assigns.post.author))
        |> Map.put(:like_icon, like_icon(assigns.current_user.id, assigns.post.id))
        |> Map.put(:like_event, like_event(assigns.current_user.id, assigns.post.id))
        |> Map.put(:changeset, changeset)
        |> Map.put(:update_comment_changeset, update_comment_changeset)
        |> Map.put(:comments, assigns.post.comments)
        |> Map.put(:report_action, false)
        |> Map.put(:like_action, false)
        |> Map.put(:is_liked, false)

      assign(socket, assigns)
    end)
  end

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  @impl true
  def handle_event(
        "delete-post",
        %{"post-id" => post_id} = _params,
        socket
      ) do
    post_id = post_id |> to_charlist
    PostClient.delete_post(post_id)
    send(self(), :reload_posts)

    {:noreply, socket}
  end

  def handle_event(
        "delete-comment",
        %{"comment-id" => comment_id, "post-id" => post_id} = _params,
        socket
      ) do
    post_id = post_id |> to_charlist
    comment_id = comment_id |> to_charlist
    IO.inspect(comment_id, label: "comment id")
    IO.inspect(post_id, label: "post id")

    :postdb.delete_comment_from_mnesia(comment_id)

    post =
      post_id
      |> rebuild_post()

    comments = Posts.get_comment_by_post_id(post.id)

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

  def handle_event("update-comment", %{"comment" => comment_params} = _params, socket) do
    comment =
      %Comment{}
      |> Comment.update_changeset(comment_params)
      |> Posts.update_comment()

    post =
      comment.changes.post_id
      |> rebuild_post()

    IO.inspect(post, label: "post-->")
    comments = Posts.get_comment_by_post_id(post.id)

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
    IO.inspect(comment_params, label: "the comments that is beeing added")

    %Comment{}
    |> Comment.changeset(comment_params)
    |> Posts.create_comment()

    post =
      comment_params["post_id"]
      |> to_charlist
      |> rebuild_post()

    # Enum.map(post.likes, fn p ->
    #   IO.inspect(p |> Mazaryn.Schema.Comment.build(), label: "comment has been build")
    # end)

    comments = Posts.get_comment_by_post_id(post.id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments)
     |> assign(:changeset, Comment.changeset(%Comment{}))}
  end

  def handle_event("show-comments", %{"id" => post_id}, socket) do
    # get the comments by post_id
    Phoenix.LiveView.JS.toggle(to: "test")

    comments =
      Posts.get_comment_by_post_id(post_id)
      |> IO.inspect()

    {:noreply,
     socket
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

    post = rebuild_post(post_id)

    Posts.get_likes_by_post_id(post_id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:like_icon, like_icon(user_id, post_id))
     |> assign(:like_event, like_event(user_id, post_id))
     |> assign(:is_liked, true)}
  end

  def handle_event("unlike_post", %{"post-id" => post_id}, socket) do
    post_id = post_id |> to_charlist
    user_id = socket.assigns.current_user.id

    # Posts.get_likes_by_post_id(post_id)

    like =
      post_id
      |> PostClient.get_likes()
      |> Enum.map(&(&1 |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)))
      |> Enum.filter(&(&1.user_id == user_id))
      |> hd()

    PostClient.unlike_post(like.id, post_id)

    post =
      rebuild_post(post_id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:like_icon, like_icon(user_id, post_id))
     |> assign(:like_event, like_event(user_id, post_id))}
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
    case Users.one_by_username(author) do
      {:ok, user} -> Helper.handle_avatar(user)
      _ -> "/images/default-user.svg"
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
    link_regex = ~r/([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#\.]?[\w-]+)*\/?/

    post.content
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

  defp activate_url_only("http" <> _rest = url) do
    url
  end

  defp activate_url_only(url) do
    path = "https" <> ":" <> "//" <> "#{url}"
    "[\ #{url}](#{path})"
  end

  defp escape_char(con) do
    case con do
      "#" -> "\\#"
      _ -> con
    end
  end

  defp check_regex(con, regex) do
    cond do
      con == "#" ->
        "#"

      con == "@" ->
        "@"

      true ->
        regex |> Regex.scan(con)
    end
  end

  defp create_user_path(username, socket) do
    case Users.one_by_username(username) do
      :ok ->
        "#"

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

  # TODO: delete the following 2 functions if they are not used - Amos Kibet, 20th Dec, 2023
  # defp followers(username) do
  #   username
  #   |> UserClient.get_follower()
  #   |> Enum.count()
  # end

  # defp followings(username) do
  #   username
  #   |> UserClient.get_following()
  #   |> Enum.count()
  # end

  defp one_of_likes?(user_id, post_id) do
    post_id
    |> PostClient.get_likes()
    |> Enum.map(fn like ->
      like |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)
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
end
