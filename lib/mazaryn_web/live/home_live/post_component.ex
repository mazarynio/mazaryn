defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Live.Helper
  import MazarynWeb.Live.Helper
  alias MazarynWeb.Component.SelectLive
  alias Home.Post
  alias Account.Users
  alias Account.User
  alias Core.UserClient
  alias Core.PostClient
  alias Mazaryn.Schema.Comment
  alias Mazaryn.Posts

  def preload(list_of_assigns) do
    changeset = Comment.changeset(%Comment{})
    update_comment_changeset = Comment.changeset(%Comment{})

    Enum.map(list_of_assigns, fn assigns ->
      assigns
      |> Map.put(:follow_event, follow_event(assigns.current_user.id, assigns.post.author))
      |> Map.put(:follow_text, follow_text(assigns.current_user.id, assigns.post.author))
      |> Map.put(:like_icon, like_icon(assigns.current_user.id, assigns.post.id))
      |> Map.put(:like_event, like_event(assigns.current_user.id, assigns.post.id))
      |> Map.put(:changeset, changeset)
      |> Map.put(:update_comment_changeset, update_comment_changeset)
      |> Map.put(:comments, assigns.post.comments)
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
    PostClient.delete_comment(comment_id, post_id)

    post =
      post_id
      |> rebuild_post()

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, post.comments)}
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

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, post.comments)}
  end

  def handle_event("validate-comment", %{"comment" => comment_params} = _params, socket) do
    changeset =
      %Comment{}
      |> Comment.changeset(comment_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  end

  def handle_event("save-comment", %{"comment" => comment_params} = _params, socket) do
    %Comment{}
    |> Comment.changeset(comment_params)
    |> Posts.create_comment()

    post =
      comment_params["post_id"]
      |> to_charlist
      |> rebuild_post()

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, post.comments)}
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

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:like_icon, like_icon(user_id, post_id))
     |> assign(:like_event, like_event(user_id, post_id))}
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

    post = rebuild_post(post_id)

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
      |> Enum.map(&(&1 |> elem(2) |> Account.Users.one_by_username()))

    {:noreply, assign(socket, users: users)}
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
    |> IO.inspect(label: "[[[[[[[[[[[")
  end

  defp activate_hashtag_only(hashtag, socket) do
    path = Routes.live_path(socket, MazarynWeb.HashtagLive.Index, hashtag)
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
        Routes.live_path(socket, MazarynWeb.UserLive.Profile, username)
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

  defp followers(username) do
    username
    |> UserClient.get_follower()
    |> Enum.count()
  end

  defp followings(username) do
    username
    |> UserClient.get_following()
    |> Enum.count()
  end

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
end
