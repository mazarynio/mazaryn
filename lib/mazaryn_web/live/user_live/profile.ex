defmodule MazarynWeb.UserLive.Profile do
  use MazarynWeb, :live_view

  require Logger

  import MazarynWeb.Live.Helper
  alias Core.UserClient
  alias Account.User
  alias Account.Users
  alias Mazaryn.Schema.Post
  alias Core.PostClient

  @impl true
  def mount(
        _params,
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    {:ok, current_user} = Users.get_by_session_uuid(session_uuid)

    post_changeset = Post.changeset(%Post{})
    user_changeset = User.changeset(%User{})

    socket =
      socket
      |> assign(posts: PostClient.get_posts_by_author(current_user.username))
      |> assign(post_changeset: post_changeset)
      |> assign(user_changeset: user_changeset)
      |> assign(current_user: current_user)

    {:ok, socket}
  end

  @impl true
  def mount(
        %{"username" => username} = _params,
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    {:ok, current_user} = Users.get_by_session_uuid(session_uuid)

    post_changeset = Post.changeset(%Post{})
    user_changeset = User.changeset(%User{})

    {:ok, user} = get_user_by_username(username)

    socket =
      socket
      |> assign(post_changeset: post_changeset)
      |> assign(user_changeset: user_changeset)
      |> assign(user: user)
      |> assign(current_user: current_user)

    {:ok, socket}
  end

  @impl true
  def handle_params(%{"username" => username} = _params, _uri, socket) do
    current_user = socket.assigns.current_user
    post_changeset = Post.changeset(%Post{})
    user_changeset = User.changeset(%User{})
    posts = Mazaryn.Posts.get_posts_by_author(username)

    {:ok, user} = get_user_by_username(username)

    socket =
      socket
      |> assign(post_changeset: post_changeset)
      |> assign(user_changeset: user_changeset)
      |> assign(user: user)
      |> assign(search: nil)
      |> assign(current_user: current_user)
      |> assign(posts: posts)
      |> handle_assigns(current_user.id, username)

    {:noreply, socket}
  end

  def handle_params(_params, _uri, socket) do
    current_user = socket.assigns.current_user
    post_changeset = Post.changeset(%Post{})
    user_changeset = User.changeset(%User{})

    socket =
      socket
      |> assign(post_changeset: post_changeset)
      |> assign(user_changeset: user_changeset)
      |> assign(search: nil)
      |> assign(user: current_user)
      |> assign(current_user: current_user)

    {:noreply, socket}
  end

  @impl true
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

  def handle_event("block_user", %{"username" => username}, socket) do
    username = UserClient.get_user(username)
    UserClient.block(socket.assigns.user, username)
    {:noreply, assign(socket, :block, username)}
  end

  def handle_event("unblock_user", %{"username" => username}, socket) do
    username = UserClient.get_user(username)
    UserClient.unblock(socket.assigns.user, username)
    {:noreply, assign(socket, :unblock, username)}
  end

  def handle_event("delete_user", %{"username" => username}, socket) do
    username = UserClient.get_user(username)
    UserClient.delete_user(username)
    {:noreply, assign(socket, :delete, username)}
  end

  defp handle_assigns(socket, user_id, username) do
    socket
    |> assign(:follow_event, follow_event(user_id, username))
    |> assign(:follow_text, follow_text(user_id, username))
    |> assign(:followers, followers(user_id))
    |> assign(followings: followings(user_id))
  end

  defp get_user_by_username(username), do: Users.one_by_username(username)

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
end
