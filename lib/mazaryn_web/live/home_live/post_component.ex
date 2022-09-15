defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Live.Helper
  alias MazarynWeb.Component.SelectLive
  alias Home.Post
  alias Account.Users
  alias Core.UserClient

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
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

  def preload(list_of_assigns) do
    Enum.map(list_of_assigns, fn assigns ->
      assigns
      |> Map.put(:follow_event, follow_event(assigns.current_user.id, assigns.post.author))
      |> Map.put(:follow_text, follow_text(assigns.current_user.id, assigns.post.author))
    end)
  end

  defp handle_assigns(socket, user_id, username) do
    socket
    |> assign(:follow_event, follow_event(user_id, username))
    |> assign(:follow_text, follow_text(user_id, username))
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

  def get_user_avatar(author) do
    case Users.one_by_username(author) do
      {:ok, user} -> Helper.handle_avatar(user)
      _ -> "/images/default-user.png"
    end
  end
end
