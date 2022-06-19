defmodule MazarynWeb.UserLive.Index do
  use MazarynWeb, :live_view

  alias Core.UserClient, as: UserClient

  def mounth(_params, _session, socket) do
    # Get the Users from the database
    {:ok, assign(socket, :users, get_user())}
  end

  defp get_user, do: UserClient.getting_users()

  def handle_event("follow_user", %{"username" => username}, socket) do
    username = UserClient.getting_user(username)
    UserClient.follow(socket.assigns.user, username)

    {:noreply, socket}
  end

  def handle_event("unfollow_user", %{"username" => username}, socket) do
    username = UserClient.getting_user(username)
    UserClient.unfollow(socket.assigns.user, username)

    {:noreply, socket}
  end

  def handle_event("block_user", %{"username" => username}, socket) do
    username = UserClient.getting_user(username)
    UserClient.block(socket.assigns.user, username)

    {:noreply, socket}
  end

  def handle_event("unblock_user", %{"username" => username}, socket) do
    username = UserClient.getting_user(username)
    UserClient.unblock(socket.assigns.user, username)

    {:noreply, socket}
  end
end
