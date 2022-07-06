defmodule MazarynWeb.UserLive.Index do
  use MazarynWeb, :live_view

  alias Core.UserClient, as: UserClient

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    {:ok, assign(socket, user_id: user_id)}
  end

  def mounth(_params, _session, socket) do
    # Get the Users from the database
    {:ok, assign(socket, :users, get_user())}
  end

  defp get_user, do: UserClient.getting_users()

  @impl true
  def handle_event("follow_user", %{"username" => username}, socket) do
    username = UserClient.getting_user(username)
    UserClient.follow(socket.assigns.user, username)

    {:noreply, assign(socket, :follow, username)}
  end

  @impl true
  def handle_event("unfollow_user", %{"username" => username}, socket) do
    username = UserClient.getting_user(username)
    UserClient.unfollow(socket.assigns.user, username)

    {:noreply, assign(socket, :unfollow, username)}
  end

  def handle_event("block_user", %{"username" => username}, socket) do
    username = UserClient.getting_user(username)
    UserClient.block(socket.assigns.user, username)

    {:noreply, assign(socket, :block, username)}
  end

  def handle_event("unblock_user", %{"username" => username}, socket) do
    username = UserClient.getting_user(username)
    UserClient.unblock(socket.assigns.user, username)

    {:noreply, assign(socket, :unblock, username)}
  end

  def handle_event("delete_user", %{"username" => username}, socket) do
    username = UserClient.getting_user(username)
    UserClient.delete_user(username)

    {:noreply, assign(socket, :delete, username)}
  end
end
