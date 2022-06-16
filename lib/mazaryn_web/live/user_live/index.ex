defmodule MazarynWeb.UserLive.Index do
  use MazarynWeb, :live_view

  alias Core.UserClient, as: UserClient

  def mounth(_params, _session, socket) do
    # Get the Users from the database
    {:ok, assign(socket, :users, get_user())}
  end

  defp get_user, do: UserClient.getting_users()

  def handle_event("follow_user", %{"to-user" => username}, socket) do
    to_user = UserClient.getting_user(username)
    UserClient.follow(socket.assigns.user, to_user)

    {:noreply, socket}
  end

  def handle_event("unfollow_user", %{"to-user" => username}, socket) do
    to_user = UserClient.getting_user(username)
    UserClient.unfollow(socket.assigns.user, to_user)

    {:noreply, socket}
  end
end
