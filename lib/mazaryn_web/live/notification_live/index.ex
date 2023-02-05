defmodule MazarynWeb.NotificationLive.Index do
  use MazarynWeb, :live_view

  require Logger
  import MazarynWeb.Live.Helper

  alias Core.UserClient, as: UserClient
  alias Account.User
  alias Account.Users
  alias Mazaryn.Schema.Post
  alias Core.PostClient
  alias Mazaryn.Posts

  def handle_event("insert_notif", %{"userID" => userID, "message" => message}, socket) do
    userID = UserClient.get_user_by_id(userID)
    UserClient.insert_notif(userID, message)
    {:noreply, handle_notif(socket, userID, message)}
  end

  def handle_event("get_notif", %{"notifID" => notifID}, socket) do
    UserClient.get_single_notif(notifID)
    {:noreply, get_notification(socket, notifID)}
  end

  defp handle_notif(socket, userID, message) do
    socket
    |> assign(:notification, new_notif(userID, message))
  end

  defp get_notification(socket, notifID) do
    socket
    |> assign(:get_notif, get_notif(notifID))
  end

  defp new_notif(userID, message) do
    UserClient.insert_notif(userID, message)
  end

  defp get_notif(notifID) do
    notifID
    |> UserClient.get_single_notif()
  end
end
