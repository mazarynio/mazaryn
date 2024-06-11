defmodule MazarynWeb.UserLive.Manage do
  use MazarynWeb, :live_view

  require Logger

  alias Account.User
  alias Account.Users

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    {:ok, current_user} = Users.get_by_session_uuid(session_uuid)

    socket =
      socket
      |> assign(title: "Manage Users")
      |> assign(session_uuid: session_uuid)
      |> assign(current_user: current_user)
      |> assign(search: nil)



    {:ok, assign(socket, %{})}
  end

end
