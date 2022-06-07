defmodule MazarynWeb.UserLiveAuth do
  import Phoenix.LiveView

  def on_mount(:default, _params, _session, socket) do
    user = %{}

    socket = assign_new(socket, :current_user, fn -> user end)

    if socket.assigns.current_user do
      {:cont, socket}
    else
      {:halt, redirect(socket, to: "/login")}
    end
  end
end
