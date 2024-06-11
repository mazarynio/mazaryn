defmodule MazarynWeb.UserLive.Manage do
  use MazarynWeb, :live_view

  require Logger

  def mount(_params, _session, socket) do
    {:ok, assign(socket, %{})}
  end

end
