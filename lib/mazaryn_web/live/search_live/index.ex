defmodule MazarynWeb.SearchLive.Index do
  use MazarynWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, assign(socket, nil)}
  end
end
