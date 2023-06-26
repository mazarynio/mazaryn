defmodule MazarynWeb.HashtagLive.Index do
  use MazarynWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  def handle_params(params, _uri, socket) do
    {:noreply, socket}
  end
end
