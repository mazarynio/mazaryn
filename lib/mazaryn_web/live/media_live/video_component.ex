defmodule MazarynWeb.MediaLive.VideoComponent do
  use Phoenix.LiveView

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div></div>
    """
  end
end
