defmodule MazarynWeb.HomeLive.Index do
  use MazarynWeb, :live_view

  def mount(_params, session, socket) do
    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
      home
    """
  end
end
