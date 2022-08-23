defmodule MazarynWeb.SearchLive.Components.UserComponent do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div><%= @found_user %></div>
    """
  end
end
