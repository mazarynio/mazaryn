defmodule MazarynWeb.SearchLive.Components.UsersComponent do
  use Phoenix.LiveComponent

  alias MazarynWeb.SearchLive.Components.UserComponent

  def render(assigns) do
    ~H"""
    <div>
      <%= for found_user <- @found_users do %>
        <.live_component module={UserComponent} found_user={found_user} />
      <% end %>
    </div>
    """
  end
end
