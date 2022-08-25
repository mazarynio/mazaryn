defmodule MazarynWeb.SearchLive.Components.UsersComponent do
  use Phoenix.LiveComponent

  alias MazarynWeb.SearchLive.Components.UserComponent

  def render(assigns) do
    ~H"""
    <div class="w-full px-6 py-6">
      <%= for found_user <- @found_users do %>
        <.live_component module={UserComponent} found_user={found_user} id={found_user.id} />
      <% end %>
    </div>
    """
  end
end
