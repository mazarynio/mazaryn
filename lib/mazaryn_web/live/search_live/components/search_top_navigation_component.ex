defmodule MazarynWeb.SearchLive.Components.SearchTopNavigationComponent do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div class="w-full h-16 rounded-2xl bg-white flex items-center justify-between shadow px-6">
      <%= for navigation <- search_top_navigations() do %>
        <div class="py-2 px-5 rounded-md hover:bg-zinc-50" style="color: #aaaaaa;">
          <%= navigation %>
        </div>
      <% end %>
    </div>
    """
  end

  def search_top_navigations do
    ["top", "latest", "users", "groups", "pages"]
  end
end
