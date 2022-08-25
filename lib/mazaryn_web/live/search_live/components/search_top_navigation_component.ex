defmodule MazarynWeb.SearchLive.Components.SearchTopNavigationComponent do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div class="w-full h-16 rounded-3xl bg-white flex items-center justify-between shadow px-6">
      <%= for navigation <- search_top_navigation_list() do %>
        <div class="py-2 px-7 rounded-md hover:bg-zinc-50 cursor-pointer" style="color: #aaaaaa;">
          <%= navigation %>
        </div>
      <% end %>
    </div>
    """
  end

  def search_top_navigation_list do
    ["top", "latest", "users", "groups", "pages"]
    |> Enum.map(fn item -> String.capitalize(item) end)
  end
end
