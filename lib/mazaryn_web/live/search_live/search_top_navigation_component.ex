defmodule SearchTopNavigationComponent do
  use Phoenix.LiveComponent

  @search_top_navigations ["top", "latest", "users", "groups", "pages"]

  def render(assigns) do
    ~H"""
    <div class="w-full h-16 rounded-md bg-white flex items-center">

    </div>
    """
  end
end
