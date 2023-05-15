defmodule MazarynWeb.SearchLive.Components.UserComponent do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div class="flex w-full justify-between items-center my-4">
      <div class="mr-3">
        <img class="h-9 w-9 object-cover rounded-full" src={@found_user.avatar_url} />
      </div>
      <div class="flex grow flex-col">
        <div>
          <%= @found_user.username %>
        </div>
        <div class="text-slate-400 text text-sm">
          <%= @found_user.media %>
        </div>
      </div>
      <div class="flex items-center px-4 h-8 bg-blue-500 text-white rounded-md cursor-pointer">
        <svg
          xmlns="http://www.w3.org/2000/svg"
          fill="none"
          viewBox="0 0 24 24"
          strokeWidth={1.5}
          stroke="currentColor"
          class="w-5 h-5 mr-1"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            d="M12 9v6m3-3H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z"
          />
        </svg>
        <div class="text-sm">Follow</div>
      </div>
    </div>
    """
  end
end
