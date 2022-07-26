defmodule MazarynWeb.HomeLive.LeftSidebarComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="w-full bg-white white:bg-gray-800 shadow p-4 rounded-xl border">
      <div class="flex justify-between align-center items-center py-5">
        <div class="flex justify-center items-center">
            <ul class="">
                <li class="flex align-center items-center">
                    <a href="/home" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("home", class: "h-5 w-5 mr-3") %></i>
                        Home
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500 ">
                        <i><%= Heroicons.icon("chat-alt-2", class: "h-5 w-5 mr-3") %></i>
                        Chat
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("users", class: "h-5 w-5 mr-3") %></i>
                        Group
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("cog", class: "h-5 w-5 mr-3") %></i>
                        Settings
                    </a>
                </li>
            </ul>
        </div>
      </div>
    </div>

    <div class="w-full bg-white white:bg-gray-800 shadow p-4 rounded-xl border my-5">
      <div class="flex justify-between align-center items-center py-5">
        <div class="flex justify-center items-center">
            <ul class="">
                <li class="flex align-center items-center">
                    <a href="index.html" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("shopping-bag", class: "h-5 w-5 mr-3") %></i>
                        My Products
                    </a>
                </li>
                <li class="flex align-center items-center group">
                    <a href="/profile" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold group-hover:text-blue-500">
                        <img class="w-4 mr-3 rounded-full ring-blue-500 group-hover:ring" src="https://placeimg.com/192/192/people"/>
                        @<%= @user_id %>
                    </a>
                </li>
            </ul>
        </div>
      </div>
    </div>
    <div class="w-full bg-white white:bg-gray-800 shadow p-4 rounded-xl border my-5">
      <div class="flex justify-between align-center items-center py-5">
        <div class="flex justify-center items-center">
            <ul class="">
                <li class="flex align-center items-center">
                    <a href="index.html" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("currency-dollar", class: "h-5 w-5 mr-3") %></i>
                       Offers
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("search", class: "h-5 w-5 mr-3") %></i>
                       Jobs
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("office-building", class: "h-5 w-5 mr-3") %></i>
                      Marketplace
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="block flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("flag", class: "h-5 w-5 mr-3") %></i>
                     Pages
                    </a>
                </li>
            </ul>
        </div>
      </div>
    </div>
    """
  end

  defp hover("Home", :home) do
    ""
  end

  defp hover(_, _) do
    "hover:"
  end
end
