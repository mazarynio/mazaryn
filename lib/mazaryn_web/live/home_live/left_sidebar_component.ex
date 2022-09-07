defmodule MazarynWeb.HomeLive.LeftSidebarComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    ~H"""
    <div>
    <div class="w-full bg-white white:bg-gray-800 shadow p-4 rounded-xl border">
      <div class="flex justify-between align-center items-center py-5">
        <div class="flex justify-center items-center">
            <ul class="">
                <%= live_redirect to: Routes.live_path(@socket, MazarynWeb.HomeLive.Home), replace: false, class: "flex align-center items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500" do %>
                    <i><%= Heroicons.icon("home", class: "h-5 w-5 mr-3") %></i>
                    Home
                    <% end %>

                <li class="flex align-center items-center">
                    <a href="index.html" class="flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500 ">
                        <i><%= Heroicons.icon("chat-alt-2", class: "h-5 w-5 mr-3") %></i>
                        Chat
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("users", class: "h-5 w-5 mr-3") %></i>
                        Group
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
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
                    <a href="index.html" class="flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("shopping-bag", class: "h-5 w-5 mr-3") %></i>
                        My Products
                    </a>
                </li>
                <li class="flex align-center items-center group">
                    <%= live_patch to: Routes.live_path(@socket, MazarynWeb.DashboardLive.Index), replace: false, class: "flex items-center text-1 px-2 py-4 text-gray-500 font-semibold group-hover:text-blue-500" do %>
                        Dashboard
                    <% end %>
                </li>
                <li class="flex align-center items-center group">
                    <%= live_patch to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile), replace: false, class: "flex items-center text-l px-2 py-4 text-gray-500 font-semibold group-hover:text-blue-500" do %>
                        <%= if @user.avatar_url do %>
                            <img src="https://placeimg.com/192/192/people" class="w-4 mr-3 rounded-full ring-blue-500 group-hover:ring"/>
                        <% else %>
                            <img alt="Default user" src={Routes.static_path(@socket, "/images/default-user.svg")} class="w-4 mr-3 rounded-full ring-blue-500 group-hover:ring"/>
                        <% end %>

                        @<%= @user.username %>
                    <% end %>

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
                    <a href="index.html" class="flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("currency-dollar", class: "h-5 w-5 mr-3") %></i>
                       Offers
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("search", class: "h-5 w-5 mr-3") %></i>
                       Jobs
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("office-building", class: "h-5 w-5 mr-3") %></i>
                      Marketplace
                    </a>
                </li>
                <li class="flex align-center items-center">
                    <a href="index.html" class="flex items-center text-l px-2 py-4 text-gray-500 font-semibold hover:text-blue-500">
                        <i><%= Heroicons.icon("flag", class: "h-5 w-5 mr-3") %></i>
                     Pages
                    </a>
                </li>
            </ul>
        </div>
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
