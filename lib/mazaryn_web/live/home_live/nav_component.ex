defmodule MazarynWeb.HomeLive.NavComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    ~H"""
    <div>
    <!-- navigation -->
        <nav class="flex justify-evenly items-center w-full bg-white relative text-center px-9 py-3 shadow-lg">
            <div class="text-xl font-extrabold text-black cursor-pointer flex-shrink-0">
                <img src={Routes.static_path(@socket, "/images/logo.png")} alt="Mazaryn Logo"/>
            </div>
            <div class="flex m-auto my-0 h-9 leading-9 w-1/2 px-10">
                <input type="search" placeholder="Search" class="w-full text-sm outline-0 rounded border-2 border-slate-200 bg-slate-100"/>
                <span class="fa fa-search"></span>
            </div>
            <ol class="hidden md:flex justify-evenly items-center list-none">
                <li class="mx-5 my-0">
                    <div class="flex justify-evenly items-center px-5 py-1 bg-slate-100 rounded-full">
                        <div class="rounded-full pr-4 flex-shrink-0">
                            <img src={Routes.static_path(@socket, "/images/mazaryn-symbol.svg")} alt="Mazaryn symbol" class="w-4 rounded-full"/>
                        </div>
                        <h1>0.23</h1>
                    </div>
                </li>
                <li class="mx-5 my-0">
                    <div class="w-8 h-8 bg-slate-100 center rounded-full p-1">
                      <span class="relative inline-block">
                            <i><%= Heroicons.icon("bell", class: "w-6 rounded-full text-gray-600 fill-gray-400") %></i>
                            <span class="absolute top-0 left-5 px-1.5 py-1 text-xs font-bold leading-none text-red-100 transform bg-red-600 rounded-full">9</span>
                        </span>
                    </div>
                </li>
                <li class="mx-5 my-0">
                    <div class="relative inline-block text-left">
                        <div>
                          <button phx-click={Phoenix.LiveView.JS.toggle(to: ".dropdown-menu", in: "fade-in-scale", out: "fade-out-scale")} type="button" class="dropdown inline-flex justify-center w-full rounded-md border border-slate-100 shadow-sm px-4 py-1.5 bg-slate-100 text-sm font-medium text-gray-700 flex-shrink-0 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-100 focus:ring-indigo-500 items-center" id="menu-button" aria-expanded="true" aria-haspopup="true">
                            <div class="rounded-full pr-6 flex-shrink-0">
                            <%= if @user.avatar_url do %>
                                <img src="https://placeimg.com/192/192/people" class="w-6 rounded-full"/>
                            <% else %>
                                <img alt="Default user" src={Routes.static_path(@socket, "/images/default-user.svg")} class="w-6 rounded-full"/>
                            <% end %>
                            <%# <img src="https://placeimg.com/192/192/people" class="w-6 rounded-full"/> %>
                            </div>
                            <%= @user.username %>
                            <i><%= Heroicons.icon("chevron-down", class: "-mr-1 ml-2 h-5 w-5") %> </i>
                          </button>
                        </div>
                        <div class="dropdown-menu hidden origin-top-right absolute right-0 mt-2 w-56 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-none"  role="menu" aria-orientation="vertical" aria-labelledby="menu-button" tabindex="-1">
                            <div class="py-1" role="none">
                              <%= link to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile),  class: "text-gray-700 block px-4 py-2 text-sm", role: "menuitem", tabindex: "-1", id: "menu-item-0" do %>
                                    <div class="flex">
                                        <div class="flex rounded-full pr-6">
                                            <%= if @user.avatar_url do %>
                                                <img src="https://placeimg.com/192/192/people" class="h-5 w-5 rounded-full"/>
                                            <% else %>
                                                <img alt="Default user" src={Routes.static_path(@socket, "/images/default-user.svg")} class="h-5 w-5 rounded-full"/>
                                            <% end %>
                                            <%# <img src="https://placeimg.com/192/192/people" class="h-5 w-5 rounded-full"/> %>
                                        </div>
                                        <div class="flex flex-col">
                                            <h1><%= @user.username %></h1>
                                            <span class="text-xs text-gray-500"><%= @user.username %></span>
                                        </div>
                                    </div>
                                  <% end %>
                            </div>
                            <div class="py-1" role="none">
                                <%= link "Log Out", to: Routes.logout_path(@socket, :index), class: "text-gray-700 block px-4 py-2 text-sm", role: "menuitem", tabindex: "-1", id: "menu-item-6"  %>
                            </div>
                        </div>
                    </div>
                </li>
            </ol>
            <!-- Mobile menu button -->
            <div class="md:hidden flex items-center">
                <button  phx-click={Phoenix.LiveView.JS.toggle(to: ".mobile-menu", in: "fade-in-scale", out: "fade-out-scale")} class="outline-none mobile-menu-button">
                    <svg
                        class="w-6 h-6 text-gray-500"
                        x-show="!showMenu"
                        fill="none"
                        stroke-linecap="round"
                        stroke-linejoin="round"
                        stroke-width="2"
                        viewBox="0 0 24 24"
                        stroke="currentColor"
                        >
                        <path d="M4 6h16M4 12h16M4 18h16"></path>
                    </svg>
                </button>
            </div>

        </nav>
        <!-- Mobile menu -->
        <div class="hidden md:hidden mobile-menu">
            <ul class="">
                <li><%= link "Coin", to: Routes.live_path(@socket, MazarynWeb.CoinLive.Index), class: "block text-sm px-2 py-2 hover:bg-slate-500 transition duration-300"%></li>
                <li><%= link "Profile", to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile), class: "block text-sm px-2 py-2 hover:bg-slate-500 transition duration-300"%></li>
                <li><%= link "Log out", to: Routes.logout_path(@socket, :index), class: "block text-sm px-2 py-4 hover:bg-slate-500 transition duration-300" %></li>
            </ul>
        </div>

    <!-- end of navigation -->
    </div>
    """
  end
end
