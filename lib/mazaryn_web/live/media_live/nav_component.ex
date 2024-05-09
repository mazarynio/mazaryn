defmodule MazarynWeb.MediaLive.NavComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Components.Icons

  @type socket :: Phoenix.LiveView.Socket.t()

  @impl Phoenix.LiveComponent
  @spec render(map()) :: Phoenix.LiveView.Rendered.t()
  def render(assigns) do
    ~H"""
    <div class="relative bg-[#151620]">
      <nav class="relative w-[90%] max-w-[1440px] mx-auto h-16 py-2.5 flex justify-evenly items-center text-center">
        <div class="w-[17%] mr-14 text-xl font-extrabold text-black cursor-pointer flex-shrink-0">
          <%= link to: Routes.live_path(@socket, MazarynWeb.HomeLive.Home), class: "block" do %>
            <Icons.logo_icon />
          <% end %>
        </div>
        <form
          class="flex items-center w-[50%] lg:w-full mx-auto my-auto h-11 outline-0 rounded-[10px] px-3.5 bg-[#323340] border-2 border-[#393A3D]"
          phx-submit="do_search"
        >
          <button type="submit">
            <%= Heroicons.icon("magnifying-glass",
              class: "magnifying_glass_icon w-5 h-5 fill-[#AAA]"
            ) %>
          </button>
          <input
            type="search"
            autocomplete="off"
            placeholder="Search"
            name="search , music, podcasts, etc."
            value={@search}
            class="p-0 ml-2 text-base leading-9 text-[#D2D2D1] caret-current placeholder:text-base placeholder:font-normal placeholder:leading-6 placeholder:text-[#AAA] outline-none border-none bg-transparent hover:ring-transparent focus:text-base focus:leading-6 focus:ring-transparent focus-within:bg-transparent focus:bg-transparent"
          />
        </form>
        <ol
          x-data="{ isopen: false }"
          x-on:click.outside="isopen = false"
          class="relative hidden w-[55%] lg:flex justify-evenly items-center list-none"
        >
          <li class="ml-5">
            <Icons.upload_icon />
          </li>
          <li class="ml-5">
            <Icons.live_stream_icon />
          </li>
          <li class="ml-28">
            <div class="flex justify-evenly items-center py-2 pl-2 pr-8 bg-[#323340] rounded-[0.625rem]">
              <div class="rounded-full pr-3 flex-shrink-0">
                <img
                  src={Routes.static_path(@socket, "/images/mazaryn-symbol.svg")}
                  alt="Mazaryn curryency symbol"
                  class="w-6 h-6 rounded-full"
                />
              </div>
              <p class="text-base text-white">0.23</p>
            </div>
          </li>
          <li class="ml-5 mr-3">
            <Icons.notifications_icon />
          </li>
          <li class="absolute right-[6rem] xl:right-[9.5rem] ml-5 -mr-8 z-50">
            <%= if @user.avatar_url do %>
              <img src={"#{@user.avatar_url}"} class="w-6 rounded-full" />
            <% else %>
              <Icons.default_user_avatar_icon />
            <% end %>
          </li>
          <li class="ml-5">
            <div @click="isopen = !isopen" class="relative z-10 inline-block text-left cursor-pointer">
              <div
                class="dropdown inline-flex justify-center w-full rounded-md border border-[#323340] shadow-sm pl-8 pr-4 py-2 bg-[#323340] text-sm font-medium text-white flex-shrink-0 hover:bg-[#4a4c5e] focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-[#323340] focus:ring-indigo-500 items-center"
                id="menu-button"
                aria-expanded="true"
                aria-haspopup="true"
              >
                <%= @user.username %>
                <i><%= Heroicons.icon("chevron-down", class: "-mr-1 ml-2 h-5 w-5") %></i>
              </div>
              <div
                x-show="isopen"
                x-transition:enter="transition ease-out duration-200"
                x-transition:enter-start="opacity-0"
                x-transition:enter-end="opacity-100"
                x-transition:leave="transition ease-in duration-200"
                x-transition:leave-start="opacity-100"
                x-transition:leave-end="opacity-0"
                class="origin-top-right absolute z-100 right-0 mt-2 w-56 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-none"
                role="menu"
                aria-orientation="vertical"
                aria-labelledby="menu-button"
                tabindex="-1"
              >
                <div class="py-1" role="none">
                  <%= live_patch to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile, @user.username),  class: "text-gray-700 block px-4 py-2 text-sm", role: "menuitem", tabindex: "-1", id: "menu-item-0" do %>
                    <div class="flex">
                      <div class="flex rounded-full pr-6">
                        <%= if @user.avatar_url do %>
                          <img src="https://placeimg.com/192/192/people" class="h-9 w-9 rounded-full" />
                        <% else %>
                          <img
                            alt="Default user"
                            src={Routes.static_path(@socket, "/images/default-user.svg")}
                            class="h-9 w-9 rounded-full"
                          />
                        <% end %>
                      </div>
                      <div class="flex flex-col">
                        <h1><%= @user.username %></h1>
                        <span class="text-xs text-gray-500"><%= @user.username %></span>
                      </div>
                    </div>
                  <% end %>
                </div>
                <div class="py-1" role="none">
                  <%= link("Log Out",
                    to: Routes.logout_path(@socket, :index),
                    class: "text-gray-700 block px-4 py-2 text-sm",
                    role: "menuitem",
                    tabindex: "-1",
                    id: "menu-item-6"
                  ) %>
                </div>
              </div>
            </div>
          </li>
        </ol>
        <!-- Mobile menu button -->
        <div class="lg:hidden flex items-center">
          <button
            phx-click={
              Phoenix.LiveView.JS.toggle(
                to: ".mobile-menu",
                in: "fade-in-scale",
                out: "fade-out-scale"
              )
            }
            class="outline-none mobile-menu-button"
          >
            <Icons.hamburger_icon />
          </button>
        </div>
      </nav>
      <!-- Mobile menu -->
      <div class="hidden mobile-menu text-white">
        <ul>
          <li>
            <%= link("Coin",
              to: Routes.live_path(@socket, MazarynWeb.CoinLive.Index),
              class: "block text-sm px-2 py-2 hover:bg-slate-500 transition duration-300"
            ) %>
          </li>
          <li>
            <%= link("Profile",
              to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile, @user.username),
              class: "block text-sm px-2 py-2 hover:bg-slate-500 transition duration-300"
            ) %>
          </li>
          <li>
            <%= link("Log out",
              to: Routes.logout_path(@socket, :index),
              class: "block text-sm px-2 py-4 hover:bg-slate-500 transition duration-300"
            ) %>
          </li>
        </ul>
      </div>
    </div>
    """
  end
end
