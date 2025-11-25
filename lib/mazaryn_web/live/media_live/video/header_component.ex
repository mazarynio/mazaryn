defmodule MazarynWeb.MediaLive.Video.HeaderComponent do
  use MazarynWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <header class="header">
      <div class="frame">
        <%!-- Logo --%>
        <div class="group">
          <div class="div">
            <.link navigate={Routes.live_path(@socket, MazarynWeb.HomeLive.Home, "en")}>
              <div class="text-wrapper">Mazaryn</div>
              <div class="ellipse"></div>
              <div class="ellipse-2"></div>
            </.link>
          </div>
        </div>

        <%!-- Search and Actions --%>
        <div class="frame-2">
          <%!-- Search Box --%>
          <form class="frame-3" phx-submit="search" phx-target={@myself}>
            <div class="div-2">
              <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path class="vector" d="M9 17C13.4183 17 17 13.4183 17 9C17 4.58172 13.4183 1 9 1C4.58172 1 1 4.58172 1 9C1 13.4183 4.58172 17 9 17Z" stroke="#AAAAAA" stroke-width="2"/>
                <path class="img" d="M19 19L14.65 14.65" stroke="#AAAAAA" stroke-width="2"/>
              </svg>
            </div>
            <input
              type="search"
              name="search"
              value={@search_query}
              placeholder="Search videos"
              class="text-wrapper-2"
              autocomplete="off"
            />
          </form>

          <%!-- Upload Button --%>
          <div class="div-wrapper">
            <div class="ep-upload-filled">
              <svg width="27" height="20" viewBox="0 0 27 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path class="vector-2" d="M13.33 0L8.33 5H12.33V12H14.33V5H18.33L13.33 0ZM5.33 7V9H3.33C2.23 9 1.33 9.9 1.33 11V17C1.33 18.1 2.23 19 3.33 19H23.33C24.43 19 25.33 18.1 25.33 17V11C25.33 9.9 24.43 9 23.33 9H21.33V7H23.33C25.54 7 27.33 8.79 27.33 11V17C27.33 19.21 25.54 21 23.33 21H3.33C1.12 21 -0.67 19.21 -0.67 17V11C-0.67 8.79 1.12 7 3.33 7H5.33Z" fill="#AAAAAA"/>
              </svg>
            </div>
          </div>

          <%!-- Live Button --%>
          <div class="div-wrapper">
            <div class="ri-live-fill">
              <svg width="17" height="12" viewBox="0 0 17 12" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path class="vector-3" d="M8.5 0C3.98 0 0.17 2.84 0 6.5C0.17 10.16 3.98 13 8.5 13C13.02 13 16.83 10.16 17 6.5C16.83 2.84 13.02 0 8.5 0Z" fill="#FF0000"/>
              </svg>
              <svg width="14" height="5" viewBox="0 0 14 5" fill="none" xmlns="http://www.w3.org/2000/svg">
                <text class="live" x="0" y="4" fill="white" font-size="4" font-family="Poppins">LIVE</text>
              </svg>
            </div>
          </div>
        </div>

        <%!-- User Section --%>
        <div class="frame-4">
          <%!-- Coin Balance --%>
          <div class="frame-5">
            <img class="element" src={Routes.static_path(@socket, "/images/mazaryn-symbol.svg")} alt="Mazaryn coin" />
            <div class="text-wrapper-3">0.23</div>
          </div>

          <%!-- Notifications --%>
          <div class="frame-wrapper">
            <.link navigate={Routes.live_path(@socket, MazarynWeb.NotificationLive.Index, "en")}>
              <div class="frame-6">
                <Heroicons.bell solid class="w-5 h-5 fill-[#AAAAAA]" />
              </div>
            </.link>
          </div>

          <%!-- User Menu --%>
          <div class="group-2" x-data="{ isopen: false }" @click.away="isopen = false">
            <div class="rectangle" @click="isopen = !isopen"></div>
            <div class="text-wrapper-4" @click="isopen = !isopen"><%= @user.username %></div>
            <div class="ellipse-3"></div>
            <svg class="vector-6" @click="isopen = !isopen" width="13" height="6" viewBox="0 0 13 6" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path d="M1 1L6.5 5L12 1" stroke="#AAAAAA" stroke-width="2"/>
            </svg>
            <%= if @user.avatar_url do %>
              <img class="mask-group" src={@user.avatar_url} alt={@user.username} />
            <% else %>
              <img class="mask-group" src={Routes.static_path(@socket, "/images/default-user.svg")} alt="Default user" />
            <% end %>

            <%!-- Dropdown Menu --%>
            <div
              x-show="isopen"
              x-cloak
              style="display: none;"
              class="origin-top-right absolute z-100 right-0 mt-2 w-56 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5"
            >
              <.link navigate={Routes.live_path(@socket, MazarynWeb.UserLive.Profile, "en", @user.username)} class="block px-4 py-2 text-sm text-gray-700">
                Profile
              </.link>
              <.link navigate={Routes.logout_path(@socket, :index, "en")} class="block px-4 py-2 text-sm text-gray-700">
                Log Out
              </.link>
            </div>
          </div>
        </div>
      </div>
    </header>
    """
  end

  @impl true
  def handle_event("search", %{"search" => query}, socket) do
    send(self(), {:search, query})
    {:noreply, assign(socket, :search_query, query)}
  end

  @impl true
  def handle_event("upload_video", _params, socket) do
    {:noreply, socket}
  end
end
