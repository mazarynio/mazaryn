defmodule MazarynWeb.VideoLive.NavComponent do
  use MazarynWeb, :live_component

  @type socket :: Phoenix.LiveView.Socket.t()

  @impl Phoenix.LiveComponent
  @spec render(map()) :: Phoenix.LiveView.Rendered.t()
  def render(assigns) do
    ~H"""
    <div class="relative bg-[#151620]">
      <nav class="relative w-[90%] max-w-[1440px] mx-auto h-16 py-2.5 flex justify-evenly items-center text-center">
        <div class="w-[17%] mr-14 text-xl font-extrabold text-black cursor-pointer flex-shrink-0">
          <%= link to: Routes.live_path(@socket, MazarynWeb.HomeLive.Home, @locale), class: "block" do %>
            <svg
              width="158"
              height="52"
              viewBox="0 0 158 52"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M29.9418 13.4736V38H23.9674V23.2911L18.4822 38H13.6607L8.14054 23.2562V38H2.16615V13.4736H9.22362L16.1064 30.4534L22.9193 13.4736H29.9418ZM33.0502 28.2174C33.0502 26.2143 33.4229 24.4557 34.1682 22.9417C34.9369 21.4278 35.9733 20.2632 37.2777 19.4479C38.5821 18.6327 40.0378 18.2251 41.6449 18.2251C43.0192 18.2251 44.2187 18.5046 45.2436 19.0636C46.2917 19.6226 47.0953 20.3563 47.6543 21.2647V18.5046H53.6287V38H47.6543V35.2399C47.072 36.1483 46.2568 36.882 45.2086 37.441C44.1838 38 42.9842 38.2795 41.61 38.2795C40.0262 38.2795 38.5821 37.8719 37.2777 37.0567C35.9733 36.2182 34.9369 35.0419 34.1682 33.5279C33.4229 31.9907 33.0502 30.2205 33.0502 28.2174ZM47.6543 28.2523C47.6543 26.7616 47.235 25.5854 46.3965 24.7236C45.5813 23.8618 44.5797 23.4309 43.3918 23.4309C42.204 23.4309 41.1908 23.8618 40.3522 24.7236C39.537 25.5621 39.1294 26.7267 39.1294 28.2174C39.1294 29.7081 39.537 30.8959 40.3522 31.781C41.1908 32.6428 42.204 33.0737 43.3918 33.0737C44.5797 33.0737 45.5813 32.6428 46.3965 31.781C47.235 30.9192 47.6543 29.743 47.6543 28.2523ZM63.5409 33.0737H71.9959V38H56.8678V33.2484L64.9734 23.4309H56.9377V18.5046H71.8212V23.2562L63.5409 33.0737ZM74.0955 28.2174C74.0955 26.2143 74.4681 24.4557 75.2135 22.9417C75.9821 21.4278 77.0186 20.2632 78.323 19.4479C79.6273 18.6327 81.0831 18.2251 82.6902 18.2251C84.0644 18.2251 85.264 18.5046 86.2888 19.0636C87.337 19.6226 88.1405 20.3563 88.6995 21.2647V18.5046H94.6739V38H88.6995V35.2399C88.1172 36.1483 87.302 36.882 86.2539 37.441C85.229 38 84.0295 38.2795 82.6553 38.2795C81.0714 38.2795 79.6273 37.8719 78.323 37.0567C77.0186 36.2182 75.9821 35.0419 75.2135 33.5279C74.4681 31.9907 74.0955 30.2205 74.0955 28.2174ZM88.6995 28.2523C88.6995 26.7616 88.2803 25.5854 87.4418 24.7236C86.6266 23.8618 85.625 23.4309 84.4371 23.4309C83.2492 23.4309 82.236 23.8618 81.3975 24.7236C80.5823 25.5621 80.1747 26.7267 80.1747 28.2174C80.1747 29.7081 80.5823 30.8959 81.3975 31.781C82.236 32.6428 83.2492 33.0737 84.4371 33.0737C85.625 33.0737 86.6266 32.6428 87.4418 31.781C88.2803 30.9192 88.6995 29.743 88.6995 28.2523ZM104.971 21.7539C105.669 20.6824 106.543 19.8439 107.591 19.2383C108.639 18.6094 109.804 18.295 111.085 18.295V24.6188H109.443C107.952 24.6188 106.834 24.9449 106.089 25.597C105.343 26.2259 104.971 27.3439 104.971 28.9511V38H98.9961V18.5046H104.971V21.7539ZM133.855 18.5046L121.627 47.2586H115.198L119.67 37.3362L111.739 18.5046H118.412L122.919 30.698L127.391 18.5046H133.855ZM147.894 18.295C150.177 18.295 151.994 19.0403 153.345 20.531C154.719 21.9984 155.406 24.0248 155.406 26.6102V38H149.466V27.4138C149.466 26.1095 149.129 25.0962 148.453 24.3742C147.778 23.6521 146.869 23.2911 145.728 23.2911C144.587 23.2911 143.678 23.6521 143.003 24.3742C142.327 25.0962 141.99 26.1095 141.99 27.4138V38H136.015V18.5046H141.99V21.09C142.595 20.2282 143.411 19.5528 144.435 19.0636C145.46 18.5512 146.613 18.295 147.894 18.295Z"
                fill="white"
              />
              <circle cx="78.5945" cy="12.2615" r="3.99682" fill="#4385F5" />
              <circle cx="91.5066" cy="12.2615" r="3.99682" fill="#4385F5" />
            </svg>
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
            name="search videos"
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
            <div class="flex bg-[#323340] center rounded-full p-2">
              <span class="relative inline-block">
                <svg
                  width="28"
                  height="28"
                  viewBox="0 0 28 20"
                  fill="none"
                  xmlns="http://www.w3.org/2000/svg"
                >
                  <path
                    d="M14.9537 20V14.2857H18.7632L14.0013 8.57143L9.2394 14.2857H13.0489V20H8.28702V19.9524C8.12702 19.9619 7.97464 20 7.81083 20C5.91642 20 4.09961 19.2475 2.76006 17.9079C1.42052 16.5684 0.667969 14.7515 0.667969 12.8571C0.667969 9.19238 3.4394 6.20571 6.99559 5.79619C7.30738 4.16627 8.17739 2.69594 9.45598 1.63809C10.7346 0.580246 12.3418 0.00100574 14.0013 0C15.661 0.000928883 17.2685 0.580101 18.5474 1.63792C19.8263 2.69575 20.6967 4.16612 21.0089 5.79619C24.5651 6.20571 27.3327 9.19238 27.3327 12.8571C27.3327 14.7515 26.5802 16.5684 25.2406 17.9079C23.9011 19.2475 22.0843 20 20.1899 20C20.0299 20 19.8756 19.9619 19.7137 19.9524V20H14.9537Z"
                    fill="white"
                  />
                </svg>
              </span>
            </div>
          </li>
          <li class="ml-5">
            <div class="flex bg-[#323340] center rounded-full p-3">
              <span class="relative inline-block">
                <svg
                  width="18"
                  height="20"
                  viewBox="0 0 18 20"
                  fill="none"
                  xmlns="http://www.w3.org/2000/svg"
                >
                  <g clip-path="url(#clip0_2861_1999)">
                    <path
                      d="M12 0C12.1989 0 12.3897 0.0790178 12.5303 0.21967C12.671 0.360322 12.75 0.551088 12.75 0.75V3.9L16.6597 1.1625C16.716 1.12309 16.7819 1.09987 16.8504 1.09538C16.9189 1.09088 16.9874 1.10528 17.0483 1.137C17.1091 1.16872 17.1602 1.21654 17.1957 1.27526C17.2313 1.33398 17.2501 1.40134 17.25 1.47V10.53C17.2501 10.5987 17.2313 10.666 17.1957 10.7247C17.1602 10.7835 17.1091 10.8313 17.0483 10.863C16.9874 10.8947 16.9189 10.9091 16.8504 10.9046C16.7819 10.9001 16.716 10.8769 16.6597 10.8375L12.75 8.1V11.25C12.75 11.4489 12.671 11.6397 12.5303 11.7803C12.3897 11.921 12.1989 12 12 12H1.5C1.30109 12 1.11032 11.921 0.96967 11.7803C0.829018 11.6397 0.75 11.4489 0.75 11.25V0.75C0.75 0.551088 0.829018 0.360322 0.96967 0.21967C1.11032 0.0790178 1.30109 0 1.5 0H12ZM5.55 3.62175C5.48084 3.62174 5.4138 3.64562 5.36023 3.68935C5.30665 3.73309 5.26983 3.79399 5.256 3.86175L5.25 3.921V8.0775C5.24999 8.12608 5.26179 8.17394 5.28436 8.21696C5.30694 8.25998 5.33963 8.29687 5.37962 8.32446C5.4196 8.35205 5.46569 8.36952 5.51392 8.37536C5.56215 8.38121 5.61108 8.37525 5.6565 8.358L5.71125 8.331L8.9775 6.25275C9.01562 6.22829 9.04773 6.19553 9.07142 6.15692C9.09511 6.11832 9.10978 6.07485 9.11432 6.02979C9.11887 5.98472 9.11317 5.9392 9.09766 5.89664C9.08215 5.85408 9.05723 5.81558 9.02475 5.784L8.9775 5.7465L5.71125 3.6675C5.66286 3.6375 5.60693 3.62115 5.55 3.62175Z"
                      fill="white"
                    />
                    <path
                      d="M3.05892 19.2434H4.79858V19.9433H2V15.5422H3.05892V19.2434Z"
                      fill="white"
                    />
                    <path
                      d="M6.00866 16.034C5.82209 16.034 5.66577 15.9857 5.53971 15.889C5.41869 15.7881 5.35818 15.6641 5.35818 15.517C5.35818 15.3699 5.41869 15.248 5.53971 15.1513C5.66577 15.0504 5.82209 15 6.00866 15C6.19523 15 6.34903 15.0504 6.47005 15.1513C6.59611 15.248 6.65914 15.3699 6.65914 15.517C6.65914 15.6641 6.59611 15.7881 6.47005 15.889C6.34903 15.9857 6.19523 16.034 6.00866 16.034ZM6.53056 16.4502V19.9433H5.47164V16.4502H6.53056Z"
                      fill="white"
                    />
                    <path
                      d="M9.324 19.1299L10.3829 16.4502H11.5099L9.95935 19.9433H8.67352L7.13052 16.4502H8.26508L9.324 19.1299Z"
                      fill="white"
                    />
                    <path
                      d="M16 18.1211C16 18.2472 15.9899 18.3607 15.9697 18.4615H12.9064C12.9316 18.7137 13.0375 18.9113 13.2241 19.0542C13.4107 19.1971 13.6401 19.2686 13.9124 19.2686C14.3057 19.2686 14.5856 19.1278 14.752 18.8462H15.8941C15.7731 19.1824 15.5411 19.4599 15.1982 19.6784C14.8554 19.8928 14.4343 20 13.9351 20C13.5317 20 13.1686 19.9264 12.8459 19.7793C12.5282 19.628 12.2786 19.4157 12.0971 19.1425C11.9206 18.8693 11.8324 18.554 11.8324 18.1967C11.8324 17.8352 11.9206 17.5179 12.0971 17.2446C12.2736 16.9714 12.5207 16.7612 12.8384 16.6141C13.156 16.467 13.5216 16.3934 13.9351 16.3934C14.3335 16.3934 14.689 16.4649 15.0016 16.6078C15.3193 16.7507 15.5638 16.9546 15.7353 17.2194C15.9118 17.48 16 17.7806 16 18.1211ZM14.9033 17.8689C14.8982 17.6419 14.7999 17.4611 14.6083 17.3266C14.4167 17.1879 14.1822 17.1185 13.9048 17.1185C13.6426 17.1185 13.4208 17.1858 13.2392 17.3203C13.0627 17.4506 12.9543 17.6335 12.914 17.8689H14.9033Z"
                      fill="white"
                    />
                  </g>
                  <defs>
                    <clipPath id="clip0_2861_1999">
                      <rect width="16.5" height="20" fill="white" transform="translate(0.75)" />
                    </clipPath>
                  </defs>
                </svg>
              </span>
            </div>
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
            <div class="flex bg-[#323340] items-center rounded-full p-[0.65rem]">
              <svg
                width="20"
                height="20"
                viewBox="0 0 20 20"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <g clip-path="url(#clip0_2852_1896)">
                  <path
                    d="M16.7669 14.1951L14.9922 11.6839V6.77797C14.9922 4.27399 13.1389 2.19496 10.732 1.83988V0.731717C10.732 0.327617 10.4044 0 10.0003 0C9.59617 0 9.26858 0.327617 9.26858 0.731717V1.83984C6.86171 2.19492 5.00838 4.27394 5.00838 6.77793V11.6839L3.23366 14.1951C2.89202 14.6784 3.23799 15.3491 3.83116 15.3491H16.1694C16.7613 15.3492 17.1093 14.6794 16.7669 14.1951Z"
                    fill="white"
                  />
                  <path
                    d="M9.99975 15.8716C8.86155 15.8716 7.93555 16.7975 7.93555 17.9357C7.93555 19.0739 8.86162 20 9.99975 20C11.138 20 12.0639 19.074 12.0639 17.9358C12.0639 16.7976 11.1379 15.8716 9.99975 15.8716Z"
                    fill="white"
                  />
                </g>
                <defs>
                  <clipPath id="clip0_2852_1896">
                    <rect width="20" height="20" fill="white" />
                  </clipPath>
                </defs>
              </svg>
            </div>
          </li>
          <li class="absolute right-[6rem] xl:right-[9.5rem] ml-5 -mr-8 z-50">
            <%= if @user.avatar_url do %>
              <img src={"#{@user.avatar_url}"} class="w-6 rounded-full" />
            <% else %>
              <svg
                width="37"
                height="40"
                viewBox="0 0 137 135"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M68.648 0C31.634 0 0.369141 30.9083 0.369141 67.5C0.369141 104.092 31.634 135 68.648 135C105.662 135 136.927 104.092 136.927 67.5C136.927 30.9083 105.662 0 68.648 0ZM68.648 33.75C80.4397 33.75 89.1316 42.336 89.1316 54C89.1316 65.664 80.4397 74.25 68.648 74.25C56.8631 74.25 48.1643 65.664 48.1643 54C48.1643 42.336 56.8631 33.75 68.648 33.75ZM33.7848 99.711C39.9094 90.801 50.1239 84.861 61.8201 84.861H75.4759C87.1789 84.861 97.3866 90.801 103.511 99.711C94.7851 108.945 82.4062 114.75 68.648 114.75C54.8898 114.75 42.5108 108.945 33.7848 99.711Z"
                  fill="#60616D"
                />
              </svg>
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
                  <%= live_patch to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile, @locale, @user.username),  class: "text-gray-700 block px-4 py-2 text-sm", role: "menuitem", tabindex: "-1", id: "menu-item-0" do %>
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
            <svg
              class="w-6 h-6 text-gray-500"
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
      <div class="hidden mobile-menu text-white">
        <ul>
          <li>
            <%= link("Coin",
              to: Routes.live_path(@socket, MazarynWeb.CoinLive.Index, @locale),
              class: "block text-sm px-2 py-2 hover:bg-slate-500 transition duration-300"
            ) %>
          </li>
          <li>
            <%= link("Profile",
              to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile, @locale, @user.username),
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
