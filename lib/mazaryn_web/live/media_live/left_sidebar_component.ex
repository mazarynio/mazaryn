defmodule MazarynWeb.MediaLive.LeftSidebarComponent do
  use MazarynWeb, :live_component
  use Phoenix.VerifiedRoutes, endpoint: MazarynWeb.Endpoint, router: MazarynWeb.Router

  alias MazarynWeb.Components.Icons

  @spec render(map()) :: Phoenix.LiveView.Rendered.t()
  def render(assigns) do
    ~H"""
    <div>
      <p class="mb-6 font-medium text-base leading-6 text-[#CFD0DD]">
        <%= if(@media == "audios", do: "Music", else: "Video") %>
      </p>
      <div class="w-full bg-[#393a48] border border-[#323340] py-6 px-5 rounded-[1.25rem]">
        <div class="flex justify-between align-center items-center">
          <div class="flex justify-center items-center">
            <ul>
              <li class="flex align-center items-center mx-2 mb-7">
                <.link
                  navigate={~p"/media/#{@media}"}
                  class="group flex align-center items-start text-base text-[#CFD0DD] font-semibold hover:text-blue-500"
                >
                  <i>
                    <Icons.home_icon />
                  </i>
                  <div class="text-[#CFD0DD] text-base leading-6 group-hover:text-[#4385F5]">
                    Home
                  </div>
                </.link>
              </li>
              <%= if @media == "audios" do %>
                <li class="flex align-center items-center mx-2 mb-7">
                  <.link
                    navigate={~p"/media/audios"}
                    class="group flex items-start text-base text-[#CFD0DD] font-semibold hover:text-blue-500 "
                  >
                    <i>
                      <Icons.artist_icon />
                    </i>
                    <div class="text-[#CFD0DD] text-base leading-6 group-hover:text-[#4385F5]">
                      Artist
                    </div>
                  </.link>
                </li>
              <% end %>
              <%= if @media == "audios" do %>
                <li class="flex align-center items-center mx-2">
                  <.link
                    navigate={~p"/media/audios"}
                    class="group flex items-start text-base text-[#CFD0DD] font-semibold hover:text-blue-500 "
                  >
                    <i>
                      <Icons.album_icon />
                    </i>
                    <div class="text-[#CFD0DD] text-base leading-6 group-hover:text-[#4385F5]">
                      Album
                    </div>
                  </.link>
                </li>
              <% end %>
              <%= if @media == "videos" do %>
                <li class="flex align-center items-center mx-2 mb-7">
                  <.link
                    navigate={~p"/media/videos"}
                    class="group flex items-start text-base text-[#CFD0DD] font-semibold hover:text-blue-500 "
                  >
                    <i>
                      <Icons.video_icon />
                    </i>
                    <div class="text-[#CFD0DD] text-base leading-6 group-hover:text-[#4385F5]">
                      Your videos
                    </div>
                  </.link>
                </li>
              <% end %>
              <%= if @media == "videos" do %>
                <li class="flex align-center items-center mx-2 mb-7">
                  <.link
                    navigate={~p"/media/videos"}
                    class="group flex items-start text-base text-[#CFD0DD] font-semibold hover:text-blue-500"
                  >
                    <i>
                      <Icons.discover_icon />
                    </i>
                    <div class="text-base leading-6 text-[#CFD0DD] group-hover:text-[#4385F5]">
                      Discover
                    </div>
                  </.link>
                </li>
              <% end %>
              <%= if @media == "videos" do %>
                <li class="flex align-center items-center mx-2">
                  <.link
                    navigate={~p"/videos"}
                    class="group flex items-start text-base text-[#CFD0DD] font-semibold hover:text-blue-500"
                  >
                    <i>
                      <Icons.trending_icon />
                    </i>
                    <div class="text-base leading-6 text-[#CFD0DD] group-hover:text-[#4385F5]">
                      Trending
                    </div>
                  </.link>
                </li>
              <% end %>
            </ul>
          </div>
        </div>
      </div>
      <div class="w-full pt-7 px-5 my-8 bg-[#393a48] border border-[#323340] rounded-[1.25rem]">
        <div class="flex justify-between align-center items-center">
          <div class="flex justify-center items-center">
            <ul>
              <%= if @media == "audios" do %>
                <li class="flex align-center items-center mx-2 mb-7">
                  <.link
                    navigate={~p"/media/audios"}
                    class="group flex items-start text-base text-[#CFD0DD] font-semibold hover:text-blue-500"
                  >
                    <i>
                      <Icons.playlist_icon />
                    </i>
                    <div class="text-base leading-6 text-[#CFD0DD] group-hover:text-[#4385F5]">
                      Playlist
                    </div>
                  </.link>
                </li>
              <% end %>
              <%= if @media == "videos" do %>
                <li class="flex align-center items-center mx-2 mb-7">
                  <.link
                    navigate={~p"/media/videos"}
                    class="group flex items-start text-base text-[#CFD0DD] font-semibold hover:text-blue-500"
                  >
                    <i>
                      <Icons.settings_icon />
                    </i>
                    <div class="text-base leading-6 text-[#CFD0DD] group-hover:text-[#4385F5]">
                      Settings
                    </div>
                  </.link>
                </li>
              <% end %>
            </ul>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
