defmodule MazarynWeb.AudioLive.Components do
  @moduledoc false

  use Phoenix.Component
  use Phoenix.VerifiedRoutes, endpoint: MazarynWeb.Endpoint, router: MazarynWeb.Router

  alias MazarynWeb.Components.Icons

  @type rendered :: Phoenix.LiveView.Rendered.t()

  @spec recently_played_audio_component(map()) :: rendered()
  def recently_played_audio_component(assigns) do
    ~H"""
    <div class="md:col-span-3 lg:col-span-4 mb-10 py-6 px-11">
      <div class="flex justify-between">
        <h1>Recently Played</h1>
        <a href="#" class="text-[#4385F5]">See All</a>
      </div>
      <div class="flex flex-wrap mt-8">
        <%= for _ <- 1..5  do %>
          <div class="flex flex-col mx-1 mt-3">
            <img
              src={~p"/images/recently_played_1.png"}
              alt="JKRS - It's a Fine Day"
              width="160"
              height="120"
            />
            <p>JKRS - It's a Fine Day</p>
            <p>Lithuania</p>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  @spec trending_and_popular_audio_component(map()) :: rendered()
  def trending_and_popular_audio_component(assigns) do
    ~H"""
    <div class="relative grid grid-cols-1 md:grid-cols-3 justify-between md:col-span-3 lg:col-span-4 ml-80 w-full">
      <div class="mr-2 mb-2">
        <p class="">Trending</p>
        <%= for _trending <- 1..4  do %>
          <div class="flex mt-3 gap-x-2">
            <img src={~p"/images/recently_played_1.png"} alt="JKRS - It's a Fine Day" width="60" />
            <div class="flex flex-col">
              <p>JKRS - It's a Fine Day</p>
              <p>Lithuania</p>
            </div>
            <div class="flex items-center">
              <p class="mr-3">6:56</p>
              <Icons.play_icon />
            </div>
          </div>
        <% end %>
      </div>
      <div class="md:col-span-2">
        <p class="">Popular Albums</p>
        <div class="flex flex-wrap mt-3 gap-x-2">
          <%= for _popular <- 1..3 do %>
            <div class="flex flex-col mb-2">
              <img src={~p"/images/recently_played_1.png"} alt="JKRS - It's a Fine Day" width="200" />
              <p class="mt-2">Kiss me more</p>
              <p class="mt-2">Doja</p>
            </div>
          <% end %>
        </div>
      </div>
      <div class="absolute top-0 right-4">
        <p class="">See All</p>
      </div>
    </div>
    """
  end
end
