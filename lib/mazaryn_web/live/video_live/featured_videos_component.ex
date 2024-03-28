defmodule MazarynWeb.VideoLive.FeaturedVideosComponent do
  use MazarynWeb, :live_component

  @spec render(map()) :: Phoenix.LiveView.Rendered.t()
  def render(assigns) do
    ~H"""
    <div class="flex items-center justify-center">
      <div>
        <svg width="49" height="52" viewBox="0 0 49 52" fill="none" xmlns="http://www.w3.org/2000/svg">
          <rect
            x="48.5"
            y="51.25"
            width="48.25"
            height="50.5"
            rx="24.125"
            transform="rotate(-180 48.5 51.25)"
            fill="#D2D2D1"
          />
          <rect
            x="48.5"
            y="51.25"
            width="48.25"
            height="50.5"
            rx="24.125"
            transform="rotate(-180 48.5 51.25)"
            fill="#323340"
          />
          <path
            d="M27.5 19.75L21.25 26L27.5 32.25"
            stroke="white"
            stroke-width="3.125"
            stroke-linecap="round"
            stroke-linejoin="round"
          />
        </svg>
      </div>
      <div class="flex justify-center items-center">
        <%!-- TODO: implement the reusable video component below --%>
        <%!-- <.live_component module={MazarynWeb.VideoLive.VideoPlayerComponent} video={@video} /> --%>
      </div>
      <div></div>
      <div>
        <svg width="49" height="52" viewBox="0 0 49 52" fill="none" xmlns="http://www.w3.org/2000/svg">
          <rect x="0.5" y="0.75" width="48.25" height="50.5" rx="24.125" fill="#D2D2D1" />
          <rect x="0.5" y="0.75" width="48.25" height="50.5" rx="24.125" fill="#323340" />
          <path
            d="M21.5 32.25L27.75 26L21.5 19.75"
            stroke="white"
            stroke-width="3.125"
            stroke-linecap="round"
            stroke-linejoin="round"
          />
        </svg>
      </div>
    </div>
    """
  end
end
