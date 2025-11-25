defmodule MazarynWeb.MediaLive.Video.SidebarComponent do
  use MazarynWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="explore">
      <div class="text-wrapper-9">Video</div>

      <div class="div-3">
        <div class={["frame-19", @active_section == "home" && "active"]} phx-click="navigate" phx-value-section="home" phx-target={@myself}>
          <div class="div-2">
            <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path class="vector-11" d="M10 2L2 8V18C2 18.5304 2.21071 19.0391 2.58579 19.4142C2.96086 19.7893 3.46957 20 4 20H16C16.5304 20 17.0391 19.7893 17.4142 19.4142C17.7893 19.0391 18 18.5304 18 18V8L10 2Z" fill={@active_section == "home" && "#4385F5" || "#CFD0DD"} stroke={@active_section == "home" && "#4385F5" || "#CFD0DD"} stroke-width="2"/>
            </svg>
          </div>
          <div class={@active_section == "home" && "text-wrapper-10" || "text-wrapper-11"}>Home</div>
        </div>

        <div class={["frame-20", @active_section == "your_videos" && "active"]} phx-click="navigate" phx-value-section="your_videos" phx-target={@myself}>
          <div class="div-2">
            <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path class="vector-12" d="M5 2C3.34315 2 2 3.34315 2 5V15C2 16.6569 3.34315 18 5 18H15C16.6569 18 18 16.6569 18 15V5C18 3.34315 16.6569 2 15 2H5ZM8 6L14 10L8 14V6Z" fill={@active_section == "your_videos" && "#4385F5" || "#CFD0DD"}/>
            </svg>
          </div>
          <div class="text-wrapper-11">Your videos</div>
        </div>

        <div class={["frame-21", @active_section == "discover" && "active"]} phx-click="navigate" phx-value-section="discover" phx-target={@myself}>
          <div class="clarity-world-solid">
            <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
              <circle class="vector-15" cx="10" cy="10" r="9" stroke={@active_section == "discover" && "#4385F5" || "#CFD0DD"} stroke-width="2"/>
              <path class="vector-13" d="M6 10C6 11 7 11.5 7.5 11.5" stroke={@active_section == "discover" && "#4385F5" || "#CFD0DD"}/>
              <path class="vector-14" d="M14 10C14 11 13 11.5 12.5 11.5" stroke={@active_section == "discover" && "#4385F5" || "#CFD0DD"}/>
            </svg>
          </div>
          <div class="text-wrapper-11">Discover</div>
        </div>

        <div class={["frame-22", @active_section == "trending" && "active"]} phx-click="navigate" phx-value-section="trending" phx-target={@myself}>
          <div class="div-2">
            <div class="group-3">
              <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path class="vector-18" d="M2 18L7 10L12 14L18 2" stroke={@active_section == "trending" && "#4385F5" || "#CFD0DD"} stroke-width="2"/>
              </svg>
            </div>
          </div>
          <div class={@active_section == "trending" && "text-wrapper-9" || "text-wrapper-11"}>Trending</div>
        </div>
      </div>

      <div class="div-3">
        <div class="frame-11" phx-click="navigate" phx-value-section="settings" phx-target={@myself}>
          <div class="div-2">
            <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path d="M10 12C11.1046 12 12 11.1046 12 10C12 8.89543 11.1046 8 10 8C8.89543 8 8 8.89543 8 10C8 11.1046 8.89543 12 10 12Z" fill="#CFD0DD"/>
              <path d="M18 10C18 9.5 17.8 9.1 17.5 8.8L16.3 7.6C16.2 7 16 6.5 15.7 6L16.2 4.5C16.3 4.1 16.2 3.7 15.9 3.4C15.6 3.1 15.2 3 14.8 3.1L13.3 3.6C12.8 3.3 12.3 3.1 11.7 3L10.5 1.8C10.2 1.5 9.8 1.3 9.3 1.3C8.8 1.3 8.4 1.5 8.1 1.8L6.9 3C6.3 3.1 5.8 3.3 5.3 3.6L3.8 3.1C3.4 3 3 3.1 2.7 3.4C2.4 3.7 2.3 4.1 2.4 4.5L2.9 6C2.6 6.5 2.4 7 2.3 7.6L1.1 8.8C0.8 9.1 0.6 9.5 0.6 10C0.6 10.5 0.8 10.9 1.1 11.2L2.3 12.4C2.4 13 2.6 13.5 2.9 14L2.4 15.5C2.3 15.9 2.4 16.3 2.7 16.6C3 16.9 3.4 17 3.8 16.9L5.3 16.4C5.8 16.7 6.3 16.9 6.9 17L8.1 18.2C8.4 18.5 8.8 18.7 9.3 18.7C9.8 18.7 10.2 18.5 10.5 18.2L11.7 17C12.3 16.9 12.8 16.7 13.3 16.4L14.8 16.9C15.2 17 15.6 16.9 15.9 16.6C16.2 16.3 16.3 15.9 16.2 15.5L15.7 14C16 13.5 16.2 13 16.3 12.4L17.5 11.2C17.8 10.9 18 10.5 18 10Z" stroke="#CFD0DD" stroke-width="2"/>
            </svg>
          </div>
          <div class="text-wrapper-9">Settings</div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("navigate", %{"section" => section}, socket) do
    send(self(), {:navigate_section, section})
    {:noreply, socket}
  end
end
