defmodule MazarynWeb.MediaLive.Video.CardComponent do
  use MazarynWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="frame-26">
      <div class={thumbnail_frame_class(@video)} phx-click="play_video" phx-value-id={@video.id} phx-target={@myself}>
        <div class={overlay_container_class(@video)}>
          <%= if @video.is_live do %>
            <div class="frame-32">
              <%= if @video.duration do %>
                <div class="text-wrapper-5"><%= @video.duration %></div>
              <% end %>
              <div class="ri-live-fill">
                <svg width="17" height="12" viewBox="0 0 17 12" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path class="vector-3" d="M8.5 0C3.98 0 0.17 2.84 0 6.5C0.17 10.16 3.98 13 8.5 13C13.02 13 16.83 10.16 17 6.5C16.83 2.84 13.02 0 8.5 0Z" fill="#FF0000"/>
                </svg>
              </div>
            </div>
          <% else %>
            <%= if @video.chat_count do %>
              <div class="frame-28">
                <div class="fluent-chat">
                  <svg width="15" height="15" viewBox="0 0 15 15" fill="none" xmlns="http://www.w3.org/2000/svg">
                    <path class="vector-20" d="M13 2H2C1.45 2 1 2.45 1 3V10C1 10.55 1.45 11 2 11H11L14 14V3C14 2.45 13.55 2 13 2Z" fill="white"/>
                  </svg>
                </div>
                <div class="frame-29">
                  <div class="text-wrapper-5"><%= @video.chat_count %></div>
                  <div class="mdi-eye">
                    <svg width="19" height="19" viewBox="0 0 19 19" fill="none" xmlns="http://www.w3.org/2000/svg">
                      <path class="vector-21" d="M9.5 3.5C5 3.5 1.23 6.61 0 10.75C1.23 14.89 5 18 9.5 18C14 18 17.77 14.89 19 10.75C17.77 6.61 14 3.5 9.5 3.5ZM9.5 16C6.52 16 4 13.48 4 10.5C4 7.52 6.52 5 9.5 5C12.48 5 15 7.52 15 10.5C15 13.48 12.48 16 9.5 16ZM9.5 7C7.57 7 6 8.57 6 10.5C6 12.43 7.57 14 9.5 14C11.43 14 13 12.43 13 10.5C13 8.57 11.43 7 9.5 7Z" fill="white"/>
                    </svg>
                  </div>
                </div>
              </div>
            <% else %>
              <%= if @video.duration do %>
                <div class="frame-32">
                  <div class="text-wrapper-5"><%= @video.duration %></div>
                </div>
              <% end %>
            <% end %>
          <% end %>
        </div>
      </div>

      <div class="frame-8">
        <img class="ellipse-5" src={creator_avatar(@video)} alt={@video.creator} />
        <div class="frame-30">
          <div class={title_class(@video)}><%= @video.title %></div>
          <div class="text-wrapper-14"><%= @video.creator %></div>
          <div class="text-wrapper-14"><%= @video.views %> views • <%= @video.time_ago %></div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("play_video", %{"id" => id}, socket) do
    send(self(), {:play_video, String.to_integer(id)})
    {:noreply, socket}
  end

  defp thumbnail_frame_class(video) do
    case video.id do
      1 -> "frame-27"
      2 -> "frame-31"
      3 -> "frame-33"
      4 -> "frame-34"
      5 -> "frame-35"
      6 -> "frame-36"
      7 -> "frame-37"
      8 -> "frame-38"
      9 -> "frame-39"
      10 -> "frame-40"
      11 -> "frame-41"
      12 -> "frame-42"
      13 -> "frame-44"
      14 -> "frame-45"
      15 -> "frame-46"
      16 -> "frame-47"
      _ -> "frame-27"
    end
  end

  defp overlay_container_class(%{is_live: false, chat_count: nil}), do: "frame-32"
  defp overlay_container_class(%{chat_count: chat}) when not is_nil(chat), do: "frame-28"
  defp overlay_container_class(_), do: "frame-32"

  defp title_class(%{title: title}) when byte_size(title) > 25, do: "text-wrapper-13"
  defp title_class(%{title: title}) when byte_size(title) > 20, do: "text-wrapper-17"
  defp title_class(_), do: "text-wrapper-15"

  defp creator_avatar(%{creator_avatar: avatar}) when not is_nil(avatar), do: avatar
  defp creator_avatar(_), do: "/images/default-creator.png"

  def handle_event("play_video", %{"id" => id}, socket) do
    send(self(), {:play_video, id})
    {:noreply, socket}
  end
end
