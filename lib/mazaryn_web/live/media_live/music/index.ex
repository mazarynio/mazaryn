defmodule MazarynWeb.MediaLive.Music.Index do
  use MazarynWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Music")
     |> assign(:featured_tracks, [])
     |> assign(:artist_tracks, [])
     |> assign(:search_query, "")
     |> load_music()}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Music")
  end

  @impl true
  def handle_event("search", %{"search" => query}, socket) do
    {:noreply, assign(socket, :search_query, query)}
  end

  @impl true
  def handle_event("navigate", %{"section" => section}, socket) do
    {:noreply, socket}
  end

  defp load_music(socket) do
    socket
    |> assign(:featured_tracks, mock_featured_tracks())
    |> assign(:artist_tracks, mock_artist_tracks())
  end

  defp mock_featured_tracks do
    [
      %{
        id: 1,
        title: "Midnight Dreams - Synthwave Mix",
        artist: "Luna Waves",
        plays: "2.3M",
        time_ago: "2 weeks ago",
        cover_art: "/images/music/cover-1.jpg",
        duration: "3:45",
        is_live: false
      },
      %{
        id: 2,
        title: "Live Jazz Session",
        artist: "The Blue Notes",
        plays: "15K",
        time_ago: "streaming now",
        cover_art: "/images/music/cover-2.jpg",
        duration: nil,
        is_live: true
      }
    ]
  end

  defp mock_artist_tracks do
    [
      %{
        id: 10,
        title: "Electronic Beats Vol. 2",
        artist: "DJ Spectrum",
        plays: "890K",
        time_ago: "1 month ago",
        cover_art: "/images/music/cover-10.jpg",
        duration: "4:12",
        is_live: false
      }
    ]
  end
end
