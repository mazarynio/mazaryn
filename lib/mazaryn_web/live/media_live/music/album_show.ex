defmodule MazarynWeb.MediaLive.Music.AlbumShow do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(%{"id" => album_id}, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Album")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:album_id, album_id)
     |> assign(:album, %{})
     |> load_album_data()}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, socket |> assign(:search_query, query)}
  end

  @impl true
  def handle_event("play_album", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("like_album", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("play_track", %{"id" => track_id}, socket) do
    {:noreply, socket}
  end

  defp get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(%{"session_uuid" => _session_uuid, "user_id" => user_id})
       when user_id != nil do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(_), do: nil

  defp load_album_data(socket) do
    album = %{
      id: socket.assigns.album_id,
      title: "Nothing Was the Same",
      description: "With Shalilipo, Jeriq, Blaqbonee and more",
      cover_image: "https://placehold.co/200x200",
      artist: "Drake",
      artist_avatar: "https://placehold.co/20x20",
      year: "2024",
      likes: "300",
      duration: "about 2 hr",
      tracks: [
        %{
          id: "1",
          title: "JKRS - It's a Fine Day",
          artist: "Lithuania",
          album_cover: "https://placehold.co/60x60",
          duration: "6:56"
        },
        %{
          id: "2",
          title: "Fine Babe",
          artist: "Tiwa",
          album_cover: "https://placehold.co/60x60",
          duration: "3:06"
        },
        %{
          id: "3",
          title: "GoodDay Today",
          artist: "Lithuania",
          album_cover: "https://placehold.co/60x60",
          duration: "10:06"
        }
      ]
    }

    socket |> assign(:album, album)
  end
end
