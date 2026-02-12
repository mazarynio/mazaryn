defmodule MazarynWeb.MediaLive.Music.SongShow do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(%{"id" => song_id}, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Song")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:song_id, song_id)
     |> assign(:song, %{})
     |> assign(:recommended_tracks, [])
     |> load_song_data()}
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
  def handle_event("play_song", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("like_song", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("play_recommended_track", %{"id" => track_id}, socket) do
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

  defp load_song_data(socket) do
    song = %{
      id: socket.assigns.song_id,
      title: "City Boy",
      cover_image: "https://placehold.co/200x200",
      artist: "Burna Boy",
      artist_avatar: "https://placehold.co/20x20",
      year: "2024",
      duration: "2:23",
      lyrics:
        "Ayo, I'm not even gonna lie       I used to call myself a ugly yute, but I'm not even a ugly yute       I'm a sexy yute, you understand?       Gyal all over the globe wanna 'uck me, you understand?\nGirls inna my crib       Zero Snapchat, zero Instagram posting (ayy)       Fuck up the vibe (yeah)       My dick start fallin' like London Bridge (yeah)       I don't care if I saw you in a magazine (ah, ah)       Or if you're on TV       That one don't mean nothin' to me       Don't need a shy ho, baby, I need a freak       Lick it like ice cream, as if you mean to be disgusting (ah, ah)       It's nothin' o       Chop my gbana       Once I halla       Then no stoppin' o (ah, ah)       Go shoppin' o       Fuck that (ayy), shh (yeah)\nMi o fa'gbo lo rado       BRKFST mo n fa o       Mi o de ni mu Jekonmo       Azul and Champagne ni ma mu o       I getti girls from the ghetto       Get girls from uptown       That's the life of a city boy       Ah       O ye ke (yeah), da mo       O ye ke, je lo       O ye ke, jowo o (ayy)       , ah)"
    }

    recommended_tracks = [
      %{
        id: "1",
        title: "It's a Fine Day",
        cover: "https://placehold.co/60x60",
        duration: "6:56"
      },
      %{
        id: "2",
        title: "Fine Babe",
        cover: "https://placehold.co/60x60",
        duration: "3:06"
      },
      %{
        id: "3",
        title: "GoodDay Today",
        cover: "https://placehold.co/60x60",
        duration: "10:06"
      },
      %{
        id: "4",
        title: "Fine Babe",
        cover: "https://placehold.co/60x60",
        duration: "3:06"
      },
      %{
        id: "5",
        title: "GoodDay Today",
        cover: "https://placehold.co/60x60",
        duration: "10:06"
      }
    ]

    socket
    |> assign(:song, song)
    |> assign(:recommended_tracks, recommended_tracks)
  end
end
