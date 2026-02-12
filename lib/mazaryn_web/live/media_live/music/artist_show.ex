defmodule MazarynWeb.MediaLive.Music.ArtistShow do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(%{"id" => artist_id}, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Artist")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:artist_id, artist_id)
     |> assign(:artist, %{})
     |> load_artist_data()}
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
  def handle_event("play_artist", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("follow_artist", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("see_all_featured", _params, socket) do
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

  defp load_artist_data(socket) do
    artist = %{
      id: socket.assigns.artist_id,
      name: "Burna Boy",
      monthly_listeners: "16,118,085",
      followers: "2,345",
      about:
        "With his fusion of dancehall, reggae, Afrobeat, and pop, Burna Boy is one of Nigeria's best-known stars. The LeriQ-produced 2012 single \"Like to Party\" proved to be his breakout track and paved the way for his full-length debut, L.I.F.E, a year later. Over the next five years, Burna released two more albums and collaborated with a variety of artists, from J Hus and Skales to Fall Out Boy and Lily Allen. His international exposure widened with 2018's Outside, which hit number three on the Billboard Reggae chart and won the Nigeria Entertainment Award for Album of the Year. 2019's African Giant and 2020's Twice as Tall were both widely acclaimed and charted in several countries. After becoming the first Nigerian to headline a show at Madison Square Garden, he released his hit sixth album, Love, Damini, in 2022, followed a year later by the Grammy-nominated I Told Them....

Burna Boy was born Damini Ogulu in Port Harcourt, Rivers State, Nigeria, in 1991. He began making music at just ten years old when a fellow classmate at school gave him a copy of the production software FruityLoops. Armed with these means, he began to create his own beats on an old computer. After he graduated, he moved to London to attend university, but he dropped out after two years and moved back to Nigeria. In 2010, the 19-year-old Ogulu traveled to Nigeria's southern coast, where a mutual acquaintance, producer LeriQ, had some studio space. This marked a period when he began to connect to the music of his native country, having spent most of his youth immersed in American acts like DMX. He delved into the dancehall and reggae music his father listened to and explored the Afro-beat music preferred by his grandfather (who had also been Fela Kuti's first manager). As a result of his new discoveries, Ogulu created a confluence of genres that would become his signature sound.",
      popular_tracks: [
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
      ],
      featured_tracks: [
        %{
          id: "1",
          title: "JKRS - It's a Fine Day",
          cover: "https://placehold.co/200x160",
          description: "The track heating up the continent"
        },
        %{
          id: "2",
          title: "Everything goes on",
          cover: "https://placehold.co/200x160",
          description: "GEt ready for Burna Boy"
        },
        %{
          id: "3",
          title: "Kiss me more",
          cover: "https://placehold.co/200x160",
          description: "The track heating up the continent"
        },
        %{
          id: "4",
          title: "JKRS - It's a Fine Day",
          cover: "https://placehold.co/200x160",
          description: "Hottest singles, COVER: Spiral"
        },
        %{
          id: "5",
          title: "JKRS - It's a Fine Day",
          cover: "https://placehold.co/200x160",
          description: "With wizkid asake devido"
        }
      ],
      playlists: [
        %{
          id: "1",
          title: "It's a Fine Day",
          cover: "https://placehold.co/200x160",
          description: "New album, I told them not, out now"
        },
        %{
          id: "2",
          title: "Everything goes on",
          cover: "https://placehold.co/200x160",
          description: "porter"
        },
        %{
          id: "3",
          title: "Kiss me more",
          cover: "https://placehold.co/200x160",
          description: "With wizkid asake devido"
        }
      ]
    }

    socket |> assign(:artist, artist)
  end
end
