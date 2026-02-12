defmodule MazarynWeb.MediaLive.Music.Index do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    Logger.info("🎵 Music Index - Mount Started")

    user = get_user_from_session(session)
    username = get_username(user)
    is_admin = is_admin_user(username)

    Logger.info("🎵 Username: #{username}, Is Admin: #{is_admin}")

    {:ok,
     socket
     |> assign(:page_title, "Music")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:username, username)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:recently_played, [])
     |> assign(:trending_tracks, [])
     |> assign(:popular_albums, [])
     |> assign(:popular_artists, [])
     |> assign(:workout_playlist, [])
     |> assign(:chill_playlist, [])
     |> assign(:study_playlist, [])
     |> assign(:is_admin, is_admin)
     |> load_music_data()}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, socket |> assign(:search_query, query) |> search_music(query)}
  end

  @impl true
  def handle_event("play_track", %{"id" => track_id}, socket) do
    track_id_string = to_string(track_id)

    {:noreply,
     push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/song/#{track_id_string}")}
  end

  @impl true
  def handle_event("add_to_queue", %{"id" => _track_id}, socket) do
    {:noreply, put_flash(socket, :info, "Added to queue")}
  end

  @impl true
  def handle_event("play_all_recently_played", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("play_all_trending", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("see_all_recently_played", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/recently-played")}
  end

  @impl true
  def handle_event("see_all_popular_albums", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/albums")}
  end

  @impl true
  def handle_event("see_all_popular_artists", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/artists")}
  end

  @impl true
  def handle_event("see_all_workout", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/playlists/workout")}
  end

  @impl true
  def handle_event("see_all_chill", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/playlists/chill")}
  end

  @impl true
  def handle_event("see_all_study", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/playlists/study")}
  end

  @impl true
  def handle_event("view_artist", %{"id" => artist_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/artist/#{artist_id}")}
  end

  @impl true
  def handle_event("view_album", %{"id" => album_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/album/#{album_id}")}
  end

  defp get_user_from_session(%{"session_uuid" => session_uuid}) when session_uuid != nil do
    case Account.Users.get_by_session_uuid(session_uuid) do
      {:ok, user} -> user
      _ -> nil
    end
  end

  defp get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    charlist_id = if is_list(user_id), do: user_id, else: String.to_charlist(user_id)

    case Core.UserClient.get_user_by_id(charlist_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(_), do: nil

  defp get_username(nil), do: ""

  defp get_username(%Account.User{} = user) do
    case user.username do
      u when is_binary(u) and u != "" -> u
      _ -> ""
    end
  end

  defp get_username(user) when is_tuple(user) do
    username = elem(user, 8)

    case username do
      u when is_list(u) ->
        str = List.to_string(u)
        if String.starts_with?(str, ["/ip4", "/ip6"]), do: "", else: str

      u when is_binary(u) ->
        if String.starts_with?(u, ["/ip4", "/ip6"]), do: "", else: u

      _ ->
        ""
    end
  end

  defp get_username(_), do: ""

  defp is_admin_user(username) when is_binary(username) and username != "" do
    admin_usernames = ["arvand", "mazaryn", "zaryn"]
    normalized = username |> String.trim() |> String.downcase()
    Enum.member?(admin_usernames, normalized)
  end

  defp is_admin_user(_), do: false

  defp load_music_data(socket) do
    public_music =
      case :musicdb.get_public_music() do
        music when is_list(music) -> music
        _ -> []
      end

    formatted_music = Enum.map(public_music, &format_music/1)

    trending =
      case :musicdb.get_trending_music(20) do
        music when is_list(music) -> Enum.map(music, &format_music/1)
        _ -> []
      end

    socket
    |> assign(:recently_played, Enum.take(formatted_music, 5))
    |> assign(:trending_tracks, Enum.take(trending, 4))
    |> assign(:popular_albums, get_popular_albums())
    |> assign(:popular_artists, get_popular_artists())
    |> assign(:workout_playlist, get_music_by_genre("workout", formatted_music))
    |> assign(:chill_playlist, get_music_by_genre("chill", formatted_music))
    |> assign(:study_playlist, get_music_by_genre("ambient", formatted_music))
  end

  defp search_music(socket, query) when query != "" do
    case :musicdb.search_music(query) do
      music when is_list(music) ->
        formatted = Enum.map(music, &format_music/1)

        socket
        |> assign(:recently_played, Enum.take(formatted, 5))
        |> assign(:trending_tracks, Enum.take(formatted, 4))

      _ ->
        socket
    end
  end

  defp search_music(socket, _), do: load_music_data(socket)

  defp format_music(music_tuple) do
    music = Mazaryn.Schema.Music.erl_changeset(music_tuple)

    %{
      id: music.changes.id,
      title: music.changes.title || "Untitled",
      artist: get_artist_name(music.changes),
      album: music.changes.album || "Unknown Album",
      album_cover: get_album_cover(music.changes),
      duration: format_duration(music.changes.duration_seconds),
      is_playing: false,
      plays: Map.get(music.changes, :plays, 0),
      genre: music.changes.genre || []
    }
  end

  defp get_artist_name(music) do
    cond do
      Map.has_key?(music, :artist) && music.artist && music.artist != "" ->
        if is_list(music.artist), do: List.to_string(music.artist), else: music.artist

      Map.has_key?(music, :user_id) ->
        get_creator_name(music.user_id)

      true ->
        "Unknown Artist"
    end
  end

  defp get_album_cover(music) do
    cond do
      Map.has_key?(music, :album_art_url) && music.album_art_url && music.album_art_url != "" ->
        music.album_art_url

      Map.has_key?(music, :album_art_cid) && music.album_art_cid && music.album_art_cid != "" ->
        cid =
          if is_list(music.album_art_cid),
            do: List.to_string(music.album_art_cid),
            else: music.album_art_cid

        "https://ipfs.io/ipfs/#{cid}"

      true ->
        "https://placehold.co/200x160"
    end
  end

  defp format_duration(nil), do: "0:00"
  defp format_duration(0), do: "0:00"
  defp format_duration(0.0), do: "0:00"

  defp format_duration(seconds) when is_float(seconds) do
    format_duration(round(seconds))
  end

  defp format_duration(seconds) when is_integer(seconds) do
    minutes = div(seconds, 60)
    secs = rem(seconds, 60)
    "#{minutes}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
  end

  defp format_duration(_), do: "0:00"

  defp get_creator_name(user_id) when is_binary(user_id) do
    get_creator_name(String.to_charlist(user_id))
  end

  defp get_creator_name(user_id) do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _reason} ->
        "Unknown"

      user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 35 ->
        username = elem(user_tuple, 8)

        username_str =
          case username do
            u when is_list(u) and length(u) > 0 ->
              str = List.to_string(u)
              if String.starts_with?(str, ["/ip4", "/ip6"]), do: "Unknown", else: str

            u when is_binary(u) and u != "" ->
              if String.starts_with?(u, ["/ip4", "/ip6"]), do: "Unknown", else: u

            _ ->
              "Unknown"
          end

        username_str

      _ ->
        "Unknown"
    end
  end

  defp get_popular_albums do
    case :musicdb.get_public_music() do
      music when is_list(music) ->
        music
        |> Enum.map(&format_music/1)
        |> Enum.group_by(& &1.album)
        |> Enum.map(fn {album_name, tracks} ->
          first_track = List.first(tracks)

          %{
            id: first_track.id,
            title: album_name,
            artist: first_track.artist,
            album_cover: first_track.album_cover,
            tracks_count: length(tracks)
          }
        end)
        |> Enum.take(3)

      _ ->
        []
    end
  end

  defp get_popular_artists do
    case :musicdb.get_public_music() do
      music when is_list(music) ->
        music
        |> Enum.map(&format_music/1)
        |> Enum.group_by(& &1.artist)
        |> Enum.map(fn {artist_name, tracks} ->
          first_track = List.first(tracks)

          genres =
            tracks
            |> Enum.flat_map(& &1.genre)
            |> Enum.frequencies()
            |> Enum.max_by(fn {_genre, count} -> count end, fn -> {"Unknown", 0} end)
            |> elem(0)

          %{
            id: first_track.id,
            name: artist_name,
            genre: genres,
            avatar: first_track.album_cover
          }
        end)
        |> Enum.take(5)

      _ ->
        []
    end
  end

  defp get_music_by_genre(genre, all_music) do
    all_music
    |> Enum.filter(fn track ->
      Enum.any?(track.genre, fn g ->
        g_str = if is_binary(g), do: g, else: to_string(g)
        String.downcase(g_str) == String.downcase(genre)
      end)
    end)
    |> Enum.take(5)
  end
end
