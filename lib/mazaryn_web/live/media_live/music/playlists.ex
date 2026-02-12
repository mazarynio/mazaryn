defmodule MazarynWeb.MediaLive.Music.Playlists do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(%{"id" => playlist_id}, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Playlist")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:playlist, %{})
     |> assign(:playlist_id, playlist_id)
     |> load_playlist_data(playlist_id)}
  end

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Playlists")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:playlists, [])
     |> load_all_playlists()}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, socket |> assign(:search_query, query) |> search_playlists(query)}
  end

  @impl true
  def handle_event("play_playlist", _params, socket) do
    {:noreply, put_flash(socket, :info, "Playing playlist")}
  end

  @impl true
  def handle_event("play_track", %{"id" => _track_id}, socket) do
    {:noreply, put_flash(socket, :info, "Playing track")}
  end

  @impl true
  def handle_event("view_playlist", %{"id" => playlist_id}, socket) do
    {:noreply,
     push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/playlist/#{playlist_id}")}
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

  defp load_all_playlists(socket) do
    public_music =
      case :musicdb.get_public_music() do
        music when is_list(music) -> music
        _ -> []
      end

    formatted_music = Enum.map(public_music, &format_music/1)

    genre_playlists =
      formatted_music
      |> Enum.flat_map(& &1.genre)
      |> Enum.frequencies()
      |> Enum.map(fn {genre, _count} ->
        genre_tracks =
          Enum.filter(formatted_music, fn track ->
            genre in track.genre
          end)
          |> Enum.take(20)

        first_track = List.first(genre_tracks)

        total_duration =
          Enum.reduce(genre_tracks, 0, fn track, acc ->
            acc + parse_duration(track.duration)
          end)

        %{
          id: String.downcase(genre),
          name: "#{genre} Mix",
          description: "Best #{genre} tracks",
          cover_image:
            if(first_track, do: first_track.album_cover, else: "https://placehold.co/200x200"),
          creator: "Mazaryn",
          creator_avatar: "https://placehold.co/20x20",
          likes: length(genre_tracks),
          track_count: length(genre_tracks),
          duration: format_total_duration(total_duration)
        }
      end)
      |> Enum.take(10)

    socket
    |> assign(:playlists, genre_playlists)
  end

  defp load_playlist_data(socket, playlist_id) do
    public_music =
      case :musicdb.get_public_music() do
        music when is_list(music) -> music
        _ -> []
      end

    formatted_music = Enum.map(public_music, &format_music/1)

    genre = String.capitalize(playlist_id)

    playlist_tracks =
      formatted_music
      |> Enum.filter(fn track ->
        genre in track.genre or
          String.downcase(genre) in Enum.map(track.genre, &String.downcase/1)
      end)
      |> Enum.map(fn track ->
        Map.put(track, :date_added, format_date_added(track.date_created))
      end)
      |> Enum.take(20)

    first_track = List.first(playlist_tracks)

    total_duration =
      Enum.reduce(playlist_tracks, 0, fn track, acc ->
        acc + parse_duration(track.duration)
      end)

    playlist = %{
      id: playlist_id,
      name: "#{genre} Mix",
      description: "The best #{genre} music collection",
      cover_image:
        if(first_track, do: first_track.album_cover, else: "https://placehold.co/200x200"),
      creator: "Mazaryn",
      creator_avatar: "https://placehold.co/20x20",
      likes: length(playlist_tracks),
      duration: format_total_duration(total_duration),
      tracks: playlist_tracks
    }

    socket |> assign(:playlist, playlist)
  end

  defp search_playlists(socket, query) when query != "" do
    public_music =
      case :musicdb.search_music(query) do
        music when is_list(music) -> music
        _ -> []
      end

    formatted_music = Enum.map(public_music, &format_music/1)

    genre_playlists =
      formatted_music
      |> Enum.flat_map(& &1.genre)
      |> Enum.frequencies()
      |> Enum.map(fn {genre, _count} ->
        genre_tracks =
          Enum.filter(formatted_music, fn track ->
            genre in track.genre
          end)
          |> Enum.take(20)

        first_track = List.first(genre_tracks)

        total_duration =
          Enum.reduce(genre_tracks, 0, fn track, acc ->
            acc + parse_duration(track.duration)
          end)

        %{
          id: String.downcase(genre),
          name: "#{genre} Mix",
          description: "Best #{genre} tracks",
          cover_image:
            if(first_track, do: first_track.album_cover, else: "https://placehold.co/200x200"),
          creator: "Mazaryn",
          creator_avatar: "https://placehold.co/20x20",
          likes: length(genre_tracks),
          track_count: length(genre_tracks),
          duration: format_total_duration(total_duration)
        }
      end)
      |> Enum.take(10)

    socket
    |> assign(:playlists, genre_playlists)
  end

  defp search_playlists(socket, _), do: load_all_playlists(socket)

  defp format_music(music_tuple) do
    music = Mazaryn.Schema.Music.erl_changeset(music_tuple)

    %{
      id: music.changes.id,
      title: music.changes.title || "Untitled",
      artist: get_artist_name(music.changes),
      album: music.changes.album || "Unknown Album",
      album_cover: get_album_cover(music.changes),
      duration: format_duration(music.changes.duration_seconds),
      plays: Map.get(music.changes, :plays, 0),
      genre: music.changes.genre || [],
      date_created: Map.get(music.changes, :date_created)
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
        "https://placehold.co/200x200"
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

  defp parse_duration(duration_str) when is_binary(duration_str) do
    case String.split(duration_str, ":") do
      [minutes, seconds] ->
        String.to_integer(minutes) * 60 + String.to_integer(seconds)

      _ ->
        0
    end
  end

  defp parse_duration(_), do: 0

  defp format_total_duration(seconds) do
    hours = div(seconds, 3600)
    minutes = div(rem(seconds, 3600), 60)

    cond do
      hours > 0 -> "about #{hours} hr #{minutes} min"
      minutes > 0 -> "about #{minutes} min"
      true -> "less than 1 min"
    end
  end

  defp format_date_added(nil), do: "Unknown"

  defp format_date_added(%NaiveDateTime{} = datetime) do
    month = String.pad_leading(Integer.to_string(datetime.month), 2, "0")
    year = Integer.to_string(datetime.year)
    "#{month}-#{year}"
  end

  defp format_date_added(_), do: "Unknown"

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
end
