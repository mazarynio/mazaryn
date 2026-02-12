defmodule MazarynWeb.MediaLive.Music.Albums do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Albums")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:popular_albums, [])
     |> load_albums_data()}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, socket |> assign(:search_query, query) |> search_albums(query)}
  end

  @impl true
  def handle_event("view_album", %{"id" => album_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/album/#{album_id}")}
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

  defp load_albums_data(socket) do
    public_music =
      case :musicdb.get_public_music() do
        music when is_list(music) -> music
        _ -> []
      end

    formatted_music = Enum.map(public_music, &format_music/1)

    albums_data =
      formatted_music
      |> Enum.filter(fn track ->
        track.album && track.album != "Unknown Album" && track.album != ""
      end)
      |> Enum.group_by(& &1.album)
      |> Enum.map(fn {album_name, tracks} ->
        first_track = List.first(tracks)
        total_plays = Enum.reduce(tracks, 0, fn track, acc -> acc + track.plays end)

        %{
          id: first_track.id,
          title: album_name,
          artist: first_track.artist,
          album_cover: first_track.album_cover,
          tracks_count: length(tracks),
          total_plays: total_plays,
          release_date: first_track.date_created
        }
      end)
      |> Enum.sort_by(& &1.total_plays, :desc)
      |> Enum.take(15)

    socket
    |> assign(:popular_albums, albums_data)
  end

  defp search_albums(socket, query) when query != "" do
    public_music =
      case :musicdb.search_music(query) do
        music when is_list(music) -> music
        _ -> []
      end

    formatted_music = Enum.map(public_music, &format_music/1)

    albums_data =
      formatted_music
      |> Enum.filter(fn track ->
        track.album && track.album != "Unknown Album" && track.album != ""
      end)
      |> Enum.group_by(& &1.album)
      |> Enum.map(fn {album_name, tracks} ->
        first_track = List.first(tracks)
        total_plays = Enum.reduce(tracks, 0, fn track, acc -> acc + track.plays end)

        %{
          id: first_track.id,
          title: album_name,
          artist: first_track.artist,
          album_cover: first_track.album_cover,
          tracks_count: length(tracks),
          total_plays: total_plays,
          release_date: first_track.date_created
        }
      end)
      |> Enum.sort_by(& &1.total_plays, :desc)
      |> Enum.take(15)

    socket
    |> assign(:popular_albums, albums_data)
  end

  defp search_albums(socket, _), do: load_albums_data(socket)

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
