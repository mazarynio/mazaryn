defmodule MazarynWeb.MediaLive.Music.MyTracks do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Your Tracks")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:selected_tracks, [])
     |> assign(:tracks, [])
     |> load_user_tracks()}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, socket |> assign(:search_query, query) |> search_user_tracks(query)}
  end

  @impl true
  def handle_event("toggle_track_selection", %{"value" => track_id}, socket) do
    selected_tracks = socket.assigns.selected_tracks

    updated_selected_tracks =
      if track_id in selected_tracks do
        List.delete(selected_tracks, track_id)
      else
        [track_id | selected_tracks]
      end

    {:noreply, socket |> assign(:selected_tracks, updated_selected_tracks)}
  end

  @impl true
  def handle_event("add_to_playlist", _params, socket) do
    selected_count = length(socket.assigns.selected_tracks)

    if selected_count > 0 do
      Logger.info("Adding #{selected_count} tracks to playlist")

      {:noreply,
       socket
       |> put_flash(:info, "Added #{selected_count} track(s) to playlist!")
       |> assign(:selected_tracks, [])}
    else
      {:noreply,
       socket
       |> put_flash(:error, "Please select tracks to add to playlist")}
    end
  end

  @impl true
  def handle_event("delete_track", %{"id" => track_id}, socket) do
    user = socket.assigns.user
    user_id = get_user_id(user)

    case :musicdb.delete_music(track_id, user_id) do
      :ok ->
        {:noreply,
         socket
         |> load_user_tracks()
         |> put_flash(:info, "Track deleted successfully")}

      {:error, reason} ->
        Logger.error("Failed to delete track: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to delete track")}
    end
  end

  @impl true
  def handle_event("play_track", %{"id" => track_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/music/song/#{track_id}")}
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

  defp get_user_id(user) when is_tuple(user) do
    elem(user, 1)
  end

  defp get_user_id(_), do: nil

  defp load_user_tracks(socket) do
    user = socket.assigns.user

    if user do
      user_id = get_user_id(user)

      user_music =
        case :musicdb.get_music_by_user(user_id) do
          music when is_list(music) -> music
          _ -> []
        end

      formatted_tracks =
        user_music
        |> Enum.map(&format_music/1)
        |> Enum.sort_by(& &1.date_created, {:desc, NaiveDateTime})

      socket |> assign(:tracks, formatted_tracks)
    else
      socket |> assign(:tracks, [])
    end
  end

  defp search_user_tracks(socket, query) when query != "" do
    user = socket.assigns.user

    if user do
      user_id = get_user_id(user)

      user_music =
        case :musicdb.get_music_by_user(user_id) do
          music when is_list(music) -> music
          _ -> []
        end

      formatted_tracks =
        user_music
        |> Enum.map(&format_music/1)
        |> Enum.filter(fn track ->
          query_lower = String.downcase(query)

          String.contains?(String.downcase(track.title), query_lower) ||
            String.contains?(String.downcase(track.artist), query_lower) ||
            String.contains?(String.downcase(track.album), query_lower)
        end)
        |> Enum.sort_by(& &1.date_created, {:desc, NaiveDateTime})

      socket |> assign(:tracks, formatted_tracks)
    else
      socket |> assign(:tracks, [])
    end
  end

  defp search_user_tracks(socket, _), do: load_user_tracks(socket)

  defp format_music(music_tuple) do
    music = Mazaryn.Schema.Music.erl_changeset(music_tuple)

    %{
      id: music.changes.id,
      title: music.changes.title || "Untitled",
      artist: get_artist_name(music.changes),
      album: music.changes.album || "Unknown Album",
      cover_image: get_album_cover(music.changes),
      duration: format_duration(music.changes.duration_seconds),
      uploaded_at: format_uploaded_at(music.changes.date_created),
      date_created: Map.get(music.changes, :date_created),
      plays: Map.get(music.changes, :plays, 0)
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
        "https://placehold.co/92x60"
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

  defp format_uploaded_at(nil), do: "Unknown"

  defp format_uploaded_at(%NaiveDateTime{} = datetime) do
    now = NaiveDateTime.utc_now()
    diff_seconds = NaiveDateTime.diff(now, datetime, :second)

    cond do
      diff_seconds < 60 ->
        "Just now"

      diff_seconds < 3600 ->
        minutes = div(diff_seconds, 60)
        "#{minutes} minute#{if minutes == 1, do: "", else: "s"} ago"

      diff_seconds < 86400 ->
        hours = div(diff_seconds, 3600)
        "#{hours} hour#{if hours == 1, do: "", else: "s"} ago"

      diff_seconds < 604_800 ->
        days = div(diff_seconds, 86400)
        "#{days} day#{if days == 1, do: "", else: "s"} ago"

      diff_seconds < 2_592_000 ->
        weeks = div(diff_seconds, 604_800)
        "#{weeks} week#{if weeks == 1, do: "", else: "s"} ago"

      diff_seconds < 31_536_000 ->
        months = div(diff_seconds, 2_592_000)
        "#{months} month#{if months == 1, do: "", else: "s"} ago"

      true ->
        years = div(diff_seconds, 31_536_000)
        "#{years} year#{if years == 1, do: "", else: "s"} ago"
    end
  end

  defp format_uploaded_at(_), do: "Unknown"

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
