defmodule MazarynWeb.MediaLive.Livestream.Index do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestreams:global")
    end

    {:ok,
     socket
     |> assign(:page_title, "Live Streams")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:active_category, "all")
     |> assign(:live_streams, [])
     |> assign(:featured_stream, nil)
     |> load_streams()}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("search", %{"search" => query}, socket) do
    {:noreply, socket |> assign(:search_query, query) |> search_streams(query)}
  end

  @impl true
  def handle_event("filter_category", %{"category" => category}, socket) do
    {:noreply, socket |> assign(:active_category, category) |> filter_by_category(category)}
  end

  @impl true
  def handle_event("watch_stream", %{"id" => stream_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/livestreams/#{stream_id}")}
  end

  @impl true
  def handle_event("navigate_to_go_live", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/livestreams/go-live")}
  end

  @impl true
  def handle_info({:stream_started, stream_data}, socket) do
    {:noreply, load_streams(socket)}
  end

  @impl true
  def handle_info({:stream_ended, stream_id}, socket) do
    streams = Enum.reject(socket.assigns.live_streams, &(&1.id == stream_id))
    {:noreply, assign(socket, :live_streams, streams)}
  end

  @impl true
  def handle_info({:viewer_update, stream_id, viewer_count}, socket) do
    streams =
      Enum.map(socket.assigns.live_streams, fn stream ->
        if stream.id == stream_id do
          Map.put(stream, :current_viewers, viewer_count)
        else
          stream
        end
      end)

    {:noreply, assign(socket, :live_streams, streams)}
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
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(_), do: nil

  defp load_streams(socket) do
    case :livestreamdb.get_live_streams() do
      streams when is_list(streams) ->
        formatted = Enum.map(streams, &format_stream/1)
        featured = List.first(Enum.sort_by(formatted, & &1.current_viewers, :desc))

        socket
        |> assign(:live_streams, formatted)
        |> assign(:featured_stream, featured)

      _ ->
        socket
        |> assign(:live_streams, [])
        |> assign(:featured_stream, nil)
    end
  end

  defp search_streams(socket, query) when query != "" do
    case :livestreamdb.search_streams(query) do
      streams when is_list(streams) ->
        formatted = Enum.map(streams, &format_stream/1)
        assign(socket, :live_streams, formatted)

      _ ->
        assign(socket, :live_streams, [])
    end
  end

  defp search_streams(socket, _), do: load_streams(socket)

  defp filter_by_category(socket, "all"), do: load_streams(socket)

  defp filter_by_category(socket, category) do
    category_binary = if is_binary(category), do: category, else: to_string(category)

    case :livestreamdb.get_live_streams_by_category(category_binary) do
      streams when is_list(streams) ->
        formatted = Enum.map(streams, &format_stream/1)
        assign(socket, :live_streams, formatted)

      _ ->
        assign(socket, :live_streams, [])
    end
  end

  defp format_stream(stream_tuple) when is_tuple(stream_tuple) do
    %{
      id: safe_elem(stream_tuple, 1) |> to_string_safe(),
      title: safe_elem(stream_tuple, 3) |> to_string_safe("Untitled Stream"),
      description: safe_elem(stream_tuple, 4) |> to_string_safe(""),
      thumbnail_url: get_thumbnail_url(safe_elem(stream_tuple, 5)),
      creator_id: safe_elem(stream_tuple, 2) |> to_string_safe(),
      creator_name: get_creator_name(safe_elem(stream_tuple, 2)),
      creator_avatar: get_creator_avatar(safe_elem(stream_tuple, 2)),
      category: format_category(safe_elem(stream_tuple, 10)),
      current_viewers: safe_elem(stream_tuple, 21, 0),
      peak_viewers: safe_elem(stream_tuple, 22, 0),
      total_views: safe_elem(stream_tuple, 23, 0),
      started_at: safe_elem(stream_tuple, 47),
      duration: calculate_duration_safe(stream_tuple),
      status: safe_elem(stream_tuple, 7, :ended),
      tags: safe_elem(stream_tuple, 9, [])
    }
  end

  defp safe_elem(tuple, index, default \\ nil) do
    try do
      elem(tuple, index)
    catch
      _, _ -> default
    end
  end

  defp to_string_safe(value, default \\ "")
  defp to_string_safe(value, _default) when is_binary(value), do: value
  defp to_string_safe(value, _default) when is_list(value), do: List.to_string(value)
  defp to_string_safe(value, _default) when is_atom(value), do: Atom.to_string(value)
  defp to_string_safe(nil, default), do: default
  defp to_string_safe(:undefined, default), do: default
  defp to_string_safe(_value, default), do: default

  defp calculate_duration_safe(stream_tuple) do
    try do
      started_at = elem(stream_tuple, 47)
      ended_at = elem(stream_tuple, 48)
      calculate_duration_from_times(started_at, ended_at)
    catch
      _, _ -> "0:00"
    end
  end

  defp get_thumbnail_url(thumbnail_cid) when is_binary(thumbnail_cid) and thumbnail_cid != "" do
    "https://ipfs.io/ipfs/#{thumbnail_cid}"
  end

  defp get_thumbnail_url(thumbnail_cid)
       when is_list(thumbnail_cid) and length(thumbnail_cid) > 0 do
    "https://ipfs.io/ipfs/#{List.to_string(thumbnail_cid)}"
  end

  defp get_thumbnail_url(:undefined), do: "/images/default-stream-thumbnail.png"
  defp get_thumbnail_url(nil), do: "/images/default-stream-thumbnail.png"
  defp get_thumbnail_url(_), do: "/images/default-stream-thumbnail.png"

  defp format_category(category) when is_atom(category), do: Atom.to_string(category)
  defp format_category(category) when is_binary(category), do: category
  defp format_category(category) when is_list(category), do: List.to_string(category)
  defp format_category(_), do: "other"

  defp get_creator_name(user_id) when is_binary(user_id) do
    get_creator_name(String.to_charlist(user_id))
  end

  defp get_creator_name(user_id) do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} ->
        "Unknown"

      user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 35 ->
        username = elem(user_tuple, 8)

        case username do
          u when is_list(u) and length(u) > 0 ->
            str = List.to_string(u)
            if String.starts_with?(str, ["/ip4", "/ip6"]), do: "Unknown", else: str

          u when is_binary(u) and u != "" ->
            if String.starts_with?(u, ["/ip4", "/ip6"]), do: "Unknown", else: u

          _ ->
            "Unknown"
        end

      _ ->
        "Unknown"
    end
  end

  defp get_creator_avatar(user_id) when is_binary(user_id) do
    get_creator_avatar(String.to_charlist(user_id))
  end

  defp get_creator_avatar(user_id) do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} ->
        "/images/default-avatar.png"

      user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 35 ->
        avatar = elem(user_tuple, 26)
        process_avatar(avatar)

      _ ->
        "/images/default-avatar.png"
    end
  end

  defp process_avatar(:undefined), do: "/images/default-avatar.png"

  defp process_avatar(avatar) when is_list(avatar) and length(avatar) > 0 do
    str = List.to_string(avatar)

    cond do
      String.starts_with?(str, ["/ip4", "/ip6"]) ->
        "/images/default-avatar.png"

      String.contains?(str, "ipfs.io/ipfs/") ->
        str

      String.starts_with?(str, "Qm") or String.starts_with?(str, "bafy") ->
        "https://ipfs.io/ipfs/#{str}"

      true ->
        "/images/default-avatar.png"
    end
  end

  defp process_avatar(avatar) when is_binary(avatar) and avatar != "" do
    cond do
      String.starts_with?(avatar, ["/ip4", "/ip6"]) ->
        "/images/default-avatar.png"

      String.contains?(avatar, "ipfs.io/ipfs/") ->
        avatar

      String.starts_with?(avatar, "Qm") or String.starts_with?(avatar, "bafy") ->
        "https://ipfs.io/ipfs/#{avatar}"

      true ->
        "/images/default-avatar.png"
    end
  end

  defp process_avatar(_), do: "/images/default-avatar.png"

  defp calculate_duration_from_times(nil, _), do: "0:00"
  defp calculate_duration_from_times(:undefined, _), do: "0:00"

  defp calculate_duration_from_times(started_at, nil) when is_tuple(started_at) do
    try do
      naive_started = naive_from_gregorian(started_at)
      now = NaiveDateTime.utc_now()
      diff_seconds = NaiveDateTime.diff(now, naive_started)
      format_duration_minutes(diff_seconds)
    catch
      _, _ -> "0:00"
    end
  end

  defp calculate_duration_from_times(started_at, :undefined) when is_tuple(started_at) do
    calculate_duration_from_times(started_at, nil)
  end

  defp calculate_duration_from_times(started_at, ended_at)
       when is_tuple(started_at) and is_tuple(ended_at) do
    try do
      naive_started = naive_from_gregorian(started_at)
      naive_ended = naive_from_gregorian(ended_at)
      diff_seconds = NaiveDateTime.diff(naive_ended, naive_started)
      format_duration_minutes(diff_seconds)
    catch
      _, _ -> "0:00"
    end
  end

  defp calculate_duration_from_times(_, _), do: "0:00"

  defp naive_from_gregorian({{year, month, day}, {hour, minute, second}}) do
    {:ok, naive} = NaiveDateTime.new(year, month, day, hour, minute, second)
    naive
  end

  defp format_duration_minutes(seconds) when seconds < 60, do: "0:00"

  defp format_duration_minutes(seconds) do
    hours = div(seconds, 3600)
    minutes = div(rem(seconds, 3600), 60)

    if hours > 0 do
      "#{hours}:#{String.pad_leading(Integer.to_string(minutes), 2, "0")}"
    else
      "#{minutes}:00"
    end
  end

  defp format_viewers(count) when is_integer(count) and count >= 1_000_000 do
    "#{Float.round(count / 1_000_000, 1)}M"
  end

  defp format_viewers(count) when is_integer(count) and count >= 1_000 do
    "#{Float.round(count / 1_000, 1)}K"
  end

  defp format_viewers(count) when is_integer(count), do: Integer.to_string(count)
  defp format_viewers(_), do: "0"
end
