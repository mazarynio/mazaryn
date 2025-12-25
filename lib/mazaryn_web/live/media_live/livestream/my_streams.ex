defmodule MazarynWeb.MediaLive.Livestream.MyStreams do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)
    if is_nil(user) do
      {:ok,
       socket
       |> put_flash(:error, "Please login to view your streams")
       |> push_navigate(to: ~p"/#{socket.assigns[:locale] || "en"}/login")}
    else
      if connected?(socket) do
        Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestreams:global")
      end

      {:ok,
       socket
       |> assign(:page_title, "My Streams")
       |> assign(:user, user)
       |> assign(:current_user, user)
       |> assign(:locale, socket.assigns[:locale] || "en")
       |> assign(:active_tab, "all")
       |> assign(:user_streams, [])
       |> assign(:selected_stream, nil)
       |> assign(:show_delete_modal, false)
       |> assign(:stats, %{total: 0, live: 0, ended: 0, recorded: 0, total_views: 0})
       |> load_streams()}
    end
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_info({:recording_ready, stream_id}, socket) do
    Logger.info("DEBUG: [MyStreams] Recording ready for stream #{stream_id} - reloading streams")
    {:noreply, load_streams(socket)}
  end

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    {:noreply, socket |> assign(:active_tab, tab) |> filter_streams(tab)}
  end

  @impl true
  def handle_event("view_stream", %{"id" => stream_id}, socket) do
    Logger.info("🎬 [MyStreams] view_stream clicked for stream: #{stream_id}")
    stream = Enum.find(socket.assigns.user_streams, &(&1.id == stream_id))

    if stream do
      Logger.info("🎬 [MyStreams] Stream: #{stream.title}, Status: #{stream.status}, VOD: #{inspect(stream.vod_cid)}")
    end

    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/livestreams/#{stream_id}")}
  end

  @impl true
  def handle_event("view_dashboard", %{"id" => stream_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/livestreams/#{stream_id}/dashboard")}
  end

  @impl true
  def handle_event("open_delete_modal", %{"id" => stream_id}, socket) do
    stream = Enum.find(socket.assigns.user_streams, &(&1.id == stream_id))
    {:noreply, socket |> assign(:selected_stream, stream) |> assign(:show_delete_modal, true)}
  end

  @impl true
  def handle_event("close_delete_modal", _params, socket) do
    {:noreply, socket |> assign(:selected_stream, nil) |> assign(:show_delete_modal, false)}
  end

  @impl true
  def handle_event("delete_stream", _params, socket) do
    stream = socket.assigns.selected_stream

    if stream do
      user_id_bin = get_user_id(socket.assigns.user)
      stream_id = if is_binary(stream.id), do: stream.id, else: to_string(stream.id)

      case :livestreamdb.delete_livestream(stream_id, user_id_bin) do
        {:ok, :deleted} ->
          {:noreply,
           socket
           |> assign(:selected_stream, nil)
           |> assign(:show_delete_modal, false)
           |> put_flash(:info, "Stream deleted successfully")
           |> load_streams()}
        {:error, reason} ->
          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> put_flash(:error, "Failed to delete stream: #{inspect(reason)}")}
      end
    else
      {:noreply, socket |> assign(:show_delete_modal, false)}
    end
  end

  @impl true
  def handle_event("download_vod", %{"id" => stream_id}, socket) do
    stream = Enum.find(socket.assigns.user_streams, &(&1.id == stream_id))

    if stream && stream.vod_cid && stream.vod_cid != "" do
      vod_url = "https://ipfs.io/ipfs/#{stream.vod_cid}"

      {:noreply,
       socket
       |> push_event("download_file", %{url: vod_url, filename: "stream_#{stream_id}.mp4"})
       |> put_flash(:info, "Download started!")}
    else
      {:noreply, put_flash(socket, :error, "No recording available")}
    end
  end

  @impl true
  def handle_event("navigate_to_go_live", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/livestreams/go-live")}
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

  defp get_user_id(%{id: db_id}) when not is_nil(db_id), do: to_string(db_id)
  defp get_user_id(user_tuple) when is_tuple(user_tuple), do: elem(user_tuple, 1) |> to_string()
  defp get_user_id(_), do: nil

  defp load_streams(socket) do
    Logger.info("🔄 [MyStreams] Loading streams...")
    user_id_bin = get_user_id(socket.assigns.user)

    if user_id_bin do
      case :livestreamdb.get_all_livestreams() do
        {:ok, all_streams} when is_list(all_streams) ->
          user_streams =
            all_streams
            |> Enum.filter(fn stream_tuple ->
              to_string(safe_elem(stream_tuple, 2)) == user_id_bin
            end)
            |> Enum.map(&format_stream/1)
            |> Enum.reject(&is_nil/1)
            |> Enum.sort_by(& &1.started_at_ts, :desc)

          Logger.info("✅ [MyStreams] Loaded #{length(user_streams)} streams")

          stats = calculate_stats(user_streams)

          socket
          |> assign(:user_streams, user_streams)
          |> assign(:stats, stats)
        _ ->
          socket
          |> assign(:user_streams, [])
          |> assign(:stats, %{total: 0, live: 0, ended: 0, recorded: 0, total_views: 0})
      end
    else
      socket
      |> assign(:user_streams, [])
      |> assign(:stats, %{total: 0, live: 0, ended: 0, recorded: 0, total_views: 0})
    end
  end

  defp filter_streams(socket, "all"), do: load_streams(socket)
  defp filter_streams(socket, "live") do
    filtered = Enum.filter(socket.assigns.user_streams, &(&1.status == :live))
    assign(socket, :user_streams, filtered)
  end
  defp filter_streams(socket, "ended") do
    filtered = Enum.filter(socket.assigns.user_streams, &(&1.status == :ended))
    assign(socket, :user_streams, filtered)
  end
  defp filter_streams(socket, "recorded") do
    filtered = Enum.filter(socket.assigns.user_streams, fn stream ->
      stream.status == :ended && has_vod?(stream.vod_cid)
    end)
    assign(socket, :user_streams, filtered)
  end

  defp format_stream(stream_tuple) when is_tuple(stream_tuple) do
    stream_id = safe_elem(stream_tuple, 1)
    stream_id_bin = if is_binary(stream_id), do: stream_id, else: to_string(stream_id)

    vod_cid = case :livestreamdb.get_recording_cid(stream_id) do
      {:ok, cid} when is_list(cid) and length(cid) > 0 ->
        cid_str = List.to_string(cid)
        if String.length(cid_str) > 10, do: cid_str, else: nil
      {:ok, cid} when is_binary(cid) and byte_size(cid) > 10 ->
        String.trim(cid)
      _ ->
        nil
    end

    Logger.info("📹 Stream: #{stream_id_bin}, VOD CID: #{inspect(vod_cid)}")

    title = safe_elem(stream_tuple, 3)
    description = safe_elem(stream_tuple, 4)
    thumbnail_cid = safe_elem(stream_tuple, 5)
    status = safe_elem(stream_tuple, 7)
    category = safe_elem(stream_tuple, 10)
    viewers = safe_elem(stream_tuple, 21) || 0
    peak = safe_elem(stream_tuple, 22) || 0
    total = safe_elem(stream_tuple, 23) || 0
    started_at = safe_elem(stream_tuple, 47)
    ended_at = safe_elem(stream_tuple, 48)

    started_at_ts = case started_at do
      {{_, _, _}, {_, _, _}} ->
        try do
          naive_from_gregorian(started_at)
          |> DateTime.from_naive!("Etc/UTC")
          |> DateTime.to_unix()
        catch
          _, _ -> 0
        end
      _ -> 0
    end

    %{
      id: stream_id_bin,
      title: if(is_binary(title), do: title, else: to_string(title || "Untitled Stream")),
      description: if(is_binary(description), do: description, else: to_string(description || "")),
      thumbnail_url: get_thumbnail_url(thumbnail_cid),
      category: format_category(category),
      status: status,
      current_viewers: viewers,
      peak_viewers: peak,
      total_views: total,
      started_at: started_at,
      started_at_ts: started_at_ts,
      ended_at: ended_at,
      duration: format_duration_from_times(started_at, ended_at),
      vod_cid: vod_cid
    }
  end

  defp has_vod?(vod_cid) when is_binary(vod_cid) and vod_cid != "", do: true
  defp has_vod?(_), do: false

  defp get_thumbnail_url(cid) when is_binary(cid) and cid != "", do: "https://ipfs.io/ipfs/#{cid}"
  defp get_thumbnail_url(cid) when is_list(cid) and length(cid) > 0, do: "https://ipfs.io/ipfs/#{List.to_string(cid)}"
  defp get_thumbnail_url(_), do: "/images/default-stream-thumbnail.png"

  defp format_category(cat) when is_atom(cat), do: Atom.to_string(cat)
  defp format_category(cat) when is_binary(cat), do: cat
  defp format_category(cat) when is_list(cat), do: List.to_string(cat)
  defp format_category(_), do: "other"

  defp format_duration_from_times(started, ended) when is_tuple(started) and is_tuple(ended) do
    try do
      diff = NaiveDateTime.diff(naive_from_gregorian(ended), naive_from_gregorian(started))
      format_duration_seconds(diff)
    catch
      _, _ -> "0:00"
    end
  end
  defp format_duration_from_times(started, _) when is_tuple(started) do
    try do
      diff = NaiveDateTime.diff(NaiveDateTime.utc_now(), naive_from_gregorian(started))
      format_duration_seconds(diff)
    catch
      _, _ -> "0:00"
    end
  end
  defp format_duration_from_times(_, _), do: "0:00"

  defp naive_from_gregorian({{y, m, d}, {h, min, s}}) do
    {:ok, naive} = NaiveDateTime.new(y, m, d, h, min, s)
    naive
  end

  defp format_duration_seconds(s) when s < 60, do: "0:#{String.pad_leading(Integer.to_string(s), 2, "0")}"
  defp format_duration_seconds(s) do
    h = div(s, 3600)
    m = div(rem(s, 3600), 60)
    sec = rem(s, 60)

    if h > 0 do
      "#{h}:#{String.pad_leading(Integer.to_string(m), 2, "0")}:#{String.pad_leading(Integer.to_string(sec), 2, "0")}"
    else
      "#{m}:#{String.pad_leading(Integer.to_string(sec), 2, "0")}"
    end
  end

  defp calculate_stats(streams) do
    %{
      total: length(streams),
      live: Enum.count(streams, &(&1.status == :live)),
      ended: Enum.count(streams, &(&1.status == :ended)),
      recorded: Enum.count(streams, fn s -> s.status == :ended && has_vod?(s.vod_cid) end),
      total_views: Enum.reduce(streams, 0, fn s, acc -> acc + s.total_views end)
    }
  end

  defp format_date(dt) when is_tuple(dt) do
    try do
      naive_from_gregorian(dt) |> Calendar.strftime("%B %d, %Y")
    catch
      _, _ -> "N/A"
    end
  end
  defp format_date(_), do: "N/A"

  defp safe_elem(tuple, index) when is_tuple(tuple) and index < tuple_size(tuple), do: elem(tuple, index)
  defp safe_elem(_, _), do: nil

  defp format_viewers(num) when is_integer(num) and num >= 1_000_000, do: "#{Float.round(num / 1_000_000, 1)}M"
  defp format_viewers(num) when is_integer(num) and num >= 1_000, do: "#{Float.round(num / 1_000, 1)}K"
  defp format_viewers(num) when is_integer(num), do: "#{num}"
  defp format_viewers(_), do: "0"
end
