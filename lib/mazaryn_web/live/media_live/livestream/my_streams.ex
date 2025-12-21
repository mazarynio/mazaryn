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
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    {:noreply, socket |> assign(:active_tab, tab) |> filter_streams(tab)}
  end

  @impl true
  def handle_event("view_stream", %{"id" => stream_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/livestreams/#{stream_id}")}
  end

  @impl true
  def handle_event("view_dashboard", %{"id" => stream_id}, socket) do
    {:noreply,
     push_navigate(socket, to: ~p"/#{socket.assigns.locale}/livestreams/#{stream_id}/dashboard")}
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

      Logger.info(
        "Attempting to delete stream: #{inspect(stream.id)} for user: #{inspect(user_id_bin)}"
      )

      case :livestreamdb.delete_livestream(stream.id, user_id_bin) do
        {:ok, :deleted} ->
          Logger.info("Stream deleted successfully: #{stream.id}")

          {:noreply,
           socket
           |> assign(:selected_stream, nil)
           |> assign(:show_delete_modal, false)
           |> put_flash(:info, "Stream '#{stream.title}' deleted successfully")
           |> load_streams()}

        {:error, :unauthorized} ->
          Logger.warning("Unauthorized delete attempt for stream: #{stream.id}")

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> put_flash(:error, "You don't have permission to delete this stream")}

        {:error, :not_found} ->
          Logger.warning("Stream not found: #{stream.id}")

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> put_flash(:error, "Stream not found or already deleted")
           |> load_streams()}

        {:error, reason} ->
          Logger.error("Failed to delete stream: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> put_flash(:error, "Failed to delete stream: #{inspect(reason)}")}
      end
    else
      Logger.warning("No stream selected for deletion")
      {:noreply, socket |> assign(:show_delete_modal, false)}
    end
  end

  @impl true
  def handle_event("download_vod", %{"id" => stream_id}, socket) do
    # ... (unchanged, keeping your existing download logic)
    Logger.info("Download VOD requested for stream: #{stream_id}")

    charlist_stream_id = String.to_charlist(stream_id)

    case :livestreamdb.get_livestream(charlist_stream_id) do
      {:ok, stream_tuple} ->
        vod_cid =
          safe_elem(stream_tuple, 57) ||
            safe_elem(stream_tuple, 17) ||
            safe_elem(stream_tuple, 55)

        cond do
          is_nil(vod_cid) or vod_cid == :undefined or vod_cid == "" ->
            {:noreply, put_flash(socket, :error, "Recording not available for this stream")}

          true ->
            cid_str =
              cond do
                is_list(vod_cid) -> List.to_string(vod_cid)
                is_binary(vod_cid) -> vod_cid
                true -> to_string(vod_cid)
              end

            clean_cid = String.trim(cid_str)

            if String.length(clean_cid) > 10 do
              gateways = [
                "https://ipfs.io/ipfs/#{clean_cid}",
                "https://gateway.pinata.cloud/ipfs/#{clean_cid}",
                "https://cloudflare-ipfs.com/ipfs/#{clean_cid}"
              ]

              vod_url = List.first(gateways)

              {:noreply,
               socket
               |> push_event("download_file", %{
                 url: vod_url,
                 filename: "mazaryn_stream_#{stream_id}.mp4"
               })
               |> put_flash(:info, "Download started! Check your browser downloads.")}
            else
              {:noreply, put_flash(socket, :error, "Invalid recording ID")}
            end
        end

      _ ->
        {:noreply, put_flash(socket, :error, "Failed to load stream data")}
    end
  end

  @impl true
  def handle_event("navigate_to_go_live", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/livestreams/go-live")}
  end

  # ────────────────────────────────────────────────────────────────────────────────
  # Private helpers
  # ────────────────────────────────────────────────────────────────────────────────

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

  defp get_user_id(%{id: db_id}) when not is_nil(db_id) do
    to_string(db_id)
  end

  defp get_user_id(user_tuple) when is_tuple(user_tuple) do
    elem(user_tuple, 1) |> to_string()
  end

  defp get_user_id(_), do: nil

  defp load_streams(socket) do
    user_id_bin = get_user_id(socket.assigns.user)

    if user_id_bin do
      case :livestreamdb.get_all_livestreams() do
        {:ok, all_streams} when is_list(all_streams) ->
          user_streams =
            all_streams
            |> Enum.filter(fn stream_tuple ->
              stream_user_id = safe_elem(stream_tuple, 2)
              to_string(stream_user_id) == user_id_bin
            end)
            |> Enum.map(&format_stream/1)
            |> Enum.reject(&is_nil/1)

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
    filtered =
      Enum.filter(socket.assigns.user_streams, fn stream ->
        stream.status == :ended && stream.has_vod
      end)

    assign(socket, :user_streams, filtered)
  end

  defp format_stream(stream_tuple) when is_tuple(stream_tuple) do
    stream_id = safe_elem(stream_tuple, 1)
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

    vod_cid = safe_elem(stream_tuple, 57) || safe_elem(stream_tuple, 17)

    %{
      id: if(is_binary(stream_id), do: stream_id, else: to_string(stream_id)),
      title: if(is_binary(title), do: title, else: to_string(title || "Untitled Stream")),
      description:
        if(is_binary(description), do: description, else: to_string(description || "")),
      thumbnail_url: get_thumbnail_url(thumbnail_cid),
      category: format_category(category),
      status: status,
      current_viewers: viewers,
      peak_viewers: peak,
      total_views: total,
      started_at: started_at,
      ended_at: ended_at,
      duration: calculate_duration_from_times(started_at, ended_at),
      has_vod: has_vod?(vod_cid)
    }
  end

  # ... (rest of your existing helper functions remain unchanged)
  defp get_thumbnail_url(thumbnail_cid) when is_binary(thumbnail_cid) and thumbnail_cid != "" do
    "https://ipfs.io/ipfs/#{thumbnail_cid}"
  end

  defp get_thumbnail_url(thumbnail_cid)
       when is_list(thumbnail_cid) and length(thumbnail_cid) > 0 do
    "https://ipfs.io/ipfs/#{List.to_string(thumbnail_cid)}"
  end

  defp get_thumbnail_url(:undefined), do: "/images/default-stream-thumbnail.png"
  defp get_thumbnail_url(nil), do: "/images/default-stream-thumbnail.png"
  defp get_thumbnail_url(false), do: "/images/default-stream-thumbnail.png"
  defp get_thumbnail_url(_), do: "/images/default-stream-thumbnail.png"

  defp format_category(category) when is_atom(category), do: Atom.to_string(category)
  defp format_category(category) when is_binary(category), do: category
  defp format_category(category) when is_list(category), do: List.to_string(category)
  defp format_category(_), do: "other"

  defp calculate_duration_from_times(false, _), do: "0:00"
  defp calculate_duration_from_times(_, false), do: "0:00"
  defp calculate_duration_from_times(nil, _), do: "0:00"
  defp calculate_duration_from_times(_, nil), do: "0:00"
  defp calculate_duration_from_times(:undefined, _), do: "0:00"
  defp calculate_duration_from_times(_, :undefined), do: "0:00"

  defp calculate_duration_from_times(started_at, ended_at)
       when is_tuple(started_at) and is_tuple(ended_at) do
    try do
      naive_started = naive_from_gregorian(started_at)
      naive_ended = naive_from_gregorian(ended_at)
      diff_seconds = NaiveDateTime.diff(naive_ended, naive_started)
      format_duration_seconds(diff_seconds)
    catch
      _, _ -> "0:00"
    end
  end

  defp calculate_duration_from_times(started_at, _) when is_tuple(started_at) do
    try do
      naive_started = naive_from_gregorian(started_at)
      now = NaiveDateTime.utc_now()
      diff_seconds = NaiveDateTime.diff(now, naive_started)
      format_duration_seconds(diff_seconds)
    catch
      _, _ -> "0:00"
    end
  end

  defp calculate_duration_from_times(_, _), do: "0:00"

  defp naive_from_gregorian({{year, month, day}, {hour, minute, second}}) do
    {:ok, naive} = NaiveDateTime.new(year, month, day, hour, minute, second)
    naive
  end

  defp format_duration_seconds(seconds) when seconds < 60,
    do: "0:#{String.pad_leading(Integer.to_string(seconds), 2, "0")}"

  defp format_duration_seconds(seconds) do
    hours = div(seconds, 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    if hours > 0 do
      "#{hours}:#{String.pad_leading(Integer.to_string(minutes), 2, "0")}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
    else
      "#{minutes}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
    end
  end

  defp has_vod?(vod_cid) when is_binary(vod_cid) and vod_cid != "", do: true
  defp has_vod?(vod_cid) when is_list(vod_cid) and length(vod_cid) > 0, do: true
  defp has_vod?(:undefined), do: false
  defp has_vod?(nil), do: false
  defp has_vod?(false), do: false
  defp has_vod?(_), do: false

  defp calculate_stats(streams) do
    total = length(streams)
    live = Enum.count(streams, &(&1.status == :live))
    ended = Enum.count(streams, &(&1.status == :ended))
    recorded = Enum.count(streams, &(&1.status == :ended && &1.has_vod))
    total_views = Enum.reduce(streams, 0, fn stream, acc -> acc + stream.total_views end)

    %{total: total, live: live, ended: ended, recorded: recorded, total_views: total_views}
  end

  defp format_date(false), do: "N/A"
  defp format_date(nil), do: "N/A"
  defp format_date(:undefined), do: "N/A"

  defp format_date(datetime) when is_tuple(datetime) do
    try do
      naive = naive_from_gregorian(datetime)
      Calendar.strftime(naive, "%B %d, %Y")
    catch
      _, _ -> "N/A"
    end
  end

  defp format_date(_), do: "N/A"

  defp safe_elem(tuple, index) when is_tuple(tuple) and is_integer(index) and index >= 0 do
    if index < tuple_size(tuple) do
      elem(tuple, index)
    else
      nil
    end
  end

  defp safe_elem(_, _), do: nil

  defp format_viewers(num) when is_integer(num) do
    cond do
      num >= 1_000_000 -> "#{Float.round(num / 1_000_000, 1)}M"
      num >= 1_000 -> "#{Float.round(num / 1_000, 1)}K"
      true -> "#{num}"
    end
  end

  defp format_viewers(_), do: "0"
end
