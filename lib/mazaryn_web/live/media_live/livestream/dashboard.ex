defmodule MazarynWeb.MediaLive.Livestream.Dashboard do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(%{"id" => stream_id}, session, socket) do
    user = get_user_from_session(session)

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestream:#{stream_id}")
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestream:#{stream_id}:viewers")
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestream:#{stream_id}:chat")
    end

    {:ok,
     socket
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:stream_id, stream_id)
     |> assign(:stream, nil)
     |> assign(:is_owner, false)
     |> assign(:show_end_modal, false)
     |> assign(:recent_messages, [])
     |> assign(:viewer_stats, %{current: 0, peak: 0, total: 0})
     |> assign(:reaction_stats, %{})
     |> assign(:viewer_chart_data, [])
     |> assign(:stream_health, %{bitrate: "N/A", fps: "N/A", resolution: "N/A", status: "unknown"})
     |> load_dashboard(stream_id, user)}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("open_end_modal", _params, socket) do
    {:noreply, assign(socket, :show_end_modal, true)}
  end

  @impl true
  def handle_event("close_end_modal", _params, socket) do
    {:noreply, assign(socket, :show_end_modal, false)}
  end

  @impl true
  def handle_event("end_stream", _params, socket) do
    stream_id = socket.assigns.stream_id
    user_id = get_user_id(socket.assigns.user)

    if stream_id && user_id && socket.assigns.is_owner do
      charlist_stream_id = String.to_charlist(stream_id)
      charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

      case :livestreamdb.end_livestream(charlist_stream_id, charlist_user_id) do
        {:ok, duration} ->
          Phoenix.PubSub.broadcast(
            Mazaryn.PubSub,
            "livestream:#{stream_id}",
            {:stream_ended, stream_id}
          )

          Phoenix.PubSub.broadcast(
            Mazaryn.PubSub,
            "livestreams:global",
            {:stream_ended, stream_id}
          )

          {:noreply,
           socket
           |> assign(:show_end_modal, false)
           |> put_flash(
             :info,
             "Stream ended successfully. Duration: #{format_duration(duration)}"
           )
           |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams/my-streams")}

        {:error, reason} ->
          Logger.error("Failed to end stream: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(:show_end_modal, false)
           |> put_flash(:error, "Failed to end stream")}
      end
    else
      {:noreply,
       socket
       |> assign(:show_end_modal, false)
       |> put_flash(:error, "Unauthorized")}
    end
  end

  @impl true
  def handle_event("refresh_stats", _params, socket) do
    stream_id = socket.assigns.stream_id
    {:noreply, load_dashboard(socket, stream_id, socket.assigns.user)}
  end

  @impl true
  def handle_info({:viewer_joined, _user_id}, socket) do
    stats = socket.assigns.viewer_stats
    current = stats.current + 1
    peak = max(current, stats.peak)
    total = stats.total + 1

    updated_stats = %{current: current, peak: peak, total: total}

    {:noreply, assign(socket, :viewer_stats, updated_stats)}
  end

  @impl true
  def handle_info({:viewer_left, _user_id}, socket) do
    stats = socket.assigns.viewer_stats
    current = max(0, stats.current - 1)

    updated_stats = %{stats | current: current}

    {:noreply, assign(socket, :viewer_stats, updated_stats)}
  end

  @impl true
  def handle_info({:new_chat_message, message_data}, socket) do
    new_message = %{
      username: message_data.username,
      message: message_data.message,
      timestamp: message_data.timestamp
    }

    messages = [new_message | socket.assigns.recent_messages] |> Enum.take(10)

    {:noreply, assign(socket, :recent_messages, messages)}
  end

  @impl true
  def handle_info({:new_reaction, reaction_type}, socket) do
    reactions = socket.assigns.reaction_stats
    count = Map.get(reactions, reaction_type, 0)
    updated_reactions = Map.put(reactions, reaction_type, count + 1)

    {:noreply, assign(socket, :reaction_stats, updated_reactions)}
  end

  @impl true
  def handle_info({:stream_ended, _stream_id}, socket) do
    {:noreply,
     socket
     |> put_flash(:info, "Stream has ended")
     |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams/my-streams")}
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

  defp get_user_id(%{id: db_id}) when not is_nil(db_id) do
    charlist_id = String.to_charlist(to_string(db_id))

    case Core.UserClient.get_user_by_id(charlist_id) do
      user_tuple when is_tuple(user_tuple) -> elem(user_tuple, 1)
      _ -> nil
    end
  end

  defp get_user_id(user_tuple) when is_tuple(user_tuple), do: elem(user_tuple, 1)
  defp get_user_id(_), do: nil

  defp load_dashboard(socket, stream_id, user) do
    charlist_stream_id = String.to_charlist(stream_id)

    case :livestreamdb.get_livestream(charlist_stream_id) do
      {:ok, stream_tuple} ->
        stream = format_stream(stream_tuple)
        user_id = get_user_id(user)

        is_owner = user_id && user_id == stream.user_id

        if is_owner || stream.status != :live do
          viewer_stats = load_viewer_stats(charlist_stream_id)
          reaction_stats = load_reaction_stats(charlist_stream_id)
          recent_messages = load_recent_messages(charlist_stream_id)
          viewer_chart_data = generate_viewer_chart_data(stream)
          stream_health = get_stream_health(stream_id)

          socket
          |> assign(:page_title, "Dashboard - #{stream.title}")
          |> assign(:stream, stream)
          |> assign(:is_owner, is_owner)
          |> assign(:viewer_stats, viewer_stats)
          |> assign(:reaction_stats, reaction_stats)
          |> assign(:recent_messages, recent_messages)
          |> assign(:viewer_chart_data, viewer_chart_data)
          |> assign(:stream_health, stream_health)
        else
          socket
          |> put_flash(:error, "Unauthorized")
          |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams/#{stream_id}")
        end

      {:error, _reason} ->
        socket
        |> put_flash(:error, "Stream not found")
        |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams")
    end
  end

  defp format_stream(stream_tuple) do
    {_tag, stream_id, user_id, title, description, _thumbnail_cid, _thumb_ipns, status,
     _visibility, _tags, category, _lang, _key, _rtmp, _backup, _playback, _hls, _vod, _rust_id,
     _proto, _qual, viewers, peak, total, _unique, _timeline, _mods, _banned, _slow, _slow_dur,
     _chat_en, _chat_replay, _chat_msgs, _reactions, _react_counts, _shares, _saves, started_at,
     ended_at, _scheduled, duration_seconds, _bitrate, _resolution, _dropped, _health, _notif,
     _created, _data} = stream_tuple

    %{
      id: if(is_binary(stream_id), do: stream_id, else: to_string(stream_id)),
      title: if(is_binary(title), do: title, else: to_string(title || "Untitled Stream")),
      description:
        if(is_binary(description), do: description, else: to_string(description || "")),
      user_id: if(is_binary(user_id), do: user_id, else: to_string(user_id)),
      status: status,
      category: format_category(category),
      started_at: started_at,
      ended_at: ended_at,
      current_viewers: viewers || 0,
      peak_viewers: peak || 0,
      total_views: total || 0,
      duration: safe_duration(duration_seconds)
    }
  end

  defp safe_duration(duration) when is_integer(duration), do: duration
  defp safe_duration(_), do: 0

  defp format_category(category) when is_atom(category), do: Atom.to_string(category)
  defp format_category(category) when is_binary(category), do: category
  defp format_category(category) when is_list(category), do: List.to_string(category)
  defp format_category(_), do: "other"

  defp load_viewer_stats(stream_id) do
    case :livestreamdb.get_livestream(stream_id) do
      {:ok, stream_tuple} ->
        {_tag, _stream_id, _user_id, _title, _description, _thumbnail_cid, _thumb_ipns, _status,
         _visibility, _tags, _category, _lang, _key, _rtmp, _backup, _playback, _hls, _vod,
         _rust_id, _proto, _qual, viewers, peak, total, _unique, _timeline, _mods, _banned, _slow,
         _slow_dur, _chat_en, _chat_replay, _chat_msgs, _reactions, _react_counts, _shares,
         _saves, _started_at, _ended_at, _scheduled, _duration, _bitrate, _resolution, _dropped,
         _health, _notif, _created, _data} = stream_tuple

        %{
          current: viewers || 0,
          peak: peak || 0,
          total: total || 0
        }

      _ ->
        %{current: 0, peak: 0, total: 0}
    end
  end

  defp load_reaction_stats(_stream_id) do
    %{}
  end

  defp load_recent_messages(_stream_id) do
    []
  end

  defp generate_viewer_chart_data(stream) do
    duration_minutes = div(stream.duration, 60)

    if duration_minutes <= 0 do
      []
    else
      peak = stream.peak_viewers
      current = stream.current_viewers

      interval = max(1, div(duration_minutes, 20))

      0..duration_minutes//interval
      |> Enum.map(fn minute ->
        ratio = minute / max(duration_minutes, 1)
        viewers = round(current + (peak - current) * :math.sin(ratio * :math.pi()))

        %{time: minute, viewers: max(0, viewers)}
      end)
    end
  end

  defp get_stream_health(_stream_id) do
    %{
      bitrate: "3500 kbps",
      fps: "30 fps",
      resolution: "1920x1080",
      status: "healthy"
    }
  end

  defp format_viewers(count) when is_integer(count) and count >= 1_000_000 do
    "#{Float.round(count / 1_000_000, 1)}M"
  end

  defp format_viewers(count) when is_integer(count) and count >= 1_000 do
    "#{Float.round(count / 1_000, 1)}K"
  end

  defp format_viewers(count) when is_integer(count), do: Integer.to_string(count)
  defp format_viewers(_), do: "0"

  defp format_duration(seconds) when is_integer(seconds) and seconds < 60, do: "#{seconds}s"

  defp format_duration(seconds) when is_integer(seconds) do
    hours = div(seconds, 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    if hours > 0 do
      "#{hours}h #{minutes}m #{secs}s"
    else
      "#{minutes}m #{secs}s"
    end
  end

  defp format_duration(_), do: "0s"

  defp format_time(datetime) when is_struct(datetime, NaiveDateTime) do
    Calendar.strftime(datetime, "%I:%M %p")
  end

  defp format_time(_), do: "N/A"

  defp get_status_color(:live), do: "bg-green-500"
  defp get_status_color(:ended), do: "bg-gray-500"
  defp get_status_color(_), do: "bg-gray-500"

  defp get_health_color("healthy"), do: "text-green-500"
  defp get_health_color("warning"), do: "text-yellow-500"
  defp get_health_color("critical"), do: "text-red-500"
  defp get_health_color(_), do: "text-gray-500"
end
