defmodule MazarynWeb.MediaLive.Livestream.GoLive do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Go Live")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:step, :camera)
     |> assign(:camera_active, false)
     |> assign(:recording, false)
     |> assign(:title, "")
     |> assign(:description, "")
     |> assign(:category, "gaming")
     |> assign(:visibility, "public")
     |> assign(:tags, [])
     |> assign(:tag_input, "")
     |> assign(:scheduled_time, nil)
     |> assign(:chat_enabled, true)
     |> assign(:chat_replay_enabled, true)
     |> assign(:stream_id, nil)
     |> assign(:rtmp_url, nil)
     |> assign(:stream_key, nil)
     |> assign(:stream_started, false)
     |> assign(:auto_record, true)
     |> assign(:recording_blob, nil)
     |> assign(:errors, [])}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("camera_ready", _params, socket) do
    {:noreply, assign(socket, :camera_active, true)}
  end

  @impl true
  def handle_event("start_recording", _params, socket) do
    {:noreply, assign(socket, :recording, true)}
  end

  @impl true
  def handle_event("stop_recording", _params, socket) do
    {:noreply, assign(socket, :recording, false)}
  end

  @impl true
  def handle_event("toggle_auto_record", _params, socket) do
    new_value = !socket.assigns.auto_record
    {:noreply, assign(socket, :auto_record, new_value)}
  end

  @impl true
  def handle_event("recording_saved", %{"blob_size" => blob_size, "duration" => duration}, socket) do
    Logger.info("Recording saved: #{blob_size} bytes, #{duration} seconds")

    {:noreply,
     socket
     |> assign(:recording_blob, %{size: blob_size, duration: duration})
     |> put_flash(:info, "Recording saved (#{blob_size} bytes)")}
  end

  @impl true
  def handle_event("setup_stream", params, socket) do
    title = Map.get(params, "title", "")
    description = Map.get(params, "description", "")
    category = Map.get(params, "category", "gaming")

    {:noreply,
     socket
     |> assign(:title, title)
     |> assign(:description, description)
     |> assign(:category, category)
     |> assign(:step, :confirm)}
  end

  @impl true
  def handle_event("back_to_camera", _params, socket) do
    {:noreply, assign(socket, :step, :camera)}
  end

  @impl true
  def handle_event("create_and_go_live", _params, socket) do
    user_id = get_user_id(socket.assigns.user)

    if is_nil(user_id) do
      {:noreply, put_flash(socket, :error, "User session expired")}
    else
      charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

      case :livestreamdb.create_livestream(
             charlist_user_id,
             socket.assigns.title,
             socket.assigns.description,
             String.to_atom(socket.assigns.visibility),
             socket.assigns.tags,
             String.to_atom(socket.assigns.category),
             "en",
             socket.assigns.scheduled_time
           ) do
        {:ok, stream_id, stream_key, rtmp_url, _backup_url} ->
          stream_id_str = if is_binary(stream_id), do: stream_id, else: to_string(stream_id)
          stream_key_str = if is_binary(stream_key), do: stream_key, else: to_string(stream_key)
          rtmp_url_str = if is_binary(rtmp_url), do: rtmp_url, else: to_string(rtmp_url)

          if socket.assigns.auto_record do
            :livestreamdb.enable_auto_record(stream_id, charlist_user_id)
          end

          rust_stream_id = "rust_#{stream_id_str}"
          playback_url = "https://stream.mazaryn.io/#{stream_id_str}"
          hls_url = "https://stream.mazaryn.io/hls/#{stream_id_str}/playlist.m3u8"

          case :livestreamdb.start_livestream(
                 stream_id,
                 charlist_user_id,
                 rust_stream_id,
                 playback_url,
                 hls_url
               ) do
            :ok ->
              Phoenix.PubSub.broadcast(
                Mazaryn.PubSub,
                "livestreams:global",
                {:stream_started, %{stream_id: stream_id_str}}
              )

              {:noreply,
               socket
               |> assign(:stream_started, true)
               |> assign(:stream_id, stream_id_str)
               |> assign(:stream_key, stream_key_str)
               |> assign(:rtmp_url, rtmp_url_str)
               |> assign(:step, :streaming)
               |> put_flash(:info, "You are now LIVE!")}

            {:error, reason} ->
              Logger.error("Failed to start stream: #{inspect(reason)}")
              {:noreply, put_flash(socket, :error, "Failed to start stream")}
          end

        {:error, reason} ->
          Logger.error("Failed to create stream: #{inspect(reason)}")
          {:noreply, put_flash(socket, :error, "Failed to create stream")}
      end
    end
  end

  @impl true
  def handle_event("end_stream", _params, socket) do
    Logger.info("🛑 End stream requested")

    if socket.assigns.stream_id do
      user_id = get_user_id(socket.assigns.user)
      stream_id = socket.assigns.stream_id
      charlist_stream_id = String.to_charlist(stream_id)
      charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

      case :livestreamdb.end_livestream(charlist_stream_id, charlist_user_id) do
        {:ok, duration} ->
          Phoenix.PubSub.broadcast(
            Mazaryn.PubSub,
            "livestreams:global",
            {:stream_ended, stream_id}
          )

          push_event(socket, "stop_camera", %{})

          {:noreply,
           socket
           |> assign(:stream_started, false)
           |> assign(:stream_id, nil)
           |> put_flash(:info, "Stream ended successfully (#{format_duration(duration)})")
           |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams/my-streams")}

        {:error, :not_found} ->
          {:noreply,
           socket
           |> put_flash(:warning, "Stream already ended or not found")
           |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams/my-streams")}

        {:error, :unauthorized} ->
          {:noreply, put_flash(socket, :error, "You don't have permission to end this stream")}

        {:error, reason} ->
          Logger.error("Failed to end stream: #{inspect(reason)}")
          {:noreply, put_flash(socket, :error, "Failed to end stream")}
      end
    else
      {:noreply, put_flash(socket, :error, "No active stream to end")}
    end
  end

  @impl true
  def handle_info({:stream_ended, ended_stream_id}, socket) do
    if ended_stream_id == socket.assigns.stream_id do
      {:noreply,
       socket
       |> assign(:stream_started, false)
       |> assign(:stream_id, nil)
       |> put_flash(:info, "Stream has been ended")
       |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams/my-streams")}
    else
      {:noreply, socket}
    end
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

  defp get_category_display_name(category) do
    case category do
      "gaming" -> "Gaming"
      "music" -> "Music"
      "education" -> "Education"
      "tech" -> "Tech & Science"
      "art" -> "Art & Creative"
      "sports" -> "Sports"
      "cooking" -> "Cooking"
      "fitness" -> "Fitness"
      "entertainment" -> "Entertainment"
      _ -> "Other"
    end
  end

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

  defp format_file_size(bytes) when is_integer(bytes) do
    cond do
      bytes >= 1_073_741_824 -> "#{Float.round(bytes / 1_073_741_824, 2)} GB"
      bytes >= 1_048_576 -> "#{Float.round(bytes / 1_048_576, 2)} MB"
      bytes >= 1024 -> "#{Float.round(bytes / 1024, 2)} KB"
      true -> "#{bytes} B"
    end
  end

  defp format_file_size(_), do: "0 B"
end
