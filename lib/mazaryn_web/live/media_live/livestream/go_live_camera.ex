defmodule MazarynWeb.MediaLive.Livestream.GoLiveCamera do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    if !user do
      {:ok, push_navigate(socket, to: ~p"/#{socket.assigns[:locale] || "en"}/login")}
    else
      {:ok,
       socket
       |> assign(:page_title, "Go Live - Camera Setup")
       |> assign(:user, user)
       |> assign(:current_user, user)
       |> assign(:locale, socket.assigns[:locale] || "en")
       |> assign(:step, :camera_test)
       |> assign(:camera_active, false)
       |> assign(:recording, false)
       |> assign(:stream_started, false)
       |> assign(:title, "")
       |> assign(:description, "")
       |> assign(:category, "gaming")
       |> assign(:stream_id, nil)
       |> assign(:stream_key, nil)
       |> assign(:rtmp_url, nil)}
    end
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
  def handle_event("create_and_go_live", _params, socket) do
    user_id = get_user_id(socket.assigns.user)
    charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

    case :livestreamdb.create_livestream(
           charlist_user_id,
           socket.assigns.title,
           socket.assigns.description,
           :public,
           [],
           socket.assigns.category,
           "en",
           :undefined
         ) do
      {:ok, stream_id, stream_key, rtmp_url, _backup_url} ->
        stream_id_str = if is_binary(stream_id), do: stream_id, else: to_string(stream_id)
        stream_key_str = if is_binary(stream_key), do: stream_key, else: to_string(stream_key)
        rtmp_url_str = if is_binary(rtmp_url), do: rtmp_url, else: to_string(rtmp_url)

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
            {:noreply,
             socket
             |> assign(:stream_started, true)
             |> assign(:stream_id, stream_id_str)
             |> assign(:stream_key, stream_key_str)
             |> assign(:rtmp_url, rtmp_url_str)
             |> assign(:step, :streaming)
             |> put_flash(:info, "Stream started! You are now LIVE!")}

          {:error, reason} ->
            Logger.error("Failed to start stream: #{inspect(reason)}")
            {:noreply, put_flash(socket, :error, "Failed to start stream")}
        end

      {:error, reason} ->
        Logger.error("Failed to create stream: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to create stream")}
    end
  end

  @impl true
  def handle_event("end_stream", _params, socket) do
    if socket.assigns.stream_id do
      user_id = get_user_id(socket.assigns.user)
      charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id
      stream_id = socket.assigns.stream_id

      case :livestreamdb.end_livestream(stream_id, charlist_user_id) do
        {:ok, _duration} ->
          {:noreply,
           socket
           |> assign(:stream_started, false)
           |> put_flash(:info, "Stream ended successfully")
           |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams")}

        {:error, reason} ->
          Logger.error("Failed to end stream: #{inspect(reason)}")
          {:noreply, put_flash(socket, :error, "Failed to end stream")}
      end
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

  defp get_user_id(user) when is_tuple(user) do
    id = elem(user, 0)
    if is_list(id), do: List.to_string(id), else: to_string(id)
  end

  defp get_user_id(user) when is_map(user) do
    Map.get(user, :id) || Map.get(user, "id")
  end

  defp get_user_id(_), do: nil
end
