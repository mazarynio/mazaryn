defmodule MazarynWeb.MediaLive.Livestream.Watch do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(%{"id" => stream_id}, session, socket) do
    user = get_user_from_session(session)

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestream:#{stream_id}")
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestream:#{stream_id}:chat")
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestream:#{stream_id}:viewers")
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestreams:global")
    end

    {:ok,
     socket
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:stream_id, stream_id)
     |> assign(:stream, nil)
     |> assign(:creator, nil)
     |> assign(:chat_messages, [])
     |> assign(:chat_input, "")
     |> assign(:current_viewers, 0)
     |> assign(:is_following, false)
     |> assign(:show_viewer_list, false)
     |> assign(:viewers_list, [])
     |> load_stream(stream_id, user)}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("send_chat", %{"message" => message}, socket) do
    user_id = get_user_id(socket.assigns.user)
    stream_id = socket.assigns.stream_id

    if user_id && String.trim(message) != "" do
      username = get_username(socket.assigns.user)
      charlist_stream_id = String.to_charlist(stream_id)
      charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

      case :livestreamdb.add_chat_message(charlist_stream_id, charlist_user_id, username, message) do
        {:ok, _msg_id} ->
          Phoenix.PubSub.broadcast(
            Mazaryn.PubSub,
            "livestream:#{stream_id}:chat",
            {:new_chat_message,
             %{
               user_id: user_id,
               username: username,
               message: message,
               timestamp: NaiveDateTime.utc_now()
             }}
          )

          {:noreply, assign(socket, :chat_input, "")}

        {:error, reason} ->
          Logger.error("Failed to send chat: #{inspect(reason)}")
          {:noreply, put_flash(socket, :error, "Failed to send message")}
      end
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("update_chat_input", %{"value" => value}, socket) do
    {:noreply, assign(socket, :chat_input, value)}
  end

  @impl true
  def handle_event("send_reaction", %{"reaction_type" => reaction_type}, socket) do
    user_id = get_user_id(socket.assigns.user)
    stream_id = socket.assigns.stream_id

    if user_id do
      charlist_stream_id = String.to_charlist(stream_id)
      charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id
      reaction_atom = String.to_atom(reaction_type)

      case :livestreamdb.react_to_livestream(charlist_user_id, charlist_stream_id, reaction_atom) do
        ID when is_binary(ID) or is_integer(ID) ->
          Phoenix.PubSub.broadcast(
            Mazaryn.PubSub,
            "livestream:#{stream_id}",
            {:new_reaction, reaction_type}
          )

          {:noreply, socket}

        {:error, reason} ->
          Logger.error("Failed to send reaction: #{inspect(reason)}")
          {:noreply, socket}
      end
    else
      {:noreply, put_flash(socket, :error, "Please login to react")}
    end
  end

  @impl true
  def handle_event("toggle_follow", _params, socket) do
    user_id = get_user_id(socket.assigns.user)
    creator_id = socket.assigns.creator.id

    if user_id && creator_id do
      new_follow_state = !socket.assigns.is_following

      if new_follow_state do
        :follow_system.follow(user_id, creator_id)
      else
        :follow_system.unfollow(user_id, creator_id)
      end

      {:noreply, assign(socket, :is_following, new_follow_state)}
    else
      {:noreply, put_flash(socket, :error, "Please login to follow")}
    end
  end

  @impl true
  def handle_event("toggle_viewer_list", _params, socket) do
    show_list = !socket.assigns.show_viewer_list

    viewers =
      if show_list do
        load_viewers(socket.assigns.stream_id)
      else
        []
      end

    {:noreply, socket |> assign(:show_viewer_list, show_list) |> assign(:viewers_list, viewers)}
  end

  @impl true
  def handle_event("stream_play", _params, socket) do
    user_id = get_user_id(socket.assigns.user)
    stream_id = socket.assigns.stream_id

    if user_id do
      charlist_stream_id = String.to_charlist(stream_id)
      charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id
      :livestreamdb.increment_viewer(charlist_stream_id, charlist_user_id)

      Phoenix.PubSub.broadcast(
        Mazaryn.PubSub,
        "livestream:#{stream_id}:viewers",
        {:viewer_joined, user_id}
      )
    end

    {:noreply, socket}
  end

  @impl true
  def handle_event("stream_pause", _params, socket) do
    user_id = get_user_id(socket.assigns.user)
    stream_id = socket.assigns.stream_id

    if user_id do
      charlist_stream_id = String.to_charlist(stream_id)
      charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id
      :livestreamdb.decrement_viewer(charlist_stream_id, charlist_user_id)

      Phoenix.PubSub.broadcast(
        Mazaryn.PubSub,
        "livestream:#{stream_id}:viewers",
        {:viewer_left, user_id}
      )
    end

    {:noreply, socket}
  end

  @impl true
  def handle_event("check_stream_status", _params, socket) do
    stream_id = socket.assigns.stream_id
    charlist_stream_id = String.to_charlist(stream_id)

    case :livestreamdb.get_livestream(charlist_stream_id) do
      {:ok, stream_tuple} ->
        stream = Mazaryn.Schema.Livestream.erl_changeset(stream_tuple)

        if stream.changes.status != :live do
          {:noreply,
           socket
           |> put_flash(:info, "Stream has ended")
           |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams")}
        else
          {:noreply, socket}
        end

      _ ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_info({:new_chat_message, message_data}, socket) do
    new_message = %{
      username: message_data.username,
      message: message_data.message,
      timestamp: message_data.timestamp,
      avatar: get_user_avatar(message_data.user_id)
    }

    messages = [new_message | socket.assigns.chat_messages] |> Enum.take(100)
    {:noreply, assign(socket, :chat_messages, messages)}
  end

  @impl true
  def handle_info({:viewer_joined, _user_id}, socket) do
    current = socket.assigns.current_viewers + 1
    {:noreply, assign(socket, :current_viewers, current)}
  end

  @impl true
  def handle_info({:viewer_left, _user_id}, socket) do
    current = max(0, socket.assigns.current_viewers - 1)
    {:noreply, assign(socket, :current_viewers, current)}
  end

  @impl true
  def handle_info({:stream_ended, ended_stream_id}, socket) do
    if ended_stream_id == socket.assigns.stream_id do
      {:noreply,
       socket
       |> put_flash(:info, "This livestream has ended")
       |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams")}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_info({:new_reaction, _reaction_type}, socket) do
    {:noreply, socket}
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

  defp get_username(%{username: username}) when is_binary(username), do: username

  defp get_username(user_tuple) when is_tuple(user_tuple) do
    username = elem(user_tuple, 8)

    case username do
      u when is_list(u) -> List.to_string(u)
      u when is_binary(u) -> u
      _ -> "Anonymous"
    end
  end

  defp get_username(_), do: "Anonymous"

  defp load_stream(socket, stream_id, user) do
    charlist_stream_id = String.to_charlist(stream_id)

    case :livestreamdb.get_livestream(charlist_stream_id) do
      {:ok, stream_tuple} ->
        stream = format_stream(stream_tuple)
        creator = get_creator_info(stream.user_id)
        messages = load_chat_messages(charlist_stream_id)
        user_id = get_user_id(user)

        is_following =
          if user_id && creator.id do
            :follow_system.is_following(user_id, creator.id)
          else
            false
          end

        socket
        |> assign(:page_title, stream.title)
        |> assign(:stream, stream)
        |> assign(:creator, creator)
        |> assign(:chat_messages, messages)
        |> assign(:current_viewers, stream.current_viewers)
        |> assign(:is_following, is_following)

      {:error, reason} ->
        Logger.error("Failed to load stream: #{inspect(reason)}")

        socket
        |> put_flash(:error, "Stream not found")
        |> push_navigate(to: ~p"/#{socket.assigns.locale}/livestreams")
    end
  end

  defp format_stream(stream_tuple) do
    {_tag, stream_id, user_id, title, description, thumbnail_cid, _thumb_ipns, status,
     _visibility, tags, category, _lang, _key, _rtmp, _backup, _playback, hls_url, _vod, _rust_id,
     _proto, _qual, viewers, peak, total, _unique, _timeline, _mods, _banned, _slow, _slow_dur,
     _chat_en, _chat_replay, _chat_msgs, _reactions, _react_counts, _shares, _saves, started_at,
     _ended_at, _scheduled, _duration, _bitrate, _resolution, _dropped, _health, _notif, _created,
     _data} = stream_tuple

    %{
      id: if(is_binary(stream_id), do: stream_id, else: to_string(stream_id)),
      title: if(is_binary(title), do: title, else: to_string(title || "Untitled Stream")),
      description:
        if(is_binary(description), do: description, else: to_string(description || "")),
      thumbnail_url: get_thumbnail_url(thumbnail_cid),
      user_id: if(is_binary(user_id), do: user_id, else: to_string(user_id)),
      category: format_category(category),
      current_viewers: viewers || 0,
      peak_viewers: peak || 0,
      total_views: total || 0,
      started_at: started_at,
      status: status,
      hls_url: if(is_binary(hls_url), do: hls_url, else: get_hls_url(stream_id)),
      tags: if(is_list(tags), do: tags, else: [])
    }
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

  defp get_hls_url(stream_id) when is_binary(stream_id) do
    "https://#{System.get_env("HLS_DOMAIN", "stream.mazaryn.io")}/live/#{stream_id}/index.m3u8"
  end

  defp get_hls_url(stream_id) when is_list(stream_id) do
    get_hls_url(List.to_string(stream_id))
  end

  defp get_creator_info(user_id) do
    charlist_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

    case Core.UserClient.get_user_by_id(charlist_id) do
      user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 35 ->
        username = elem(user_tuple, 8)
        avatar = elem(user_tuple, 26)

        %{
          id: user_id,
          username: process_username(username),
          avatar: process_avatar(avatar)
        }

      _ ->
        %{id: user_id, username: "Unknown", avatar: "/images/default-avatar.png"}
    end
  end

  defp process_username(username) when is_list(username) and length(username) > 0 do
    str = List.to_string(username)
    if String.starts_with?(str, ["/ip4", "/ip6"]), do: "Unknown", else: str
  end

  defp process_username(username) when is_binary(username) and username != "" do
    if String.starts_with?(username, ["/ip4", "/ip6"]), do: "Unknown", else: username
  end

  defp process_username(_), do: "Unknown"

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

  defp load_chat_messages(stream_id) do
    case :livestreamdb.get_chat_messages(stream_id) do
      messages when is_list(messages) ->
        messages
        |> Enum.take(50)
        |> Enum.map(&format_chat_message/1)
        |> Enum.reverse()

      _ ->
        []
    end
  end

  defp format_chat_message({msg_id, user_id, username, message, timestamp}) do
    %{
      id: msg_id,
      username: if(is_list(username), do: List.to_string(username), else: username),
      message: if(is_list(message), do: List.to_string(message), else: message),
      timestamp: timestamp,
      avatar: get_user_avatar(user_id)
    }
  end

  defp get_user_avatar(user_id) do
    charlist_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

    case Core.UserClient.get_user_by_id(charlist_id) do
      user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 35 ->
        avatar = elem(user_tuple, 26)
        process_avatar(avatar)

      _ ->
        "/images/default-avatar.png"
    end
  end

  defp load_viewers(stream_id) do
    charlist_stream_id = String.to_charlist(stream_id)

    case :livestreamdb.get_current_viewers(charlist_stream_id) do
      viewers when is_list(viewers) ->
        Enum.map(viewers, fn user_id ->
          charlist_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

          case Core.UserClient.get_user_by_id(charlist_id) do
            user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 35 ->
              username = elem(user_tuple, 8)
              avatar = elem(user_tuple, 26)

              %{
                id: user_id,
                username: process_username(username),
                avatar: process_avatar(avatar)
              }

            _ ->
              %{id: user_id, username: "Unknown", avatar: "/images/default-avatar.png"}
          end
        end)

      _ ->
        []
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
