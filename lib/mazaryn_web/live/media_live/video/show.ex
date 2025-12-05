defmodule MazarynWeb.MediaLive.Video.Show do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(params, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, session["locale"] || "en")
     |> assign(:video, nil)
     |> assign(:creator, nil)
     |> assign(:related_videos, [])
     |> assign(:comments, [])
     |> assign(:comment_text, "")
     |> assign(:is_subscribed, false)
     |> assign(:user_reaction, nil)
     |> assign(:watch_time_total, 0)
     |> assign(:show_delete_modal, false)}
  rescue
    error ->
      Logger.error("===== VIDEO SHOW MOUNT: Exception: #{inspect(error)} =====")

      {:ok,
       socket
       |> put_flash(:error, "Failed to load video")
       |> push_navigate(to: ~p"/en/videos")}
  end

  @impl true
  def handle_params(%{"id" => video_id} = params, url, socket) do
    Logger.info("===== VIDEO SHOW: Loading video #{video_id} =====")

    user = socket.assigns.user

    normalized_id =
      case Integer.parse(video_id) do
        {int_id, ""} ->
          int_id

        _ ->
          if String.match?(video_id, ~r/^\d+$/),
            do: String.to_integer(video_id),
            else: String.to_charlist(video_id)
      end

    case load_video(normalized_id, user) do
      {:ok, video, creator, related_videos, comments} ->
        Logger.info("===== VIDEO SHOW: Video loaded successfully =====")

        {:noreply,
         socket
         |> assign(:page_title, video.title)
         |> assign(:video, video)
         |> assign(:creator, creator)
         |> assign(:related_videos, related_videos)
         |> assign(:comments, comments)}

      {:error, reason} ->
        Logger.error("===== VIDEO SHOW: Error: #{inspect(reason)} =====")

        {:noreply,
         socket
         |> put_flash(:error, "Video not found")
         |> push_navigate(to: ~p"/#{socket.assigns.locale}/videos")}
    end
  end

  @impl true
  def handle_event("track_watch_time", %{"video_id" => video_id, "seconds" => seconds}, socket) do
    user_id = get_user_id(socket.assigns.user)

    if user_id do
      charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

      spawn(fn ->
        try do
          :videodb.track_watch_time(charlist_video_id, user_id, seconds)
        catch
          error_type, error ->
            Logger.error("===== TRACK_WATCH_TIME: Error #{error_type}: #{inspect(error)} =====")
        end
      end)

      new_total = socket.assigns.watch_time_total + seconds
      {:noreply, assign(socket, :watch_time_total, new_total)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("video_play", %{"video_id" => video_id}, socket) do
    Logger.info("===== VIDEO_PLAY: Video #{video_id} started =====")
    {:noreply, socket}
  end

  @impl true
  def handle_event(
        "video_pause",
        %{"video_id" => _video_id, "current_time" => _current_time},
        socket
      ) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("video_ended", %{"video_id" => video_id}, socket) do
    Logger.info("===== VIDEO_ENDED: Video #{video_id} finished =====")
    {:noreply, socket}
  end

  @impl true
  def handle_event(
        "video_progress",
        %{"video_id" => _video_id, "percentage" => _percentage, "current_time" => _current_time},
        socket
      ) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("add_comment", %{"comment" => comment_text}, socket) do
    user_id = get_user_id(socket.assigns.user)
    video_id = socket.assigns.video.id

    if String.trim(comment_text) != "" do
      charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

      case :videodb.add_video_comment(user_id, charlist_video_id, comment_text) do
        comment_id when is_binary(comment_id) or is_list(comment_id) ->
          comments = load_comments(charlist_video_id)

          {:noreply,
           socket
           |> assign(:comments, comments)
           |> assign(:comment_text, "")
           |> put_flash(:info, "Comment added successfully")}

        {:error, reason} ->
          Logger.error("===== ADD_COMMENT: Error: #{inspect(reason)} =====")
          {:noreply, put_flash(socket, :error, "Failed to add comment")}

        other ->
          Logger.error("===== ADD_COMMENT: Unexpected: #{inspect(other)} =====")
          {:noreply, put_flash(socket, :error, "Failed to add comment")}
      end
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("update_comment", %{"value" => text}, socket) do
    {:noreply, assign(socket, :comment_text, text)}
  end

  @impl true
  def handle_event("react", %{"type" => reaction_type}, socket) do
    user_id = get_user_id(socket.assigns.user)
    video_id = socket.assigns.video.id
    reaction_atom = String.to_atom(reaction_type)

    charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    case :videodb.react_to_video(user_id, charlist_video_id, reaction_atom) do
      reaction_id when is_binary(reaction_id) or is_list(reaction_id) ->
        video = reload_video(charlist_video_id)

        {:noreply,
         socket
         |> assign(:video, video)
         |> assign(:user_reaction, reaction_type)}

      {:removed, _type} ->
        video = reload_video(charlist_video_id)

        {:noreply,
         socket
         |> assign(:video, video)
         |> assign(:user_reaction, nil)}

      {:error, reason} ->
        Logger.error("===== REACT: Error: #{inspect(reason)} =====")
        {:noreply, socket}

      other ->
        Logger.error("===== REACT: Unexpected: #{inspect(other)} =====")
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("share", _params, socket) do
    video_id = socket.assigns.video.id
    user_id = get_user_id(socket.assigns.user)

    charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    if user_id do
      :videodb.share_video(charlist_video_id, user_id)
    end

    {:noreply, put_flash(socket, :info, "Video shared")}
  end

  @impl true
  def handle_event("save", _params, socket) do
    video_id = socket.assigns.video.id
    user_id = get_user_id(socket.assigns.user)

    charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    if user_id do
      :videodb.save_video(charlist_video_id, user_id)
    end

    {:noreply, put_flash(socket, :info, "Video saved")}
  end

  @impl true
  def handle_event("subscribe", _params, socket) do
    {:noreply, assign(socket, :is_subscribed, true)}
  end

  @impl true
  def handle_event("play_related", %{"id" => video_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id}")}
  end

  @impl true
  def handle_event("show_delete_modal", _params, socket) do
    {:noreply, assign(socket, :show_delete_modal, true)}
  end

  @impl true
  def handle_event("hide_delete_modal", _params, socket) do
    {:noreply, assign(socket, :show_delete_modal, false)}
  end

  @impl true
  def handle_event("delete_video", _params, socket) do
    Logger.info("===== DELETE_VIDEO: Starting =====")

    user_id = get_user_id(socket.assigns.user)
    video_id = socket.assigns.video.id

    Logger.info("===== DELETE_VIDEO: User ID: #{inspect(user_id)} =====")
    Logger.info("===== DELETE_VIDEO: Video ID: #{inspect(video_id)} =====")

    if is_nil(user_id) do
      Logger.error("===== DELETE_VIDEO: No user ID =====")
      {:noreply, put_flash(socket, :error, "User session expired")}
    else
      charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

      Logger.info("===== DELETE_VIDEO: Calling videodb:delete_video =====")

      case :videodb.delete_video(charlist_video_id, user_id) do
        :ok ->
          Logger.info("===== DELETE_VIDEO: Success =====")

          video_id_str = if is_list(video_id), do: List.to_string(video_id), else: video_id

          try do
            case :ets.lookup(:video_path_cache, video_id_str) do
              [{^video_id_str, local_path, _timestamp}] ->
                Logger.info("===== DELETE_VIDEO: Found in cache, deleting file =====")

                spawn(fn ->
                  if File.exists?(local_path) do
                    File.rm(local_path)
                    Logger.info("===== DELETE_VIDEO: Local file deleted =====")
                  end
                end)

                :ets.delete(:video_path_cache, video_id_str)

              [] ->
                Logger.info("===== DELETE_VIDEO: Not in cache =====")
            end
          rescue
            _ -> :ok
          end

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> put_flash(:info, "Video deleted successfully")
           |> push_navigate(to: ~p"/#{socket.assigns.locale}/videos/my-videos")}

        {:error, :unauthorized} ->
          Logger.error("===== DELETE_VIDEO: Unauthorized =====")

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> put_flash(:error, "You are not authorized to delete this video")}

        {:error, :video_not_found} ->
          Logger.error("===== DELETE_VIDEO: Video not found =====")

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> put_flash(:error, "Video not found")}

        {:error, reason} ->
          Logger.error("===== DELETE_VIDEO: Error: #{inspect(reason)} =====")

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> put_flash(:error, "Failed to delete video")}
      end
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
  defp get_user_id(nil), do: nil
  defp get_user_id(_), do: nil

  defp load_video(video_id, user) do
    Logger.info("===== LOAD_VIDEO: Loading video #{inspect(video_id)} =====")

    user_id = get_user_id(user)

    video_result = try_load_video_formats(video_id)

    case video_result do
      {:ok, video_tuple} ->
        Logger.info("===== LOAD_VIDEO: Video tuple received =====")

        if user_id do
          charlist_video_id =
            if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

          :videodb.increment_view_count(charlist_video_id, user_id)
          :videodb.increment_unique_view(charlist_video_id, user_id)
        end

        video = format_video(video_tuple)
        creator = get_creator_info(video.user_id)
        related_videos = load_related_videos(video_id)
        comments = load_comments(video_id)

        {:ok, video, creator, related_videos, comments}

      error ->
        Logger.error("===== LOAD_VIDEO: Error: #{inspect(error)} =====")
        {:error, :not_found}
    end
  rescue
    error ->
      Logger.error("===== LOAD_VIDEO: Exception: #{inspect(error)} =====")
      {:error, :exception}
  end

  defp try_load_video_formats(video_id) do
    charlist_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    case :videodb.get_video_by_id(charlist_id) do
      {:error, :video_not_found} -> {:error, :not_found}
      video_tuple when is_tuple(video_tuple) -> {:ok, video_tuple}
      error -> {:error, :invalid_response}
    end
  end

  defp reload_video(video_id) do
    charlist_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    case :videodb.get_video_by_id(charlist_id) do
      video_tuple when is_tuple(video_tuple) -> format_video(video_tuple)
      _ -> nil
    end
  end

  defp format_video(video_tuple) do
    video = Mazaryn.Schema.Video.erl_changeset(video_tuple)

    %{
      id: video.changes.id,
      title: video.changes.title || "Untitled",
      description: video.changes.description || "",
      ipfs_cid: get_ipfs_cid(video.changes),
      thumbnail_url: get_thumbnail_url(video.changes),
      duration: format_duration(video.changes.duration_seconds),
      views: Map.get(video.changes, :views, 0),
      user_id: video.changes.user_id,
      created_at: video.changes.date_created,
      status: Map.get(video.changes, :status, :ready),
      reaction_counts: Map.get(video.changes, :reaction_counts, %{})
    }
  end

  defp get_ipfs_cid(video) do
    cond do
      Map.has_key?(video, :ipfs_cid) && is_tuple(video.ipfs_cid) ->
        video.ipfs_cid

      Map.has_key?(video, :ipfs_cid) && is_binary(video.ipfs_cid) && video.ipfs_cid != "" ->
        video.ipfs_cid

      Map.has_key?(video, :ipfs_cid) && is_list(video.ipfs_cid) && length(video.ipfs_cid) > 0 ->
        video.ipfs_cid

      true ->
        nil
    end
  end

  defp get_thumbnail_url(video) do
    cond do
      Map.has_key?(video, :thumbnail_url) && video.thumbnail_url && video.thumbnail_url != "" ->
        video.thumbnail_url

      Map.has_key?(video, :thumbnail_cid) && video.thumbnail_cid && video.thumbnail_cid != "" ->
        cid =
          if is_list(video.thumbnail_cid),
            do: List.to_string(video.thumbnail_cid),
            else: video.thumbnail_cid

        "https://ipfs.io/ipfs/#{cid}"

      Map.has_key?(video, :thumbnail_cids) && is_list(video.thumbnail_cids) &&
          length(video.thumbnail_cids) > 0 ->
        first_cid = List.first(video.thumbnail_cids)
        cid_string = if is_list(first_cid), do: List.to_string(first_cid), else: first_cid
        "https://ipfs.io/ipfs/#{cid_string}"

      true ->
        "/images/default-thumbnail.jpg"
    end
  end

  defp format_duration(nil), do: "00:00"
  defp format_duration(0), do: "00:00"
  defp format_duration(0.0), do: "00:00"

  defp format_duration(seconds) when is_float(seconds), do: format_duration(round(seconds))

  defp format_duration(seconds) when is_integer(seconds) do
    minutes = div(seconds, 60)
    secs = rem(seconds, 60)

    "#{String.pad_leading(Integer.to_string(minutes), 2, "0")}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
  end

  defp format_duration(_), do: "00:00"

  defp format_time_ago(nil), do: "Just now"

  defp format_time_ago(datetime) do
    now = NaiveDateTime.utc_now()
    diff_seconds = NaiveDateTime.diff(now, datetime)

    cond do
      diff_seconds < 60 -> "Just now"
      diff_seconds < 3600 -> "#{div(diff_seconds, 60)} minutes ago"
      diff_seconds < 86400 -> "#{div(diff_seconds, 3600)} hours ago"
      diff_seconds < 604_800 -> "#{div(diff_seconds, 86400)} days ago"
      true -> "#{div(diff_seconds, 604_800)} weeks ago"
    end
  end

  defp format_views(views) when is_integer(views) and views >= 1_000_000,
    do: "#{Float.round(views / 1_000_000, 1)}M"

  defp format_views(views) when is_integer(views) and views >= 1_000,
    do: "#{Float.round(views / 1_000, 1)}K"

  defp format_views(views) when is_integer(views), do: Integer.to_string(views)
  defp format_views(_), do: "0"

  defp get_creator_info(user_id) do
    Logger.info("===== GET_CREATOR_INFO: Getting info for user: #{inspect(user_id)} =====")

    charlist_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

    case Core.UserClient.get_user_by_id(charlist_id) do
      user_tuple when is_tuple(user_tuple) ->
        Logger.info("===== GET_CREATOR_INFO: User tuple received =====")
        Logger.info("===== GET_CREATOR_INFO: User tuple size: #{tuple_size(user_tuple)} =====")

        username = elem(user_tuple, 2)

        username_str =
          case username do
            u when is_list(u) and length(u) > 0 ->
              str = List.to_string(u)

              if String.starts_with?(str, ["/ip4", "/ip6"]) do
                "Unknown"
              else
                str
              end

            u when is_binary(u) and u != "" ->
              if String.starts_with?(u, ["/ip4", "/ip6"]) do
                "Unknown"
              else
                u
              end

            _ ->
              "Unknown"
          end

        avatar = elem(user_tuple, 6)

        avatar_str =
          case avatar do
            a when is_list(a) and length(a) > 0 ->
              str = List.to_string(a)

              if String.starts_with?(str, ["/ip4", "/ip6", "http"]) and
                   not String.contains?(str, "ipfs.io") do
                "/images/default-avatar.png"
              else
                str
              end

            a when is_binary(a) and a != "" ->
              if String.starts_with?(a, ["/ip4", "/ip6"]) do
                "/images/default-avatar.png"
              else
                a
              end

            _ ->
              "/images/default-avatar.png"
          end

        Logger.info("===== GET_CREATOR_INFO: Username: #{username_str} =====")
        Logger.info("===== GET_CREATOR_INFO: Avatar: #{avatar_str} =====")

        %{
          id: user_id,
          username: username_str,
          avatar: avatar_str,
          subscribers: 0
        }

      error ->
        Logger.error("===== GET_CREATOR_INFO: Error: #{inspect(error)} =====")

        %{
          id: user_id,
          username: "Unknown",
          avatar: "/images/default-avatar.png",
          subscribers: 0
        }
    end
  end

  defp load_related_videos(current_video_id) do
    case :videodb.get_public_videos() do
      videos when is_list(videos) ->
        charlist_current_id =
          if is_binary(current_video_id),
            do: String.to_charlist(current_video_id),
            else: current_video_id

        videos
        |> Enum.reject(fn video_tuple ->
          video = Mazaryn.Schema.Video.erl_changeset(video_tuple)
          video_id = video.changes.id
          normalized_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id
          normalized_id == charlist_current_id
        end)
        |> Enum.take(8)
        |> Enum.map(&format_related_video/1)

      _ ->
        []
    end
  rescue
    _ -> []
  end

  defp format_related_video(video_tuple) do
    video = Mazaryn.Schema.Video.erl_changeset(video_tuple)

    %{
      id: video.changes.id,
      title: video.changes.title || "Untitled",
      thumbnail_url: get_thumbnail_url(video.changes),
      duration: format_duration(video.changes.duration_seconds),
      views: format_views(Map.get(video.changes, :views, 0)),
      created_at: format_time_ago(video.changes.date_created),
      creator: get_creator_name(video.changes.user_id)
    }
  end

  defp get_creator_name(user_id) do
    charlist_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

    case Core.UserClient.get_user_by_id(charlist_id) do
      user_tuple when is_tuple(user_tuple) ->
        username = elem(user_tuple, 2)
        if is_list(username), do: List.to_string(username), else: username

      _ ->
        "Unknown"
    end
  end

  defp load_comments(_video_id), do: []
end
