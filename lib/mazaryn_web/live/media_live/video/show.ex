defmodule MazarynWeb.MediaLive.Video.Show do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(params, session, socket) do
    Logger.info("===== VIDEO SHOW MOUNT START =====")
    Logger.info("===== VIDEO SHOW: Params: #{inspect(params)} =====")
    Logger.info("===== VIDEO SHOW: Session keys: #{inspect(Map.keys(session))} =====")

    user = get_user_from_session(session)
    Logger.info("===== VIDEO SHOW: User loaded: #{inspect(user)} =====")

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
     |> assign(:watch_time_total, 0)}
  rescue
    error ->
      Logger.error("===== VIDEO SHOW MOUNT: Exception occurred =====")
      Logger.error("===== VIDEO SHOW MOUNT: Error: #{inspect(error)} =====")
      Logger.error("===== VIDEO SHOW MOUNT: Stacktrace: #{inspect(__STACKTRACE__)} =====")

      {:ok,
       socket
       |> put_flash(:error, "Failed to load video")
       |> push_navigate(to: ~p"/en/videos")}
  end

  @impl true
  def handle_params(%{"id" => video_id} = params, url, socket) do
    Logger.info("===== VIDEO SHOW: handle_params called =====")
    Logger.info("===== VIDEO SHOW: Video ID from params: #{video_id} =====")

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

    Logger.info("===== VIDEO SHOW: Normalized ID: #{inspect(normalized_id)} =====")

    case load_video(normalized_id, user) do
      {:ok, video, creator, related_videos, comments} ->
        Logger.info("===== VIDEO SHOW: Successfully loaded video =====")

        {:noreply,
         socket
         |> assign(:page_title, video.title)
         |> assign(:video, video)
         |> assign(:creator, creator)
         |> assign(:related_videos, related_videos)
         |> assign(:comments, comments)}

      {:error, reason} ->
        Logger.error("===== VIDEO SHOW: Error loading video: #{inspect(reason)} =====")

        {:noreply,
         socket
         |> put_flash(:error, "Video not found")
         |> push_navigate(to: ~p"/#{socket.assigns.locale}/videos")}
    end
  end

  @impl true
  def handle_event("track_watch_time", %{"video_id" => video_id, "seconds" => seconds}, socket) do
    Logger.info("===== TRACK_WATCH_TIME: Video #{video_id}, Seconds: #{seconds} =====")

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
    Logger.info("===== VIDEO_PLAY: Video #{video_id} started playing =====")
    {:noreply, socket}
  end

  @impl true
  def handle_event(
        "video_pause",
        %{"video_id" => video_id, "current_time" => current_time},
        socket
      ) do
    Logger.info("===== VIDEO_PAUSE: Video #{video_id} paused at #{current_time}s =====")
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
    Logger.info("===== ADD_COMMENT: Event triggered =====")
    Logger.info("===== ADD_COMMENT: Comment text: #{comment_text} =====")

    user_id = get_user_id(socket.assigns.user)
    video_id = socket.assigns.video.id

    Logger.info("===== ADD_COMMENT: User ID: #{inspect(user_id)} =====")
    Logger.info("===== ADD_COMMENT: Video ID: #{video_id} =====")

    if String.trim(comment_text) != "" do
      charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

      case :videodb.add_video_comment(user_id, charlist_video_id, comment_text) do
        comment_id when is_binary(comment_id) or is_list(comment_id) ->
          Logger.info("===== ADD_COMMENT: Comment added with ID: #{inspect(comment_id)} =====")
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
          Logger.error("===== ADD_COMMENT: Unexpected response: #{inspect(other)} =====")
          {:noreply, put_flash(socket, :error, "Failed to add comment")}
      end
    else
      Logger.warning("===== ADD_COMMENT: Empty comment text =====")
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("update_comment", %{"value" => text}, socket) do
    Logger.info("===== UPDATE_COMMENT: Text: #{text} =====")
    {:noreply, assign(socket, :comment_text, text)}
  end

  @impl true
  def handle_event("react", %{"type" => reaction_type}, socket) do
    Logger.info("===== REACT: Reaction type: #{reaction_type} =====")

    user_id = get_user_id(socket.assigns.user)
    video_id = socket.assigns.video.id
    reaction_atom = String.to_atom(reaction_type)

    Logger.info("===== REACT: User ID: #{inspect(user_id)} =====")
    Logger.info("===== REACT: Video ID: #{video_id} =====")

    charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    case :videodb.react_to_video(user_id, charlist_video_id, reaction_atom) do
      reaction_id when is_binary(reaction_id) or is_list(reaction_id) ->
        Logger.info("===== REACT: Reaction added with ID: #{inspect(reaction_id)} =====")
        video = reload_video(charlist_video_id)

        {:noreply,
         socket
         |> assign(:video, video)
         |> assign(:user_reaction, reaction_type)}

      {:removed, _type} ->
        Logger.info("===== REACT: Reaction removed =====")
        video = reload_video(charlist_video_id)

        {:noreply,
         socket
         |> assign(:video, video)
         |> assign(:user_reaction, nil)}

      {:error, reason} ->
        Logger.error("===== REACT: Error: #{inspect(reason)} =====")
        {:noreply, socket}

      other ->
        Logger.error("===== REACT: Unexpected response: #{inspect(other)} =====")
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("share", _params, socket) do
    Logger.info("===== SHARE: Event triggered =====")
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
    Logger.info("===== SAVE: Event triggered =====")
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
    Logger.info("===== SUBSCRIBE: Event triggered =====")
    {:noreply, assign(socket, :is_subscribed, true)}
  end

  @impl true
  def handle_event("play_related", %{"id" => video_id}, socket) do
    Logger.info("===== PLAY_RELATED: Navigating to video: #{video_id} =====")
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id}")}
  end

  defp get_user_from_session(%{"session_uuid" => session_uuid}) when session_uuid != nil do
    Logger.info("===== GET_USER_FROM_SESSION: Using session_uuid: #{session_uuid} =====")

    case Account.Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        Logger.info("===== GET_USER_FROM_SESSION: User found via session_uuid =====")
        user

      {:error, reason} ->
        Logger.error(
          "===== GET_USER_FROM_SESSION: Error with session_uuid: #{inspect(reason)} ====="
        )

        nil

      other ->
        Logger.error(
          "===== GET_USER_FROM_SESSION: Unknown session_uuid response: #{inspect(other)} ====="
        )

        nil
    end
  end

  defp get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    Logger.info("===== GET_USER_FROM_SESSION: Using user_id: #{user_id} =====")

    charlist_id = if is_list(user_id), do: user_id, else: String.to_charlist(user_id)

    case Core.UserClient.get_user_by_id(charlist_id) do
      {:error, reason} ->
        Logger.error("===== GET_USER_FROM_SESSION: Error with user_id: #{inspect(reason)} =====")
        nil

      :user_not_exist ->
        Logger.error("===== GET_USER_FROM_SESSION: User does not exist =====")
        nil

      user_tuple when is_tuple(user_tuple) ->
        Logger.info("===== GET_USER_FROM_SESSION: User tuple found =====")
        user_tuple

      other ->
        Logger.error(
          "===== GET_USER_FROM_SESSION: Unknown user_id response: #{inspect(other)} ====="
        )

        nil
    end
  end

  defp get_user_from_session(session) do
    Logger.error("===== GET_USER_FROM_SESSION: No valid session keys found =====")
    Logger.error("===== GET_USER_FROM_SESSION: Session keys: #{inspect(Map.keys(session))} =====")
    nil
  end

  defp get_user_id(%{id: db_id} = _user) when not is_nil(db_id) do
    Logger.info("===== GET_USER_ID: Got Ecto struct with database ID: #{db_id} =====")

    charlist_id = String.to_charlist(to_string(db_id))
    Logger.info("===== GET_USER_ID: Looking up user with charlist: #{inspect(charlist_id)} =====")

    case Core.UserClient.get_user_by_id(charlist_id) do
      user_tuple when is_tuple(user_tuple) ->
        Logger.info("===== GET_USER_ID: Got user tuple =====")
        user_id = elem(user_tuple, 1)
        Logger.info("===== GET_USER_ID: Extracted user_id: #{inspect(user_id)} =====")
        user_id

      {:error, reason} ->
        Logger.error("===== GET_USER_ID: Error getting user tuple: #{inspect(reason)} =====")
        nil

      :user_not_exist ->
        Logger.error("===== GET_USER_ID: User does not exist =====")
        nil

      other ->
        Logger.error("===== GET_USER_ID: Unexpected response: #{inspect(other)} =====")
        nil
    end
  end

  defp get_user_id(user_tuple) when is_tuple(user_tuple) do
    Logger.info("===== GET_USER_ID: Extracting from tuple =====")
    user_id = elem(user_tuple, 1)
    Logger.info("===== GET_USER_ID: User ID: #{inspect(user_id)} =====")
    user_id
  end

  defp get_user_id(nil) do
    Logger.warning("===== GET_USER_ID: User is nil =====")
    nil
  end

  defp get_user_id(user) do
    Logger.error("===== GET_USER_ID: Invalid user format: #{inspect(user)} =====")
    nil
  end

  defp load_video(video_id, user) do
    Logger.info("===== LOAD_VIDEO: Starting to load video: #{inspect(video_id)} =====")

    user_id = get_user_id(user)
    Logger.info("===== LOAD_VIDEO: User ID for tracking: #{inspect(user_id)} =====")

    video_result = try_load_video_formats(video_id)

    case video_result do
      {:ok, video_tuple} ->
        Logger.info("===== LOAD_VIDEO: Video tuple received successfully =====")

        if user_id do
          Logger.info("===== LOAD_VIDEO: Incrementing view counts =====")

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
        Logger.error("===== LOAD_VIDEO: Error from videodb: #{inspect(error)} =====")
        {:error, :not_found}
    end
  rescue
    error ->
      Logger.error("===== LOAD_VIDEO: Exception during load =====")
      Logger.error("===== LOAD_VIDEO: Error: #{inspect(error)} =====")
      {:error, :exception}
  end

  defp try_load_video_formats(video_id) do
    charlist_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    case :videodb.get_video_by_id(charlist_id) do
      {:error, :video_not_found} ->
        {:error, :not_found}

      video_tuple when is_tuple(video_tuple) ->
        {:ok, video_tuple}

      error ->
        Logger.error("===== TRY_LOAD_VIDEO: Unexpected response: #{inspect(error)} =====")
        {:error, :invalid_response}
    end
  end

  defp reload_video(video_id) do
    Logger.info("===== RELOAD_VIDEO: Reloading video: #{inspect(video_id)} =====")

    charlist_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    case :videodb.get_video_by_id(charlist_id) do
      video_tuple when is_tuple(video_tuple) ->
        Logger.info("===== RELOAD_VIDEO: Video reloaded successfully =====")
        format_video(video_tuple)

      error ->
        Logger.error("===== RELOAD_VIDEO: Error reloading: #{inspect(error)} =====")
        nil
    end
  end

  defp format_video(video_tuple) do
    Logger.info("===== FORMAT_VIDEO: Starting format =====")

    video = Mazaryn.Schema.Video.erl_changeset(video_tuple)
    Logger.info("===== FORMAT_VIDEO: Changeset created =====")

    formatted = %{
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

    Logger.info("===== FORMAT_VIDEO: Formatted successfully =====")
    formatted
  rescue
    error ->
      Logger.error("===== FORMAT_VIDEO: Exception during format =====")
      Logger.error("===== FORMAT_VIDEO: Error: #{inspect(error)} =====")
      reraise error, __STACKTRACE__
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

  defp format_duration(seconds) when is_float(seconds) do
    format_duration(round(seconds))
  end

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

  defp format_views(views) when is_integer(views) and views >= 1_000_000 do
    "#{Float.round(views / 1_000_000, 1)}M"
  end

  defp format_views(views) when is_integer(views) and views >= 1_000 do
    "#{Float.round(views / 1_000, 1)}K"
  end

  defp format_views(views) when is_integer(views), do: Integer.to_string(views)
  defp format_views(_), do: "0"

  defp get_creator_info(user_id) do
    Logger.info("===== GET_CREATOR_INFO: Getting info for user: #{inspect(user_id)} =====")

    charlist_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

    case Core.UserClient.get_user_by_id(charlist_id) do
      user_tuple when is_tuple(user_tuple) ->
        Logger.info("===== GET_CREATOR_INFO: User tuple received =====")
        username = elem(user_tuple, 2)
        avatar = elem(user_tuple, 6)

        %{
          id: user_id,
          username: if(is_list(username), do: List.to_string(username), else: username),
          avatar:
            if(is_list(avatar) && length(avatar) > 0,
              do: List.to_string(avatar),
              else: "/images/default-avatar.png"
            ),
          subscribers: "65.6K"
        }

      error ->
        Logger.error("===== GET_CREATOR_INFO: Error getting user: #{inspect(error)} =====")

        %{
          id: user_id,
          username: "Unknown",
          avatar: "/images/default-avatar.png",
          subscribers: "0"
        }
    end
  end

  defp load_related_videos(current_video_id) do
    Logger.info("===== LOAD_RELATED_VIDEOS: Loading related videos =====")

    case :videodb.get_public_videos() do
      videos when is_list(videos) ->
        Logger.info("===== LOAD_RELATED_VIDEOS: Got #{length(videos)} public videos =====")

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

      error ->
        Logger.error("===== LOAD_RELATED_VIDEOS: Error getting videos: #{inspect(error)} =====")
        []
    end
  rescue
    error ->
      Logger.error("===== LOAD_RELATED_VIDEOS: Exception =====")
      Logger.error("===== LOAD_RELATED_VIDEOS: Error: #{inspect(error)} =====")
      []
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

  defp load_comments(_video_id) do
    []
  end
end
