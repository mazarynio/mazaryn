defmodule MazarynWeb.MediaLive.Video.Show do
  use MazarynWeb, :live_view
  require Logger

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
     |> assign(:reaction_counts, %{"like" => 0, "love" => 0, "wow" => 0, "haha" => 0, "fire" => 0})
     |> assign(:total_reactions, 0)
     |> assign(:show_reactions_modal, false)
     |> assign(:all_reaction_users, %{})
     |> assign(:current_reaction_type, "like")
     |> assign(:watch_time_total, 0)
     |> assign(:show_delete_modal, false)
     |> assign(:is_owner, false)
     |> assign(:search_query, "")}
  rescue
    error ->
      Logger.error("===== VIDEO SHOW MOUNT: Exception: #{inspect(error)} =====")

      {:ok,
       socket
       |> put_flash(:error, "Failed to load video")
       |> push_navigate(to: ~p"/en/videos")}
  end

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

        user_reaction = get_user_video_reaction(user, video.id)
        reaction_counts = normalize_reaction_counts(video.reaction_counts)
        total_reactions = Map.values(reaction_counts) |> Enum.sum()

        is_owner = user && get_user_id(user) == video.user_id

        {:noreply,
         socket
         |> assign(:page_title, video.title)
         |> assign(:video, video)
         |> assign(:creator, creator)
         |> assign(:related_videos, related_videos)
         |> assign(:comments, comments)
         |> assign(:user_reaction, user_reaction)
         |> assign(:reaction_counts, reaction_counts)
         |> assign(:total_reactions, total_reactions)
         |> assign(:show_delete_modal, false)
         |> assign(:is_owner, is_owner)}

      {:error, reason} ->
        Logger.error("===== VIDEO SHOW: Error: #{inspect(reason)} =====")

        {:noreply,
         socket
         |> put_flash(:error, "Video not found")
         |> push_navigate(to: ~p"/#{socket.assigns.locale}/videos")}
    end
  end

  @impl true
  def handle_event("ignore", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("view_profile", %{"username" => username}, socket) do
    Logger.info("===== VIEW_PROFILE: Received username: #{inspect(username)} =====")

    locale = socket.assigns.locale || "en"

    username_str =
      case username do
        u when is_binary(u) and byte_size(u) > 0 ->
          Logger.info("===== VIEW_PROFILE: Username is binary: #{u} =====")
          u

        u when is_list(u) and length(u) > 0 ->
          str = List.to_string(u)
          Logger.info("===== VIEW_PROFILE: Username was list, converted to: #{str} =====")
          str

        _ ->
          Logger.error("===== VIEW_PROFILE: Invalid username format: #{inspect(username)} =====")
          nil
      end

    if username_str && username_str != "" && !String.starts_with?(username_str, ["/ip4", "/ip6"]) do
      Logger.info("===== VIEW_PROFILE: Navigating to profile for: #{username_str} =====")

      profile_path = Routes.live_path(socket, MazarynWeb.UserLive.Profile, locale, username_str)
      Logger.info("===== VIEW_PROFILE: Profile path: #{profile_path} =====")

      {:noreply, push_navigate(socket, to: profile_path)}
    else
      Logger.error("===== VIEW_PROFILE: Invalid username, not navigating =====")
      {:noreply, put_flash(socket, :error, "Invalid user profile")}
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
    Logger.info("===== ADD_COMMENT: Received text: '#{comment_text}' =====")

    username =
      case socket.assigns.user do
        %{username: uname} when is_binary(uname) ->
          uname

        %{username: uname} when is_list(uname) ->
          List.to_string(uname)

        user_tuple when is_tuple(user_tuple) ->
          author = elem(user_tuple, 2)
          if is_list(author), do: List.to_string(author), else: author

        _ ->
          nil
      end

    Logger.info("===== ADD_COMMENT: Username extracted: #{inspect(username)} =====")

    video_id = socket.assigns.video.id
    charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    if username && String.trim(comment_text) != "" do
      Logger.info(
        "===== ADD_COMMENT: Calling videodb:add_video_comment(#{inspect(username)}, #{inspect(charlist_video_id)}, #{inspect(comment_text)}) ====="
      )

      case :videodb.add_video_comment(username, charlist_video_id, comment_text) do
        comment_id when is_binary(comment_id) or is_list(comment_id) ->
          Logger.info("===== ADD_COMMENT: Success! Comment ID: #{inspect(comment_id)} =====")

          comments = load_comments(charlist_video_id)
          Logger.info("===== ADD_COMMENT: Loaded #{length(comments)} comments =====")

          {:noreply,
           socket
           |> assign(:comments, comments)
           |> assign(:comment_text, "")
           |> put_flash(:info, "Comment added successfully")}

        {:error, reason} ->
          Logger.error("===== ADD_COMMENT: Backend error: #{inspect(reason)} =====")
          {:noreply, put_flash(socket, :error, "Failed to add comment")}

        other ->
          Logger.error("===== ADD_COMMENT: Unexpected return: #{inspect(other)} =====")
          {:noreply, put_flash(socket, :error, "Failed to add comment")}
      end
    else
      Logger.warn(
        "===== ADD_COMMENT: Skipped – username: #{inspect(username)}, text empty: #{String.trim(comment_text) == ""} ====="
      )

      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("update_comment", %{"value" => text}, socket) do
    {:noreply, assign(socket, :comment_text, text)}
  end

  @impl true
  def handle_event("delete_comment", %{"comment-id" => comment_id}, socket) do
    video_id = socket.assigns.video.id
    charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    charlist_comment_id =
      if is_binary(comment_id), do: String.to_charlist(comment_id), else: comment_id

    case :videodb.delete_video_comment(charlist_comment_id, charlist_video_id) do
      {:ok, :comment_deleted} ->
        comments = load_comments(charlist_video_id)

        {:noreply,
         socket
         |> assign(:comments, comments)
         |> put_flash(:info, "Comment deleted")}

      {:error, reason} ->
        Logger.error("===== DELETE_COMMENT: Error: #{inspect(reason)} =====")
        {:noreply, put_flash(socket, :error, "Failed to delete comment")}

      other ->
        Logger.error("===== DELETE_COMMENT: Unexpected: #{inspect(other)} =====")
        {:noreply, put_flash(socket, :error, "Failed to delete comment")}
    end
  end

  @impl true
  def handle_event("react_to_video", %{"reaction-type" => reaction_type}, socket) do
    user_id = get_user_id(socket.assigns.user)
    video_id = socket.assigns.video.id

    if user_id do
      try do
        reaction_atom = String.to_existing_atom(reaction_type)

        charlist_video_id =
          if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

        charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

        result = :videodb.react_to_video(charlist_user_id, charlist_video_id, reaction_atom)

        Process.sleep(50)

        video = reload_video(charlist_video_id)

        user_reaction =
          case :videodb.get_user_reaction_type(charlist_user_id, charlist_video_id) do
            reaction when is_atom(reaction) and reaction != :undefined -> reaction
            _ -> nil
          end

        reaction_counts = normalize_reaction_counts(video.reaction_counts)
        total_reactions = Map.values(reaction_counts) |> Enum.sum()

        {:noreply,
         socket
         |> assign(:video, video)
         |> assign(:user_reaction, user_reaction)
         |> assign(:reaction_counts, reaction_counts)
         |> assign(:total_reactions, total_reactions)}
      rescue
        e ->
          Logger.error("Error in react_to_video: #{inspect(e)}")
          {:noreply, put_flash(socket, :error, "Failed to react to video")}
      end
    else
      {:noreply, put_flash(socket, :error, "Please login to react")}
    end
  end

  @impl true
  def handle_event("open_reactions_modal", %{"reaction-type" => reaction_type}, socket) do
    Logger.info("===== OPEN_REACTIONS_MODAL: Starting for reaction type: #{reaction_type} =====")

    video_id = socket.assigns.video.id
    charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    all_reactions_map =
      try do
        :videodb.get_all_reactions(charlist_video_id)
      rescue
        e ->
          Logger.error(
            "===== OPEN_REACTIONS_MODAL: Error fetching reactions: #{inspect(e)} ====="
          )

          %{like: [], love: [], wow: [], haha: [], fire: []}
      end

    Logger.info(
      "===== OPEN_REACTIONS_MODAL: Got reactions map: #{inspect(Map.keys(all_reactions_map))} ====="
    )

    grouped_users =
      Enum.into(all_reactions_map, %{}, fn {type, reactions_list} ->
        type_str = to_string(type)

        Logger.info(
          "===== OPEN_REACTIONS_MODAL: Processing #{type_str} reactions, count: #{length(reactions_list)} ====="
        )

        users =
          Enum.map(reactions_list, fn reaction ->
            user_id =
              case reaction do
                %{userID: uid} -> uid
                {_, _, _, _, uid, _, _, _} -> uid
                _ -> nil
              end

            Logger.info(
              "===== OPEN_REACTIONS_MODAL: Processing user_id: #{inspect(user_id)} ====="
            )

            if user_id do
              charlist_user_id =
                if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

              case Core.UserClient.get_user_by_id(charlist_user_id) do
                {:error, reason} ->
                  Logger.error(
                    "===== OPEN_REACTIONS_MODAL: Error getting user: #{inspect(reason)} ====="
                  )

                  %{username: "Anonymous", avatar: "/images/default-avatar.png", verified: false}

                user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 9 ->
                  username = elem(user_tuple, 8)
                  avatar = elem(user_tuple, 6)

                  Logger.info(
                    "===== OPEN_REACTIONS_MODAL: Raw username: #{inspect(username)} ====="
                  )

                  Logger.info("===== OPEN_REACTIONS_MODAL: Raw avatar: #{inspect(avatar)} =====")

                  username_str =
                    case username do
                      u when is_list(u) and length(u) > 0 ->
                        str = List.to_string(u)

                        Logger.info(
                          "===== OPEN_REACTIONS_MODAL: Converted username from list: #{str} ====="
                        )

                        if String.starts_with?(str, ["/ip4", "/ip6"]), do: "Anonymous", else: str

                      u when is_binary(u) and u != "" ->
                        Logger.info("===== OPEN_REACTIONS_MODAL: Username is binary: #{u} =====")
                        if String.starts_with?(u, ["/ip4", "/ip6"]), do: "Anonymous", else: u

                      _ ->
                        Logger.warn(
                          "===== OPEN_REACTIONS_MODAL: Username format unknown, defaulting to Anonymous ====="
                        )

                        "Anonymous"
                    end

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

                  Logger.info("===== OPEN_REACTIONS_MODAL: Final username: #{username_str} =====")
                  Logger.info("===== OPEN_REACTIONS_MODAL: Final avatar: #{avatar_str} =====")

                  %{
                    username: username_str,
                    avatar: avatar_str,
                    verified: false
                  }

                _ ->
                  Logger.warn("===== OPEN_REACTIONS_MODAL: User tuple invalid or too small =====")
                  %{username: "Unknown", avatar: "/images/default-avatar.png", verified: false}
              end
            else
              Logger.warn("===== OPEN_REACTIONS_MODAL: No user_id found in reaction =====")
              %{username: "Unknown", avatar: "/images/default-avatar.png", verified: false}
            end
          end)
          |> Enum.filter(fn user ->
            keep = user.username not in ["Anonymous", "Unknown"]

            if !keep do
              Logger.info(
                "===== OPEN_REACTIONS_MODAL: Filtering out user: #{user.username} ====="
              )
            end

            keep
          end)

        Logger.info(
          "===== OPEN_REACTIONS_MODAL: Final user count for #{type_str}: #{length(users)} ====="
        )

        {type_str, users}
      end)

    Logger.info(
      "===== OPEN_REACTIONS_MODAL: Grouped users summary: #{inspect(Enum.map(grouped_users, fn {k, v} -> {k, length(v)} end))} ====="
    )

    {:noreply,
     socket
     |> assign(:show_reactions_modal, true)
     |> assign(:current_reaction_type, reaction_type)
     |> assign(:all_reaction_users, grouped_users)}
  end

  @impl true
  def handle_event("close_reactions_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_reactions_modal, false)
     |> assign(:all_reaction_users, %{})
     |> assign(:current_reaction_type, "like")}
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

  def handle_event("show_delete_modal", _params, socket) do
    Logger.info("🗑 Showing delete video modal")
    {:noreply, assign(socket, show_delete_modal: true)}
  end

  def handle_event("hide_delete_modal", _params, socket) do
    Logger.info("🚫 Hiding delete video modal")
    {:noreply, assign(socket, show_delete_modal: false)}
  end

  def handle_event("confirm_delete_video", %{"id" => video_id}, socket) do
    delete_start = :erlang.system_time(:millisecond)
    Logger.info("🗑 Starting confirm_delete_video for video #{video_id}")

    Task.start(fn ->
      try do
        :videodb.delete_video(String.to_charlist(video_id))
        Logger.info("✅ Video #{video_id} deleted successfully")
      rescue
        error -> Logger.error("Error deleting video: #{inspect(error)}")
      end
    end)

    socket =
      socket
      |> assign(show_delete_modal: false)
      |> put_flash(:info, "Video deleted successfully")
      |> push_navigate(to: ~p"/#{socket.assigns.locale}/videos")

    delete_end = :erlang.system_time(:millisecond)
    Logger.info("🗑 confirm_delete_video completed in #{delete_end - delete_start}ms")
    {:noreply, socket}
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

  defp load_comments(video_id) do
    Logger.info("===== LOAD_COMMENTS: Loading for video ID: #{inspect(video_id)} =====")
    charlist_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

    case :videodb.get_video_comments(charlist_id) do
      comments when is_list(comments) ->
        result =
          Enum.map(comments, fn comment_tuple ->
            comment_id = elem(comment_tuple, 1)
            user_id = elem(comment_tuple, 2)

            content =
              case :videodb.get_video_comment_content(comment_id) do
                {error, reason} ->
                  Logger.error("===== LOAD_COMMENTS: Failed to load content for comment ~p: ~p", [
                    comment_id,
                    reason
                  ])

                  "[Content unavailable]"

                binary_content when is_binary(binary_content) ->
                  binary_content

                list_content when is_list(list_content) ->
                  List.to_string(list_content)

                _ ->
                  "[Content loading...]"
              end

            username =
              case Core.UserClient.get_user_by_id(user_id) do
                {:error, _} ->
                  Logger.warn("===== LOAD_COMMENTS: Could not get user for comment =====")
                  "Anonymous"

                user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 9 ->
                  raw_username = elem(user_tuple, 8)

                  Logger.info(
                    "===== LOAD_COMMENTS: Raw username from tuple: #{inspect(raw_username)} ====="
                  )

                  case raw_username do
                    u when is_list(u) and length(u) > 0 ->
                      str = List.to_string(u)
                      Logger.info("===== LOAD_COMMENTS: Converted username: #{str} =====")
                      str

                    u when is_binary(u) and u != "" ->
                      Logger.info("===== LOAD_COMMENTS: Username is binary: #{u} =====")
                      u

                    _ ->
                      Logger.warn("===== LOAD_COMMENTS: Username format unknown =====")
                      "Anonymous"
                  end

                _ ->
                  Logger.warn("===== LOAD_COMMENTS: User tuple invalid =====")
                  "Anonymous"
              end

            Logger.info("===== LOAD_COMMENTS: Final username for comment: #{username} =====")

            %{
              id: if(is_list(comment_id), do: List.to_string(comment_id), else: comment_id),
              author: username,
              content: content
            }
          end)

        Logger.info("===== LOAD_COMMENTS: Returning #{length(result)} formatted comments =====")
        result

      other ->
        Logger.warning("===== LOAD_COMMENTS: videodb returned: #{inspect(other)} =====")
        []
    end
  end

  defp get_user_video_reaction(user, video_id) do
    user_id = get_user_id(user)

    if user_id do
      charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id
      charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

      case :videodb.get_user_reaction_type(charlist_user_id, charlist_video_id) do
        reaction when is_atom(reaction) and reaction != :undefined -> reaction
        _ -> nil
      end
    else
      nil
    end
  end

  defp normalize_reaction_counts(counts) when is_map(counts) do
    %{
      "like" => Map.get(counts, :like, Map.get(counts, "like", 0)),
      "love" => Map.get(counts, :love, Map.get(counts, "love", 0)),
      "wow" => Map.get(counts, :wow, Map.get(counts, "wow", 0)),
      "haha" => Map.get(counts, :haha, Map.get(counts, "haha", 0)),
      "fire" => Map.get(counts, :fire, Map.get(counts, "fire", 0))
    }
  end

  defp normalize_reaction_counts(_) do
    %{"like" => 0, "love" => 0, "wow" => 0, "haha" => 0, "fire" => 0}
  end
end
