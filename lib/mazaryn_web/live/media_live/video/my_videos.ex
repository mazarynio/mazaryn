defmodule MazarynWeb.MediaLive.Video.MyVideos do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    Logger.info("===== MY_VIDEOS MOUNT: Session keys: #{inspect(Map.keys(session))} =====")

    user = get_user_from_session(session)
    Logger.info("===== MY_VIDEOS MOUNT: User: #{inspect(user)} =====")

    {:ok,
     socket
     |> assign(:page_title, "My Videos")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:active_tab, "all")
     |> assign(:videos, [])
     |> assign(:uploaded_videos, [])
     |> assign(:live_streams, [])
     |> assign(:scheduled, [])
     |> assign(:show_delete_modal, false)
     |> assign(:video_to_delete, nil)
     |> load_user_videos()}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, :active_tab, tab)}
  end

  @impl true
  def handle_event("show_delete_modal", %{"id" => video_id}, socket) do
    Logger.info("===== SHOW_DELETE_MODAL: Video #{video_id} =====")
    {:noreply, socket |> assign(:show_delete_modal, true) |> assign(:video_to_delete, video_id)}
  end

  @impl true
  def handle_event("hide_delete_modal", _params, socket) do
    {:noreply, socket |> assign(:show_delete_modal, false) |> assign(:video_to_delete, nil)}
  end

  @impl true
  def handle_event("confirm_delete_video", _params, socket) do
    Logger.info("===== CONFIRM_DELETE_VIDEO: Starting =====")

    user_id = get_user_id(socket.assigns.user)
    video_id = socket.assigns.video_to_delete

    Logger.info("===== CONFIRM_DELETE_VIDEO: User ID: #{inspect(user_id)} =====")
    Logger.info("===== CONFIRM_DELETE_VIDEO: Video ID: #{inspect(video_id)} =====")

    if is_nil(user_id) or is_nil(video_id) do
      Logger.error("===== CONFIRM_DELETE_VIDEO: Missing user ID or video ID =====")

      {:noreply,
       socket
       |> assign(:show_delete_modal, false)
       |> assign(:video_to_delete, nil)
       |> put_flash(:error, "Unable to delete video")}
    else
      charlist_video_id = if is_binary(video_id), do: String.to_charlist(video_id), else: video_id

      case :videodb.delete_video(charlist_video_id, user_id) do
        :ok ->
          Logger.info("===== CONFIRM_DELETE_VIDEO: Success =====")

          try do
            video_id_str = if is_list(video_id), do: List.to_string(video_id), else: video_id

            case :ets.lookup(:video_path_cache, video_id_str) do
              [{^video_id_str, local_path, _timestamp}] ->
                Logger.info("===== CONFIRM_DELETE_VIDEO: Found in cache =====")

                spawn(fn ->
                  if File.exists?(local_path) do
                    File.rm(local_path)
                    Logger.info("===== CONFIRM_DELETE_VIDEO: Local file deleted =====")
                  end
                end)

                :ets.delete(:video_path_cache, video_id_str)

              [] ->
                Logger.info("===== CONFIRM_DELETE_VIDEO: Not in cache =====")
            end
          rescue
            error ->
              Logger.error(
                "===== CONFIRM_DELETE_VIDEO: Cache cleanup error: #{inspect(error)} ====="
              )

              :ok
          end

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> assign(:video_to_delete, nil)
           |> put_flash(:info, "Video deleted successfully")
           |> load_user_videos()}

        {:error, :unauthorized} ->
          Logger.error("===== CONFIRM_DELETE_VIDEO: Unauthorized =====")

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> assign(:video_to_delete, nil)
           |> put_flash(:error, "You are not authorized to delete this video")}

        {:error, :video_not_found} ->
          Logger.error("===== CONFIRM_DELETE_VIDEO: Video not found =====")

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> assign(:video_to_delete, nil)
           |> put_flash(:error, "Video not found")}

        {:error, reason} ->
          Logger.error("===== CONFIRM_DELETE_VIDEO: Error: #{inspect(reason)} =====")

          {:noreply,
           socket
           |> assign(:show_delete_modal, false)
           |> assign(:video_to_delete, nil)
           |> put_flash(:error, "Failed to delete video")}
      end
    end
  end

  @impl true
  def handle_event("edit_video", %{"id" => video_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id}/edit")}
  end

  @impl true
  def handle_event("view_video", %{"id" => video_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id}")}
  end

  @impl true
  def handle_event("navigate_to_upload", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/upload")}
  end

  @impl true
  def handle_info({:view_video, video_id}, socket) do
    Logger.info("===== VIEW_VIDEO INFO: Navigating to video #{video_id} =====")
    video_id_string = to_string(video_id)
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id_string}")}
  end

  @impl true
  def handle_info({:edit_video, video_id}, socket) do
    Logger.info("===== EDIT_VIDEO INFO: Navigating to edit #{video_id} =====")
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id}/edit")}
  end

  defp video_card(assigns) do
    ~H"""
    <div class="p-5 bg-[#323340] rounded-[20px] shadow-lg">
      <div class="flex gap-5">
        <div class="relative">
          <img
            src={@video.thumbnail_url}
            class="w-64 h-44 rounded-md object-cover cursor-pointer hover:opacity-80 transition-opacity"
            alt={@video.title}
            phx-click="view_video"
            phx-value-id={@video.id}
          />
          <%= if @video.is_live do %>
            <span class="absolute bottom-2 right-2 px-2 py-1 bg-red-600 text-white text-xs font-bold rounded">
              LIVE
            </span>
          <% else %>
            <%= if @video.status == "processing" do %>
              <span class="absolute bottom-2 right-2 px-2 py-1 bg-yellow-600 text-white text-xs font-bold rounded">
                PROCESSING
              </span>
            <% end %>
            <%= if @video.status == "failed" do %>
              <span class="absolute bottom-2 right-2 px-2 py-1 bg-red-600 text-white text-xs font-bold rounded">
                FAILED
              </span>
            <% end %>
          <% end %>
        </div>

        <div class="flex-1 space-y-3">
          <div>
            <h3
              class="text-white text-lg font-semibold font-['Poppins'] cursor-pointer hover:text-blue-500 transition-colors"
              phx-click="view_video"
              phx-value-id={@video.id}
            >
              <%= @video.title %>
            </h3>
            <p class="text-neutral-400 text-sm font-['Poppins'] mt-1"><%= @video.url_slug %></p>
          </div>

          <div class="p-3 bg-[#27282e] rounded-lg">
            <p class="text-gray-300 text-sm font-['Poppins'] line-clamp-2">
              <%= @video.description %>
            </p>
          </div>

          <div class="flex items-center justify-between">
            <div class="flex items-center gap-4 text-gray-400 text-sm">
              <span><%= @video.views %> views</span>
              <span>•</span>
              <span><%= @video.created_at %></span>
            </div>

            <div class="flex gap-2">
              <button
                phx-click="edit_video"
                phx-value-id={@video.id}
                class="p-2 rounded-lg bg-[#27282e] hover:bg-blue-500 transition-colors group"
                title="Edit video"
              >
                <Heroicons.pencil class="w-5 h-5 text-gray-400 group-hover:text-white" />
              </button>

              <button
                phx-click="show_delete_modal"
                phx-value-id={@video.id}
                class="p-2 rounded-lg bg-[#27282e] hover:bg-red-500 transition-colors group"
                title="Delete video"
              >
                <Heroicons.trash class="w-5 h-5 text-gray-400 group-hover:text-white" />
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
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

  defp get_user_id(nil), do: nil

  defp get_user_id(user) do
    Logger.error("===== GET_USER_ID: Invalid user format: #{inspect(user)} =====")
    nil
  end

  defp load_user_videos(socket) do
    user_id = get_user_id(socket.assigns.user)
    Logger.info("===== LOAD_USER_VIDEOS: Loading videos for user: #{inspect(user_id)} =====")

    if is_nil(user_id) do
      Logger.error("===== LOAD_USER_VIDEOS: No user_id found =====")

      socket
      |> assign(:videos, [])
      |> assign(:uploaded_videos, [])
      |> assign(:live_streams, [])
      |> assign(:scheduled, [])
    else
      case :videodb.get_videos_by_creator(user_id) do
        videos when is_list(videos) ->
          Logger.info("===== LOAD_USER_VIDEOS: Found #{length(videos)} videos =====")
          formatted_videos = Enum.map(videos, &format_video/1)

          all_videos = formatted_videos

          uploaded_videos =
            Enum.filter(formatted_videos, &(&1.status in ["ready", "processing", "failed"]))

          live_streams = Enum.filter(formatted_videos, & &1.is_live)
          scheduled = Enum.filter(formatted_videos, &(&1.visibility == "scheduled"))

          Logger.info(
            "===== LOAD_USER_VIDEOS: All: #{length(all_videos)}, Uploaded: #{length(uploaded_videos)}, Live: #{length(live_streams)}, Scheduled: #{length(scheduled)} ====="
          )

          socket
          |> assign(:videos, all_videos)
          |> assign(:uploaded_videos, uploaded_videos)
          |> assign(:live_streams, live_streams)
          |> assign(:scheduled, scheduled)

        error ->
          Logger.error("===== LOAD_USER_VIDEOS: Error loading videos: #{inspect(error)} =====")

          socket
          |> assign(:videos, [])
          |> assign(:uploaded_videos, [])
          |> assign(:live_streams, [])
          |> assign(:scheduled, [])
      end
    end
  end

  defp format_video(video_tuple) do
    video = Mazaryn.Schema.Video.erl_changeset(video_tuple)

    %{
      id: video.changes.id,
      title: video.changes.title || "Untitled",
      description: video.changes.description || "No description available",
      thumbnail_url: get_thumbnail_url(video.changes),
      url_slug: Map.get(video.changes, :file_url, "mazaryn.xyz://#{video.changes.id}"),
      duration: format_duration(video.changes.duration_seconds),
      views: Map.get(video.changes, :views, 0),
      status: to_string_safe(video.changes.status, "ready"),
      visibility: to_string_safe(video.changes.privacy, "public"),
      created_at: format_time_ago(video.changes.date_created),
      is_live: Map.get(video.changes, :is_live, false),
      upload_progress: get_upload_progress(video.changes)
    }
  end

  defp to_string_safe(value, _default) when is_binary(value), do: value

  defp to_string_safe(value, _default) when is_atom(value) and not is_nil(value),
    do: Atom.to_string(value)

  defp to_string_safe(_value, default), do: default

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

  defp get_upload_progress(video) do
    status = Map.get(video, :status, :ready)

    case status do
      :processing -> 50
      :ready -> 100
      :failed -> 0
      _ -> 0
    end
  end
end
