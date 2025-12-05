defmodule MazarynWeb.MediaLive.Video.Upload do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    Logger.info("===== UPLOAD MOUNT: Starting =====")
    Logger.info("===== UPLOAD MOUNT: Session keys: #{inspect(Map.keys(session))} =====")

    user = get_user_from_session(session)
    Logger.info("===== UPLOAD MOUNT: User loaded: #{inspect(user)} =====")

    socket =
      socket
      |> assign(:user, user)
      |> assign(:locale, session["locale"] || "en")
      |> assign(:step, :upload)
      |> assign(:title, "")
      |> assign(:description, "")
      |> assign(:visibility, "public")
      |> assign(:tags, [])
      |> assign(:tag_input, "")
      |> assign(:language, "en")
      |> assign(:uploading, false)
      |> assign(:upload_progress, 0)
      |> allow_upload(:video,
        accept: ~w(video/mp4 video/webm video/quicktime video/x-msvideo video/x-matroska),
        max_entries: 1,
        max_file_size: 10_000_000_000,
        auto_upload: true,
        progress: &handle_progress/3,
        chunk_size: 64_000
      )
      |> allow_upload(:thumbnail,
        accept: ~w(image/jpeg image/png image/webp),
        max_entries: 1,
        max_file_size: 5_000_000,
        auto_upload: true,
        progress: &handle_progress/3
      )

    Logger.info("===== UPLOAD MOUNT: Complete =====")
    {:ok, socket}
  end

  defp handle_progress(:video, entry, socket) do
    Logger.info("===== VIDEO PROGRESS: #{entry.progress}% - Done: #{entry.done?} =====")

    if entry.done? do
      Logger.info("===== VIDEO PROGRESS: Upload complete for #{entry.client_name} =====")
    end

    {:noreply, socket}
  end

  defp handle_progress(:thumbnail, entry, socket) do
    Logger.info("===== THUMBNAIL PROGRESS: #{entry.progress}% =====")
    {:noreply, socket}
  end

  @impl true
  def handle_event("validate_video", _params, socket) do
    Logger.info("===== VALIDATE_VIDEO: Event triggered =====")
    {:noreply, socket}
  end

  @impl true
  def handle_event("validate_thumbnail", _params, socket) do
    Logger.info("===== VALIDATE_THUMBNAIL: Event triggered =====")
    {:noreply, socket}
  end

  @impl true
  def handle_event("cancel_video_upload", %{"ref" => ref}, socket) do
    Logger.info("===== CANCEL_VIDEO_UPLOAD: Ref #{ref} =====")
    {:noreply, cancel_upload(socket, :video, ref)}
  end

  @impl true
  def handle_event("cancel_thumbnail_upload", %{"ref" => ref}, socket) do
    Logger.info("===== CANCEL_THUMBNAIL_UPLOAD: Ref #{ref} =====")
    {:noreply, cancel_upload(socket, :thumbnail, ref)}
  end

  @impl true
  def handle_event("update_title", %{"value" => title}, socket) do
    Logger.info("===== UPDATE_TITLE: '#{title}' =====")
    {:noreply, assign(socket, :title, title)}
  end

  @impl true
  def handle_event("update_description", %{"value" => description}, socket) do
    Logger.info("===== UPDATE_DESCRIPTION: Length #{String.length(description)} =====")
    {:noreply, assign(socket, :description, description)}
  end

  @impl true
  def handle_event("select_visibility", %{"visibility" => visibility}, socket) do
    Logger.info("===== SELECT_VISIBILITY: #{visibility} =====")
    {:noreply, assign(socket, :visibility, visibility)}
  end

  @impl true
  def handle_event("add_tag", %{"tag" => tag}, socket) when tag != "" do
    Logger.info("===== ADD_TAG: '#{tag}' =====")

    if length(socket.assigns.tags) < 5 and tag not in socket.assigns.tags do
      Logger.info("===== ADD_TAG: Tag added =====")
      {:noreply, socket |> assign(:tags, [tag | socket.assigns.tags]) |> assign(:tag_input, "")}
    else
      Logger.warning("===== ADD_TAG: Max tags reached or duplicate =====")
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("add_tag", _params, socket) do
    Logger.info("===== ADD_TAG: Empty tag =====")
    {:noreply, socket}
  end

  @impl true
  def handle_event("remove_tag", %{"tag" => tag}, socket) do
    Logger.info("===== REMOVE_TAG: '#{tag}' =====")
    {:noreply, assign(socket, :tags, List.delete(socket.assigns.tags, tag))}
  end

  @impl true
  def handle_event("update_language", %{"value" => language}, socket) do
    Logger.info("===== UPDATE_LANGUAGE: #{language} =====")
    {:noreply, assign(socket, :language, language)}
  end

  @impl true
  def handle_event("proceed_to_confirm", _params, socket) do
    Logger.info("===== PROCEED_TO_CONFIRM: Starting =====")
    Logger.info("===== PROCEED_TO_CONFIRM: Title: '#{socket.assigns.title}' =====")

    Logger.info(
      "===== PROCEED_TO_CONFIRM: Video entries count: #{length(socket.assigns.uploads.video.entries)} ====="
    )

    title = String.trim(socket.assigns.title)
    has_file = length(socket.assigns.uploads.video.entries) > 0

    Logger.info("===== PROCEED_TO_CONFIRM: Title valid: #{title != ""} =====")
    Logger.info("===== PROCEED_TO_CONFIRM: Has file: #{has_file} =====")

    video_uploaded =
      Enum.all?(socket.assigns.uploads.video.entries, fn entry ->
        is_done = entry.done?
        Logger.info("===== PROCEED_TO_CONFIRM: Entry #{entry.client_name} done: #{is_done} =====")
        is_done
      end)

    Logger.info("===== PROCEED_TO_CONFIRM: All videos uploaded: #{video_uploaded} =====")

    cond do
      title == "" || !has_file ->
        Logger.warning("===== PROCEED_TO_CONFIRM: Validation failed =====")
        {:noreply, put_flash(socket, :error, "Please fill in title and select video")}

      !video_uploaded ->
        Logger.warning("===== PROCEED_TO_CONFIRM: Video still uploading =====")
        {:noreply, put_flash(socket, :info, "Please wait for video to finish uploading")}

      true ->
        Logger.info("===== PROCEED_TO_CONFIRM: Proceeding to confirm =====")
        {:noreply, assign(socket, :step, :confirm)}
    end
  end

  @impl true
  def handle_event("back_to_upload", _params, socket) do
    Logger.info("===== BACK_TO_UPLOAD: Returning to upload step =====")
    {:noreply, assign(socket, :step, :upload)}
  end

  @impl true
  def handle_event("confirm_upload", _params, socket) do
    Logger.info("===== CONFIRM_UPLOAD: Starting =====")
    Logger.info("===== CONFIRM_UPLOAD: User: #{inspect(socket.assigns.user)} =====")

    user_id = get_user_id(socket.assigns.user)
    Logger.info("===== CONFIRM_UPLOAD: User ID: #{inspect(user_id)} =====")

    if is_nil(user_id) do
      Logger.error("===== CONFIRM_UPLOAD: No user ID =====")
      {:noreply, put_flash(socket, :error, "User session expired")}
    else
      video_ready = Enum.all?(socket.assigns.uploads.video.entries, fn entry -> entry.done? end)
      Logger.info("===== CONFIRM_UPLOAD: Video ready: #{video_ready} =====")

      if !video_ready do
        Logger.error("===== CONFIRM_UPLOAD: Video not ready =====")
        {:noreply, put_flash(socket, :error, "Please wait for video to finish uploading")}
      else
        socket = assign(socket, uploading: true, upload_progress: 0)

        Logger.info("===== CONFIRM_UPLOAD: Starting consume_video_to_path =====")

        case consume_video_to_path(socket) do
          nil ->
            Logger.error("===== CONFIRM_UPLOAD: consume_video_to_path returned nil =====")

            {:noreply,
             socket
             |> assign(uploading: false)
             |> put_flash(:error, "Failed to process video file")}

          video_path ->
            Logger.info("===== CONFIRM_UPLOAD: Video path obtained: #{video_path} =====")

            thumbnail_path = consume_thumbnail_to_path(socket)
            Logger.info("===== CONFIRM_UPLOAD: Thumbnail path: #{inspect(thumbnail_path)} =====")

            title = socket.assigns.title
            description = socket.assigns.description
            visibility_atom = String.to_atom(socket.assigns.visibility)
            tags = socket.assigns.tags

            Logger.info("===== CONFIRM_UPLOAD: Creating video =====")

            video_path_charlist = String.to_charlist(video_path)
            Logger.info("===== CONFIRM_UPLOAD: Calling videodb =====")

            result =
              :videodb.create_video_with_rust(
                user_id,
                title,
                description,
                video_path_charlist,
                0,
                visibility_atom,
                tags,
                true,
                true,
                false
              )

            Logger.info("===== CONFIRM_UPLOAD: videodb result: #{inspect(result)} =====")

            video_id = extract_video_id(result)
            Logger.info("===== CONFIRM_UPLOAD: Video ID: #{inspect(video_id)} =====")

            if video_id do
              Logger.info("===== CONFIRM_UPLOAD: Caching video path =====")
              cache_video_path(video_id, video_path)

              if thumbnail_path do
                spawn(fn ->
                  case File.read(thumbnail_path) do
                    {:ok, thumb_content} ->
                      upload_thumbnail_to_ipfs(video_id, thumb_content)
                      schedule_cleanup(thumbnail_path, 20_000)

                    _ ->
                      :ok
                  end
                end)
              end

              Logger.info("===== CONFIRM_UPLOAD: Success =====")

              {:noreply,
               socket
               |> assign(uploading: false, upload_progress: 100, step: :success)
               |> put_flash(:info, "Video uploaded successfully!")}
            else
              Logger.error("===== CONFIRM_UPLOAD: No video ID =====")
              schedule_cleanup(video_path, 100)
              {:noreply, socket |> assign(uploading: false) |> put_flash(:error, "Upload failed")}
            end
        end
      end
    end
  rescue
    error ->
      Logger.error("===== CONFIRM_UPLOAD: Exception: #{inspect(error)} =====")
      Logger.error("===== CONFIRM_UPLOAD: Stacktrace: #{inspect(__STACKTRACE__)} =====")

      {:noreply,
       socket |> assign(uploading: false) |> put_flash(:error, "Upload error: #{inspect(error)}")}
  end

  @impl true
  def handle_event("cancel_upload", _params, socket) do
    Logger.info("===== CANCEL_UPLOAD: Canceling =====")
    {:noreply, socket |> assign(:step, :upload) |> assign(:uploading, false)}
  end

  @impl true
  def handle_event("view_my_videos", _params, socket) do
    Logger.info("===== VIEW_MY_VIDEOS: Redirecting =====")
    {:noreply, push_navigate(socket, to: "/#{socket.assigns.locale}/videos/my-videos")}
  end

  defp consume_video_to_path(socket) do
    Logger.info("===== CONSUME_VIDEO: Starting =====")

    if length(socket.assigns.uploads.video.entries) == 0 do
      Logger.error("===== CONSUME_VIDEO: No entries =====")
      nil
    else
      result =
        consume_uploaded_entries(socket, :video, fn %{path: path}, entry ->
          Logger.info("===== CONSUME_VIDEO: Processing =====")
          Logger.info("===== CONSUME_VIDEO: Filename: #{entry.client_name} =====")
          Logger.info("===== CONSUME_VIDEO: Temp path: #{path} =====")

          dir = Application.get_env(:mazaryn, :media)[:uploads_dir] || "./priv/static/uploads"
          ext = get_video_extension(entry)
          dest = Path.join(dir, "#{entry.uuid}.#{ext}")

          Logger.info("===== CONSUME_VIDEO: Destination: #{dest} =====")

          File.mkdir_p!(Path.dirname(dest))

          if File.exists?(path) do
            Logger.info("===== CONSUME_VIDEO: Source exists =====")

            case File.cp(path, dest) do
              :ok ->
                Logger.info("===== CONSUME_VIDEO: Copy success =====")

                if File.exists?(dest) do
                  Logger.info("===== CONSUME_VIDEO: Dest verified =====")
                  dest
                else
                  Logger.error("===== CONSUME_VIDEO: Dest not found =====")
                  nil
                end

              {:error, reason} ->
                Logger.error("===== CONSUME_VIDEO: Copy failed: #{inspect(reason)} =====")
                nil
            end
          else
            Logger.error("===== CONSUME_VIDEO: Source not found =====")
            nil
          end
        end)

      Logger.info("===== CONSUME_VIDEO: Result: #{inspect(result)} =====")

      case result do
        [path] when is_binary(path) ->
          Logger.info("===== CONSUME_VIDEO: Got string path: #{path} =====")
          path

        [{:ok, path}] ->
          Logger.info("===== CONSUME_VIDEO: Got tuple path: #{path} =====")
          path

        [path | _] when is_binary(path) ->
          Logger.info("===== CONSUME_VIDEO: Got first path: #{path} =====")
          path

        [] ->
          Logger.error("===== CONSUME_VIDEO: Empty result =====")
          nil

        other ->
          Logger.error("===== CONSUME_VIDEO: Unexpected: #{inspect(other)} =====")
          nil
      end
    end
  rescue
    error ->
      Logger.error("===== CONSUME_VIDEO: Exception: #{inspect(error)} =====")
      Logger.error("===== CONSUME_VIDEO: Stacktrace: #{inspect(__STACKTRACE__)} =====")
      nil
  end

  defp consume_thumbnail_to_path(socket) do
    Logger.info("===== CONSUME_THUMBNAIL: Starting =====")

    if length(socket.assigns.uploads.thumbnail.entries) == 0 do
      Logger.info("===== CONSUME_THUMBNAIL: No entries =====")
      nil
    else
      result =
        consume_uploaded_entries(socket, :thumbnail, fn %{path: path}, entry ->
          dir = Application.get_env(:mazaryn, :media)[:uploads_dir] || "./priv/static/uploads"
          ext = Path.extname(entry.client_name)
          dest = Path.join(dir, "#{entry.uuid}#{ext}")

          File.mkdir_p!(Path.dirname(dest))

          case File.cp(path, dest) do
            :ok ->
              Logger.info("===== CONSUME_THUMBNAIL: Success =====")
              dest

            {:error, reason} ->
              Logger.error("===== CONSUME_THUMBNAIL: Failed: #{inspect(reason)} =====")
              nil
          end
        end)

      case result do
        [path] when is_binary(path) -> path
        [{:ok, path}] -> path
        _ -> nil
      end
    end
  rescue
    error ->
      Logger.error("===== CONSUME_THUMBNAIL: Exception: #{inspect(error)} =====")
      nil
  end

  defp cache_video_path(video_id, local_path) do
    Logger.info("===== CACHE_VIDEO_PATH: Starting =====")
    Logger.info("===== CACHE_VIDEO_PATH: Video ID: #{inspect(video_id)} =====")
    Logger.info("===== CACHE_VIDEO_PATH: Path: #{local_path} =====")

    ensure_cache_table()

    video_id_str = if is_list(video_id), do: List.to_string(video_id), else: video_id
    :ets.insert(:video_path_cache, {video_id_str, local_path, :erlang.system_time(:second)})

    Logger.info("===== CACHE_VIDEO_PATH: Cached successfully =====")
  rescue
    error ->
      Logger.error("===== CACHE_VIDEO_PATH: Exception: #{inspect(error)} =====")
  end

  defp ensure_cache_table do
    case :ets.whereis(:video_path_cache) do
      :undefined ->
        :ets.new(:video_path_cache, [:set, :public, :named_table, read_concurrency: true])

      _ ->
        :ok
    end
  rescue
    ArgumentError ->
      :ets.new(:video_path_cache, [:set, :public, :named_table, read_concurrency: true])
  end

  defp schedule_cleanup(file_path, delay_ms) do
    Task.start(fn ->
      Process.sleep(delay_ms)

      try do
        if File.exists?(file_path) do
          File.rm(file_path)
          Logger.info("===== CLEANUP: Deleted #{file_path} =====")
        end
      rescue
        _ -> :ok
      end
    end)
  end

  defp extract_video_id(result) do
    case result do
      {:ok, vid} -> vid
      vid when is_list(vid) -> vid
      vid when is_binary(vid) -> vid
      _ -> nil
    end
  end

  defp get_video_extension(entry) do
    case entry.client_type do
      "video/mp4" -> "mp4"
      "video/webm" -> "webm"
      "video/quicktime" -> "mov"
      "video/x-msvideo" -> "avi"
      "video/x-matroska" -> "mkv"
      _ -> Path.extname(entry.client_name) |> String.trim_leading(".")
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

  defp upload_thumbnail_to_ipfs(video_id, thumbnail_content) do
    try do
      case :ipfs_content.upload_binary(thumbnail_content) do
        thumbnail_cid when is_list(thumbnail_cid) or is_binary(thumbnail_cid) ->
          :videodb.update_video_thumbnail(video_id, thumbnail_cid)

        _ ->
          :ok
      end
    catch
      _, _ -> :ok
    end
  end
end
