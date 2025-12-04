defmodule MazarynWeb.MediaLive.Video.Upload do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    Logger.info("===== MOUNT: Starting upload mount =====")
    Logger.info("===== MOUNT: Session keys: #{inspect(Map.keys(session))} =====")

    user = get_user_from_session(session)
    Logger.info("===== MOUNT: User loaded: #{inspect(user)} =====")

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
        max_file_size: 16_000_000_000,
        auto_upload: false
      )
      |> allow_upload(:thumbnail,
        accept: ~w(image/jpeg image/png image/webp),
        max_entries: 1,
        max_file_size: 5_000_000,
        auto_upload: false
      )

    {:ok, socket}
  end

  @impl true
  def handle_event("validate_video", _params, socket) do
    Logger.info("===== VALIDATE_VIDEO: Event triggered =====")

    Logger.info(
      "===== VALIDATE_VIDEO: Video entries: #{inspect(socket.assigns.uploads.video.entries)} ====="
    )

    {:noreply, socket}
  end

  @impl true
  def handle_event("validate_thumbnail", _params, socket) do
    Logger.info("===== VALIDATE_THUMBNAIL: Event triggered =====")

    Logger.info(
      "===== VALIDATE_THUMBNAIL: Thumbnail entries: #{inspect(socket.assigns.uploads.thumbnail.entries)} ====="
    )

    {:noreply, socket}
  end

  @impl true
  def handle_event("cancel_video_upload", %{"ref" => ref}, socket) do
    Logger.info("===== CANCEL_VIDEO_UPLOAD: Canceling ref #{ref} =====")
    {:noreply, cancel_upload(socket, :video, ref)}
  end

  @impl true
  def handle_event("cancel_thumbnail_upload", %{"ref" => ref}, socket) do
    Logger.info("===== CANCEL_THUMBNAIL_UPLOAD: Canceling ref #{ref} =====")
    {:noreply, cancel_upload(socket, :thumbnail, ref)}
  end

  @impl true
  def handle_event("update_title", %{"value" => title}, socket) do
    Logger.info("===== UPDATE_TITLE: Title: '#{title}' =====")
    {:noreply, assign(socket, :title, title)}
  end

  @impl true
  def handle_event("update_description", %{"value" => description}, socket) do
    Logger.info(
      "===== UPDATE_DESCRIPTION: Description length: #{String.length(description)} ====="
    )

    {:noreply, assign(socket, :description, description)}
  end

  @impl true
  def handle_event("select_visibility", %{"visibility" => visibility}, socket) do
    Logger.info("===== SELECT_VISIBILITY: Visibility: #{visibility} =====")
    {:noreply, assign(socket, :visibility, visibility)}
  end

  @impl true
  def handle_event("add_tag", %{"tag" => tag}, socket) when tag != "" do
    Logger.info("===== ADD_TAG: Tag: #{tag} =====")

    if length(socket.assigns.tags) < 5 and tag not in socket.assigns.tags do
      {:noreply,
       socket
       |> assign(:tags, [tag | socket.assigns.tags])
       |> assign(:tag_input, "")}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("add_tag", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("remove_tag", %{"tag" => tag}, socket) do
    Logger.info("===== REMOVE_TAG: Tag: #{tag} =====")
    {:noreply, assign(socket, :tags, List.delete(socket.assigns.tags, tag))}
  end

  @impl true
  def handle_event("update_language", %{"value" => language}, socket) do
    Logger.info("===== UPDATE_LANGUAGE: Language: #{language} =====")
    {:noreply, assign(socket, :language, language)}
  end

  @impl true
  def handle_event("proceed_to_confirm", _params, socket) do
    Logger.info("===== PROCEED_TO_CONFIRM: Event triggered =====")
    Logger.info("===== PROCEED_TO_CONFIRM: Title: '#{socket.assigns.title}' =====")

    Logger.info(
      "===== PROCEED_TO_CONFIRM: Video entries: #{inspect(socket.assigns.uploads.video.entries)} ====="
    )

    title = String.trim(socket.assigns.title)
    has_file = length(socket.assigns.uploads.video.entries) > 0

    Logger.info("===== PROCEED_TO_CONFIRM: Title valid: #{title != ""} =====")
    Logger.info("===== PROCEED_TO_CONFIRM: Has file: #{has_file} =====")

    if title != "" && has_file do
      Logger.info("===== PROCEED_TO_CONFIRM: ✅ Proceeding to confirm step =====")
      {:noreply, assign(socket, :step, :confirm)}
    else
      Logger.warning("===== PROCEED_TO_CONFIRM: ❌ Validation failed =====")
      {:noreply, put_flash(socket, :error, "Please fill in the title and select a video file")}
    end
  end

  @impl true
  def handle_event("back_to_upload", _params, socket) do
    Logger.info("===== BACK_TO_UPLOAD: Returning to upload step =====")
    {:noreply, assign(socket, :step, :upload)}
  end

  @impl true
  def handle_event("confirm_upload", _params, socket) do
    Logger.info("===== CONFIRM_UPLOAD: Starting upload process =====")
    Logger.info("===== CONFIRM_UPLOAD: User: #{inspect(socket.assigns.user)} =====")

    user_id = get_user_id(socket.assigns.user)
    Logger.info("===== CONFIRM_UPLOAD: User ID: #{inspect(user_id)} =====")

    if is_nil(user_id) do
      Logger.error("===== CONFIRM_UPLOAD: No user ID found =====")
      {:noreply, put_flash(socket, :error, "User session expired. Please login again.")}
    else
      socket = assign(socket, uploading: true, upload_progress: 0)

      Logger.info("===== CONFIRM_UPLOAD: Starting to consume video entries =====")

      video_result =
        consume_uploaded_entries(socket, :video, fn %{path: path}, entry ->
          Logger.info("===== CONFIRM_UPLOAD: Processing video file: #{entry.client_name} =====")
          Logger.info("===== CONFIRM_UPLOAD: Video file path: #{path} =====")

          case File.read(path) do
            {:ok, video_content} ->
              Logger.info(
                "===== CONFIRM_UPLOAD: Video file size: #{byte_size(video_content)} bytes ====="
              )

              {:ok, video_content}

            {:error, reason} ->
              Logger.error(
                "===== CONFIRM_UPLOAD: Failed to read video file: #{inspect(reason)} ====="
              )

              {:error, reason}
          end
        end)

      Logger.info("===== CONFIRM_UPLOAD: Starting to consume thumbnail entries =====")

      thumbnail_result =
        consume_uploaded_entries(socket, :thumbnail, fn %{path: path}, entry ->
          Logger.info(
            "===== CONFIRM_UPLOAD: Processing thumbnail file: #{entry.client_name} ====="
          )

          Logger.info("===== CONFIRM_UPLOAD: Thumbnail file path: #{path} =====")

          case File.read(path) do
            {:ok, thumbnail_content} ->
              Logger.info(
                "===== CONFIRM_UPLOAD: Thumbnail file size: #{byte_size(thumbnail_content)} bytes ====="
              )

              {:ok, thumbnail_content}

            {:error, reason} ->
              Logger.error(
                "===== CONFIRM_UPLOAD: Failed to read thumbnail file: #{inspect(reason)} ====="
              )

              {:error, reason}
          end
        end)

      Logger.info(
        "===== CONFIRM_UPLOAD: Video result: #{inspect(length(video_result))} files ====="
      )

      Logger.info(
        "===== CONFIRM_UPLOAD: Thumbnail result: #{inspect(length(thumbnail_result))} files ====="
      )

      case video_result do
        [{:ok, video_content}] ->
          Logger.info("===== CONFIRM_UPLOAD: Video content loaded successfully =====")

          thumbnail_content =
            case thumbnail_result do
              [{:ok, content}] ->
                Logger.info("===== CONFIRM_UPLOAD: Thumbnail content loaded successfully =====")
                content

              _ ->
                Logger.info("===== CONFIRM_UPLOAD: No thumbnail provided =====")
                nil
            end

          title = socket.assigns.title
          description = socket.assigns.description
          visibility_atom = String.to_atom(socket.assigns.visibility)
          tags = socket.assigns.tags

          Logger.info("===== CONFIRM_UPLOAD: Calling videodb:create_video_with_rust =====")

          Logger.info(
            "===== CONFIRM_UPLOAD: Parameters - user_id: #{user_id}, title: #{title} ====="
          )

          result =
            :videodb.create_video_with_rust(
              user_id,
              title,
              description,
              video_content,
              0,
              visibility_atom,
              tags,
              true,
              true,
              false
            )

          Logger.info("===== CONFIRM_UPLOAD: videodb result: #{inspect(result)} =====")

          case result do
            {:ok, video_id} ->
              Logger.info("===== CONFIRM_UPLOAD: Video created with ID: #{video_id} =====")

              if thumbnail_content do
                spawn(fn ->
                  upload_thumbnail_to_ipfs(video_id, thumbnail_content)
                end)
              end

              {:noreply,
               socket
               |> assign(uploading: false, upload_progress: 100, step: :success)
               |> put_flash(:info, "Video uploaded successfully!")}

            {:error, reason} ->
              Logger.error("===== CONFIRM_UPLOAD: Upload failed: #{inspect(reason)} =====")

              {:noreply,
               socket
               |> assign(uploading: false)
               |> put_flash(:error, "Upload failed: #{inspect(reason)}")}
          end

        [] ->
          Logger.error("===== CONFIRM_UPLOAD: No video file found =====")

          {:noreply,
           socket
           |> assign(uploading: false)
           |> put_flash(:error, "No video file selected")}

        error ->
          Logger.error("===== CONFIRM_UPLOAD: Upload error: #{inspect(error)} =====")

          {:noreply,
           socket
           |> assign(uploading: false)
           |> put_flash(:error, "Failed to process video file")}
      end
    end
  end

  @impl true
  def handle_event("cancel_upload", _params, socket) do
    Logger.info("===== CANCEL_UPLOAD: Canceling upload =====")

    {:noreply,
     socket
     |> assign(:step, :upload)
     |> assign(:uploading, false)
     |> assign(:upload_progress, 0)}
  end

  @impl true
  def handle_event("view_my_videos", _params, socket) do
    Logger.info("===== VIEW_MY_VIDEOS: Redirecting to my videos =====")
    {:noreply, push_navigate(socket, to: "/#{socket.assigns.locale}/videos/my-videos")}
  end

  defp get_user_from_session(%{"session_uuid" => session_uuid}) when session_uuid != nil do
    Logger.info("===== GET_USER_FROM_SESSION: Using session_uuid: #{session_uuid} =====")

    case Account.Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        Logger.info("===== GET_USER_FROM_SESSION: User found via session_uuid =====")

        case user do
          %{id: id} when not is_nil(id) ->
            Logger.info("===== GET_USER_FROM_SESSION: User ID from struct: #{id} =====")
            user

          _ ->
            Logger.error("===== GET_USER_FROM_SESSION: Invalid user struct =====")
            nil
        end

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

  defp get_user_id(%{id: id}) when not is_nil(id) do
    Logger.info("===== GET_USER_ID: Extracting from Ecto struct =====")
    to_string(id)
  end

  defp get_user_id(user_tuple) when is_tuple(user_tuple) do
    Logger.info("===== GET_USER_ID: Extracting from tuple =====")
    user_id = elem(user_tuple, 1)

    if is_list(user_id) do
      to_string(user_id)
    else
      user_id
    end
  end

  defp get_user_id(user) do
    Logger.error("===== GET_USER_ID: Invalid user format: #{inspect(user)} =====")
    nil
  end

  defp can_proceed?(title, video_entries) do
    title_valid = title != "" && String.trim(title) != ""
    file_valid = length(video_entries) > 0
    title_valid && file_valid
  end

  defp upload_thumbnail_to_ipfs(video_id, thumbnail_content) do
    Logger.info("===== UPLOAD_THUMBNAIL: Starting thumbnail upload for video #{video_id} =====")

    try do
      case :ipfs_content.upload_binary(thumbnail_content) do
        thumbnail_cid when is_list(thumbnail_cid) or is_binary(thumbnail_cid) ->
          Logger.info(
            "===== UPLOAD_THUMBNAIL: Thumbnail uploaded with CID: #{inspect(thumbnail_cid)} ====="
          )

          case :videodb.update_video_thumbnail(video_id, thumbnail_cid) do
            :ok ->
              Logger.info("===== UPLOAD_THUMBNAIL: Video thumbnail updated successfully =====")

            {:error, reason} ->
              Logger.error(
                "===== UPLOAD_THUMBNAIL: Failed to update video thumbnail: #{inspect(reason)} ====="
              )
          end

        {:error, reason} ->
          Logger.error(
            "===== UPLOAD_THUMBNAIL: Failed to upload thumbnail: #{inspect(reason)} ====="
          )
      end
    catch
      error_type, error ->
        Logger.error("===== UPLOAD_THUMBNAIL: Exception: #{error_type} - #{inspect(error)} =====")
    end
  end
end
