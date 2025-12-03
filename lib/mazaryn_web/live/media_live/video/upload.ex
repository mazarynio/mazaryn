defmodule MazarynWeb.MediaLive.Video.Upload do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Upload Video")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:step, :upload)
     |> assign(:title, "")
     |> assign(:description, "")
     |> assign(:url_slug, "")
     |> assign(:thumbnail_method, "upload")
     |> assign(:thumbnail_url, "")
     |> assign(:visibility, "public")
     |> assign(:language, "en")
     |> assign(:tags, [])
     |> assign(:tag_input, "")
     |> assign(:uploading, false)
     |> assign(:upload_progress, 0)
     |> assign(:selected_file, nil)
     |> allow_upload(:video,
       accept: ~w(.mp4 .webm .mov .avi .mkv),
       max_entries: 1,
       max_file_size: 16_000_000_000,
       auto_upload: false
     )
     |> allow_upload(:thumbnail,
       accept: ~w(.jpg .jpeg .png .webp),
       max_entries: 1,
       max_file_size: 5_000_000,
       auto_upload: false
     )}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("validate_video", _params, socket) do
    selected_file =
      case socket.assigns.uploads.video.entries do
        [entry | _] -> entry.client_name
        [] -> nil
      end

    {:noreply, assign(socket, :selected_file, selected_file)}
  end

  @impl true
  def handle_event("validate_thumbnail", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("update_title", %{"value" => title}, socket) do
    {:noreply,
     socket
     |> assign(:title, title)
     |> assign(:url_slug, generate_url_slug(title))}
  end

  @impl true
  def handle_event("update_description", %{"value" => description}, socket) do
    {:noreply, assign(socket, :description, description)}
  end

  @impl true
  def handle_event("update_url", %{"value" => url_slug}, socket) do
    {:noreply, assign(socket, :url_slug, url_slug)}
  end

  @impl true
  def handle_event("update_language", %{"value" => language}, socket) do
    {:noreply, assign(socket, :language, language)}
  end

  @impl true
  def handle_event("select_visibility", %{"visibility" => visibility}, socket) do
    {:noreply, assign(socket, :visibility, visibility)}
  end

  @impl true
  def handle_event("add_tag", %{"tag" => tag}, socket) when tag != "" do
    tags = socket.assigns.tags

    cond do
      length(tags) >= 5 ->
        {:noreply, put_flash(socket, :error, "Maximum 5 tags allowed")}

      tag in tags ->
        {:noreply, put_flash(socket, :error, "Tag already added")}

      true ->
        {:noreply,
         socket
         |> assign(:tags, tags ++ [tag])
         |> assign(:tag_input, "")}
    end
  end

  @impl true
  def handle_event("add_tag", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("remove_tag", %{"tag" => tag}, socket) do
    tags = Enum.reject(socket.assigns.tags, &(&1 == tag))
    {:noreply, assign(socket, :tags, tags)}
  end

  @impl true
  def handle_event("select_thumbnail_method", %{"method" => method}, socket) do
    {:noreply, assign(socket, :thumbnail_method, method)}
  end

  @impl true
  def handle_event("update_thumbnail_url", %{"value" => url}, socket) do
    {:noreply, assign(socket, :thumbnail_url, url)}
  end

  @impl true
  def handle_event("proceed_to_confirm", _params, socket) do
    if valid_for_confirmation?(socket) do
      {:noreply, assign(socket, :step, :confirm)}
    else
      {:noreply, put_flash(socket, :error, "Please fill in all required fields")}
    end
  end

  @impl true
  def handle_event("back_to_upload", _params, socket) do
    {:noreply, assign(socket, :step, :upload)}
  end

  @impl true
  def handle_event("confirm_upload", _params, socket) do
    socket = assign(socket, :uploading, true)

    socket =
      consume_uploaded_entries(socket, :video, fn %{path: path}, _entry ->
        video_content = File.read!(path)

        case :videodb.create_video_with_rust(
               socket.assigns.user.id,
               socket.assigns.title,
               socket.assigns.description,
               video_content,
               0,
               String.to_atom(socket.assigns.visibility),
               socket.assigns.tags,
               true,
               true,
               false
             ) do
          {:ok, video_id} ->
            send(self(), {:upload_complete, video_id})
            {:ok, video_id}

          {:error, reason} ->
            send(self(), {:upload_failed, reason})
            {:postpone, :error}
        end
      end)

    {:noreply, socket}
  end

  @impl true
  def handle_event("cancel_upload", _params, socket) do
    {:noreply,
     socket
     |> assign(:step, :upload)
     |> assign(:uploading, false)
     |> assign(:upload_progress, 0)}
  end

  @impl true
  def handle_event("view_my_videos", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/my-videos")}
  end

  @impl true
  def handle_info({:upload_complete, _video_id}, socket) do
    {:noreply,
     socket
     |> assign(:step, :success)
     |> assign(:uploading, false)
     |> assign(:upload_progress, 100)}
  end

  @impl true
  def handle_info({:upload_failed, reason}, socket) do
    Logger.error("Video upload failed: #{inspect(reason)}")

    {:noreply,
     socket
     |> assign(:uploading, false)
     |> assign(:upload_progress, 0)
     |> put_flash(:error, "Upload failed. Please try again.")}
  end

  defp get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(%{"session_uuid" => _session_uuid, "user_id" => user_id})
       when user_id != nil do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(_), do: nil

  defp generate_url_slug(title) do
    title
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9\s-]/, "")
    |> String.replace(~r/\s+/, "-")
    |> String.trim("-")
  end

  defp valid_for_confirmation?(socket) do
    socket.assigns.title != "" && socket.assigns.selected_file != nil
  end
end
