defmodule MazarynWeb.AiLive.Learning.PathShow do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger
  @impl true
  def mount(%{"path_id" => path_id}, session, socket) do
    Logger.info("🚀 [PathShow.mount] Starting with path_id: #{inspect(path_id)}")

    if String.contains?(path_id, "/") or String.length(path_id) > 100 do
      Logger.warning("⚠️ [PathShow.mount] Invalid path_id detected - likely routing error")
      locale = Map.get(session, "locale", "en")
      {:ok, redirect(socket, to: "/#{locale}/ai/learning")}
    else
      do_mount(path_id, session, socket)
    end
  end

  def mount(_params, _session, socket) do
    locale = socket.assigns[:locale] || "en"
    {:ok, redirect(socket, to: "/#{locale}/ai/learning")}
  end

  defp do_mount(path_id, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Using session_uuid: #{inspect(session_uuid)}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)
      handle_path_load(path_id, user, user_id, session_uuid, socket)
    else
      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  defp do_mount(path_id, %{"user_id" => user_id}, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)
        handle_path_load(path_id, user, uid, nil, socket)

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  defp handle_path_load(path_id, user, user_id, session_uuid, socket) do
    case load_learning_path_data(path_id) do
      {:ok, path} ->
        description = fetch_description_from_ipfs(path.description)
        updated_path = Map.put(path, :description, description)

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(session_uuid: session_uuid)
          |> assign(path_id: path_id)
          |> assign(path: updated_path)
          |> assign(modules: load_path_modules(path_id))
          |> assign(is_enrolled: check_enrollment(user_id, path_id))
          |> assign(is_creator: path.creator_id == user_id)
          |> assign(enrollment_progress: get_enrollment_progress(user_id, path_id))
          |> assign(active_tab: :overview)
          |> assign(show_enroll_modal: false)

        {:ok, socket}

      {:error, _reason} ->
        locale = socket.assigns[:locale] || "en"

        {:ok,
         socket
         |> put_flash(:error, "Course not found")
         |> redirect(to: "/#{locale}/ai/learning")}
    end
  end

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    tab_atom = String.to_existing_atom(tab)
    {:noreply, assign(socket, active_tab: tab_atom)}
  end

  @impl true
  def handle_event("show_enroll_modal", _params, socket) do
    {:noreply, assign(socket, show_enroll_modal: true)}
  end

  @impl true
  def handle_event("close_enroll_modal", _params, socket) do
    {:noreply, assign(socket, show_enroll_modal: false)}
  end

  @impl true
  def handle_event("confirm_enroll", _params, socket) do
    user_id = socket.assigns.user_id
    path_id = socket.assigns.path_id

    case :learningdb.enroll_user(user_id, path_id) do
      enrollment_id when is_binary(enrollment_id) or is_list(enrollment_id) ->
        {:noreply,
         socket
         |> assign(is_enrolled: true)
         |> assign(enrollment_progress: 0.0)
         |> assign(show_enroll_modal: false)
         |> put_flash(:info, "Successfully enrolled! Start learning now.")}

      {:ok, _enrollment_id} ->
        {:noreply,
         socket
         |> assign(is_enrolled: true)
         |> assign(enrollment_progress: 0.0)
         |> assign(show_enroll_modal: false)
         |> put_flash(:info, "Successfully enrolled! Start learning now.")}

      {:error, _reason} ->
        {:noreply,
         socket
         |> assign(show_enroll_modal: false)
         |> put_flash(:error, "Failed to enroll in course")}

      _other ->
        {:noreply,
         socket
         |> assign(is_enrolled: true)
         |> assign(enrollment_progress: 0.0)
         |> assign(show_enroll_modal: false)
         |> put_flash(:info, "Successfully enrolled! Start learning now.")}
    end
  end

  @impl true
  def handle_event("unenroll", _params, socket) do
    user_id = socket.assigns.user_id
    path_id = socket.assigns.path_id

    case :learningdb.unenroll_user(user_id, path_id) do
      :ok ->
        {:noreply,
         socket
         |> assign(is_enrolled: false)
         |> assign(enrollment_progress: 0.0)
         |> put_flash(:info, "Successfully unenrolled from the course")}

      {:error, _reason} ->
        {:noreply, put_flash(socket, :error, "Failed to unenroll from course")}
    end
  end

  defp to_string_safe(value) when is_list(value), do: List.to_string(value)
  defp to_string_safe(value) when is_binary(value), do: value
  defp to_string_safe(nil), do: nil
  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(value), do: to_string(value)

  defp load_learning_path_data(path_id) do
    try do
      charlist_id = if is_binary(path_id), do: String.to_charlist(path_id), else: path_id
      result = :learningdb.get_learning_path(charlist_id)

      case result do
        {:error, reason} -> {:error, reason}
        path when is_tuple(path) and tuple_size(path) > 10 -> extract_path_data(path)
        other -> {:error, :not_found}
      end
    rescue
      _ -> {:error, :exception}
    end
  end

  defp extract_path_data(path) do
    raw_id = elem(path, 1)
    raw_title = elem(path, 2)
    raw_description = elem(path, 3)
    raw_creator_id = elem(path, 4)
    raw_creator_name = elem(path, 5)
    raw_creator_family = elem(path, 6)
    id = to_string_safe(raw_id)
    title = to_string_safe(raw_title)
    description = to_string_safe(raw_description)
    creator_id = to_string_safe(raw_creator_id)
    creator_name = to_string_safe(raw_creator_name)
    creator_family = to_string_safe(raw_creator_family)

    {:ok,
     %{
       id: id,
       title: title || "Untitled Course",
       description: description || "",
       creator_id: creator_id,
       creator_name: creator_name || "Unknown Instructor",
       creator_family: creator_family || "",
       approval_status: elem(path, 7) || :pending,
       difficulty_level: elem(path, 13) || :beginner,
       estimated_duration: elem(path, 14) || 0,
       tags: elem(path, 17) || [],
       category: elem(path, 23) || "uncategorized",
       enrollment_count: elem(path, 28) || 0,
       average_rating: (elem(path, 30) || 0.0) * 1.0,
       learning_outcomes: elem(path, 32) || [],
       price: (elem(path, 37) || 0.0) * 1.0,
       currency: to_string_safe(elem(path, 38)) || "USD",
       lifetime_access: elem(path, 42) || true
     }}
  end

  defp load_path_modules(path_id) do
    try do
      case :learningdb.get_modules_by_path(path_id) do
        modules when is_list(modules) ->
          Enum.map(modules, fn mod ->
            module_id = elem(mod, 1)
            lessons = load_module_lessons(module_id)

            %{
              id: to_string_safe(module_id),
              title: to_string_safe(elem(mod, 5)) || "Module",
              description: to_string_safe(elem(mod, 6)) || "",
              order: elem(mod, 7) || 1,
              estimated_hours: elem(mod, 8) || 0,
              is_optional: false,
              lesson_count: length(lessons),
              lessons: lessons
            }
          end)
          |> Enum.sort_by(& &1.order)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp load_module_lessons(module_id) do
    try do
      case :learningdb.get_lessons_by_module(module_id) do
        lessons when is_list(lessons) ->
          Enum.map(lessons, fn lesson ->
            %{
              id: to_string_safe(elem(lesson, 1)),
              title: to_string_safe(elem(lesson, 5)) || "Lesson",
              lesson_number: elem(lesson, 7) || 1,
              content_type: elem(lesson, 8) || :video,
              duration_minutes: elem(lesson, 10) || 0,
              is_free: false,
              allow_preview: false
            }
          end)
          |> Enum.sort_by(& &1.lesson_number)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp check_enrollment(user_id, path_id) do
    try do
      case :learningdb.get_user_progress(user_id, path_id) do
        %{enrollment: _enrollment} -> true
        {:error, :not_enrolled} -> false
        _ -> false
      end
    rescue
      _ -> false
    end
  end

  defp get_enrollment_progress(user_id, path_id) do
    try do
      case :learningdb.get_user_progress(user_id, path_id) do
        %{progress_percentage: progress} when is_number(progress) -> progress
        _ -> 0.0
      end
    rescue
      _ -> 0.0
    end
  end

  defp fetch_description_from_ipfs(description) when is_binary(description) do
    cond do
      String.starts_with?(description, "Qm") or String.starts_with?(description, "baf") ->
        try do
          cid_charlist = String.to_charlist(description)
          content = :ipfs_content.get_text_content(cid_charlist)
          if is_binary(content), do: content, else: to_string(content)
        rescue
          _ -> description
        end

      true ->
        description
    end
  end

  defp fetch_description_from_ipfs(description), do: description || ""
end
