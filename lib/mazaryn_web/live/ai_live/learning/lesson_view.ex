defmodule MazarynWeb.AiLive.Learning.LessonView do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(
        %{"path_id" => path_id, "module_id" => module_id, "lesson_id" => lesson_id},
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)

      case load_lesson_data(lesson_id, module_id, path_id, user_id) do
        {:ok, data} ->
          socket =
            socket
            |> assign(user: user)
            |> assign(user_id: user_id)
            |> assign(session_uuid: session_uuid)
            |> assign(path_id: path_id)
            |> assign(module_id: module_id)
            |> assign(lesson_id: lesson_id)
            |> assign(path: data.path)
            |> assign(module: data.module)
            |> assign(lesson: data.lesson)
            |> assign(all_modules: data.all_modules)
            |> assign(is_enrolled: data.is_enrolled)
            |> assign(can_access: data.can_access)
            |> assign(lesson_completed: data.lesson_completed)
            |> assign(next_lesson: data.next_lesson)
            |> assign(prev_lesson: data.prev_lesson)
            |> assign(show_sidebar: true)
            |> assign(notes: "")
            |> assign(playback_position: 0)

          {:ok, socket}

        {:error, reason} ->
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, get_error_message(reason))
           |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}")}
      end
    else
      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(
        %{"path_id" => path_id, "module_id" => module_id, "lesson_id" => lesson_id},
        %{"user_id" => user_id} = _session,
        socket
      ) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)

        case load_lesson_data(lesson_id, module_id, path_id, uid) do
          {:ok, data} ->
            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: uid)
              |> assign(path_id: path_id)
              |> assign(module_id: module_id)
              |> assign(lesson_id: lesson_id)
              |> assign(path: data.path)
              |> assign(module: data.module)
              |> assign(lesson: data.lesson)
              |> assign(all_modules: data.all_modules)
              |> assign(is_enrolled: data.is_enrolled)
              |> assign(can_access: data.can_access)
              |> assign(lesson_completed: data.lesson_completed)
              |> assign(next_lesson: data.next_lesson)
              |> assign(prev_lesson: data.prev_lesson)
              |> assign(show_sidebar: true)
              |> assign(notes: "")
              |> assign(playback_position: 0)

            {:ok, socket}

          {:error, reason} ->
            locale = socket.assigns[:locale] || "en"

            {:ok,
             socket
             |> put_flash(:error, get_error_message(reason))
             |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}")}
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("toggle_sidebar", _params, socket) do
    {:noreply, assign(socket, show_sidebar: !socket.assigns.show_sidebar)}
  end

  @impl true
  def handle_event("mark_complete", _params, socket) do
    user_id = socket.assigns.user_id
    lesson_id = socket.assigns.lesson_id

    case mark_lesson_complete(user_id, lesson_id, socket.assigns.path_id) do
      :ok ->
        {:noreply,
         socket
         |> assign(lesson_completed: true)
         |> put_flash(:info, "Lesson marked as complete!")}

      {:error, _reason} ->
        {:noreply, put_flash(socket, :error, "Failed to mark lesson as complete")}
    end
  end

  @impl true
  def handle_event("update_progress", %{"position" => position}, socket) do
    position_int = String.to_integer(position)
    {:noreply, assign(socket, playback_position: position_int)}
  end

  @impl true
  def handle_event("save_notes", %{"notes" => notes}, socket) do
    {:noreply, assign(socket, notes: notes)}
  end

  defp load_lesson_data(lesson_id, module_id, path_id, user_id) do
    try do
      with {:ok, lesson} <- get_lesson(lesson_id),
           {:ok, module} <- get_module(module_id),
           {:ok, path} <- get_path(path_id) do
        is_enrolled = check_enrollment(user_id, path_id)
        can_access = can_access_lesson?(user_id, lesson, is_enrolled)

        if can_access do
          all_modules = load_all_modules_with_lessons(path_id)
          lesson_completed = check_lesson_completion(user_id, lesson_id)
          {prev_lesson, next_lesson} = find_adjacent_lessons(all_modules, module_id, lesson_id)

          {:ok,
           %{
             path: path,
             module: module,
             lesson: lesson,
             all_modules: all_modules,
             is_enrolled: is_enrolled,
             can_access: can_access,
             lesson_completed: lesson_completed,
             next_lesson: next_lesson,
             prev_lesson: prev_lesson
           }}
        else
          {:error, :access_denied}
        end
      end
    rescue
      _ -> {:error, :exception}
    end
  end

  defp get_lesson(lesson_id) do
    try do
      case :learningdb.get_lesson(lesson_id) do
        {:error, :not_found} ->
          {:error, :lesson_not_found}

        lesson_tuple when is_tuple(lesson_tuple) ->
          lesson = %{
            id: elem(lesson_tuple, 1),
            module_id: elem(lesson_tuple, 2),
            title: elem(lesson_tuple, 3),
            lesson_number: elem(lesson_tuple, 4),
            description: elem(lesson_tuple, 5),
            content_type: elem(lesson_tuple, 6),
            content_data: elem(lesson_tuple, 7),
            duration_minutes: elem(lesson_tuple, 8),
            is_free: elem(lesson_tuple, 9),
            allow_preview: elem(lesson_tuple, 10)
          }

          {:ok, lesson}

        _ ->
          {:error, :invalid_lesson}
      end
    rescue
      _ -> {:error, :exception}
    end
  end

  defp get_module(module_id) do
    try do
      case :learningdb.get_module(module_id) do
        {:error, :not_found} ->
          {:error, :module_not_found}

        module_tuple when is_tuple(module_tuple) ->
          module = %{
            id: elem(module_tuple, 1),
            path_id: elem(module_tuple, 2),
            title: elem(module_tuple, 3),
            description: elem(module_tuple, 4),
            order: elem(module_tuple, 5)
          }

          {:ok, module}

        _ ->
          {:error, :invalid_module}
      end
    rescue
      _ -> {:error, :exception}
    end
  end

  defp get_path(path_id) do
    try do
      case :learningdb.get_learning_path(path_id) do
        {:error, :not_found} ->
          {:error, :path_not_found}

        path_tuple when is_tuple(path_tuple) ->
          case Mazaryn.Schema.Learning.erl_changeset(path_tuple) do
            changeset when is_map(changeset) ->
              {:ok, Ecto.Changeset.apply_changes(changeset)}

            _ ->
              {:error, :invalid_path}
          end

        _ ->
          {:error, :invalid_path}
      end
    rescue
      _ -> {:error, :exception}
    end
  end

  defp check_enrollment(user_id, path_id) do
    try do
      case :learningdb.get_user_progress(user_id, path_id) do
        %{enrollment: _enrollment} -> true
        _ -> false
      end
    rescue
      _ -> false
    end
  end

  defp can_access_lesson?(user_id, lesson, is_enrolled) do
    lesson.is_free or lesson.allow_preview or is_enrolled
  end

  defp load_all_modules_with_lessons(path_id) do
    try do
      case :learningdb.get_modules_by_path(path_id) do
        modules when is_list(modules) ->
          Enum.map(modules, fn mod ->
            module_id = elem(mod, 1)
            lessons = load_module_lessons(module_id)

            %{
              id: module_id,
              title: elem(mod, 3),
              order: elem(mod, 5),
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
              id: elem(lesson, 1),
              module_id: elem(lesson, 2),
              title: elem(lesson, 3),
              lesson_number: elem(lesson, 4),
              content_type: elem(lesson, 6),
              duration_minutes: elem(lesson, 8)
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

  defp check_lesson_completion(user_id, lesson_id) do
    try do
      case :learningdb.get_lesson_progress(user_id, lesson_id) do
        %{completed: true} -> true
        _ -> false
      end
    rescue
      _ -> false
    end
  end

  defp find_adjacent_lessons(all_modules, current_module_id, current_lesson_id) do
    all_lessons =
      Enum.flat_map(all_modules, fn module ->
        Enum.map(module.lessons, fn lesson ->
          {module.id, lesson.id, lesson}
        end)
      end)

    current_index =
      Enum.find_index(all_lessons, fn {_, lesson_id, _} -> lesson_id == current_lesson_id end)

    if current_index do
      prev_lesson =
        if current_index > 0 do
          {mod_id, _, lesson} = Enum.at(all_lessons, current_index - 1)
          %{lesson: lesson, module_id: mod_id}
        else
          nil
        end

      next_lesson =
        if current_index < length(all_lessons) - 1 do
          {mod_id, _, lesson} = Enum.at(all_lessons, current_index + 1)
          %{lesson: lesson, module_id: mod_id}
        else
          nil
        end

      {prev_lesson, next_lesson}
    else
      {nil, nil}
    end
  end

  defp mark_lesson_complete(user_id, lesson_id, path_id) do
    try do
      :learningdb.mark_lesson_completed(user_id, lesson_id, path_id)
      :ok
    rescue
      _ -> {:error, :failed}
    end
  end

  defp get_error_message(:lesson_not_found), do: "Lesson not found"
  defp get_error_message(:module_not_found), do: "Module not found"
  defp get_error_message(:path_not_found), do: "Course not found"

  defp get_error_message(:access_denied),
    do: "You don't have access to this lesson. Please enroll in the course."

  defp get_error_message(_), do: "An error occurred"

  def convert_youtube_url_to_embed(url) do
    cond do
      String.contains?(url, "youtube.com/watch?v=") ->
        video_id = url |> String.split("v=") |> Enum.at(1) |> String.split("&") |> Enum.at(0)
        "https://www.youtube.com/embed/#{video_id}"

      String.contains?(url, "youtu.be/") ->
        video_id =
          url |> String.split("youtu.be/") |> Enum.at(1) |> String.split("?") |> Enum.at(0)

        "https://www.youtube.com/embed/#{video_id}"

      true ->
        url
    end
  end
end
