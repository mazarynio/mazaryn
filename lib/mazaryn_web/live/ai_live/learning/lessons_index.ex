defmodule MazarynWeb.AiLive.Learning.LessonsIndex do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(
        %{"path_id" => path_id, "module_id" => module_id},
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    Logger.info("🚀 [LessonsIndex.mount] Starting")
    Logger.info("   Path ID: #{inspect(path_id)}")
    Logger.info("   Module ID: #{inspect(module_id)}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)
      Logger.info("✅ [LessonsIndex.mount] User found: #{user_id}")

      if is_verified_instructor?(user_id) do
        case load_module_and_path(module_id, path_id, user_id) do
          {:ok, module, path} ->
            Logger.info("✅ [LessonsIndex.mount] Module: #{inspect(module.title)}")
            Logger.info("✅ [LessonsIndex.mount] Path: #{inspect(path.title)}")

            lessons = load_lessons(module_id)
            Logger.info("✅ [LessonsIndex.mount] Loaded #{length(lessons)} lessons")

            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(session_uuid: session_uuid)
              |> assign(path_id: path_id)
              |> assign(module_id: module_id)
              |> assign(path: path)
              |> assign(module: module)
              |> assign(lessons: lessons)
              |> assign(is_creator: true)

            {:ok, socket}

          {:error, reason} ->
            Logger.error("❌ [LessonsIndex.mount] Failed: #{inspect(reason)}")
            locale = socket.assigns[:locale] || "en"

            {:ok,
             socket
             |> put_flash(:error, "Module not found or access denied")
             |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}/modules")}
        end
      else
        Logger.error("❌ [LessonsIndex.mount] User is not verified instructor")
        locale = socket.assigns[:locale] || "en"

        {:ok,
         socket
         |> put_flash(:error, "You must be a verified instructor")
         |> redirect(to: "/#{locale}/ai/learning")}
      end
    else
      {:error, reason} ->
        Logger.error("❌ [LessonsIndex.mount] Auth failed: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(
        %{"path_id" => path_id, "module_id" => module_id},
        %{"user_id" => user_id} = _session,
        socket
      ) do
    Logger.info("🚀 [LessonsIndex.mount] Starting with user_id")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)

        if is_verified_instructor?(uid) do
          case load_module_and_path(module_id, path_id, uid) do
            {:ok, module, path} ->
              socket =
                socket
                |> assign(user: user)
                |> assign(user_id: uid)
                |> assign(path_id: path_id)
                |> assign(module_id: module_id)
                |> assign(path: path)
                |> assign(module: module)
                |> assign(lessons: load_lessons(module_id))
                |> assign(is_creator: true)

              {:ok, socket}

            {:error, _reason} ->
              locale = socket.assigns[:locale] || "en"

              {:ok,
               socket
               |> put_flash(:error, "Module not found or access denied")
               |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}/modules")}
          end
        else
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, "You must be a verified instructor")
           |> redirect(to: "/#{locale}/ai/learning")}
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("delete_lesson", %{"id" => lesson_id}, socket) do
    Logger.info("🗑️  [LessonsIndex] Deleting lesson: #{lesson_id}")

    charlist_id = if is_binary(lesson_id), do: String.to_charlist(lesson_id), else: lesson_id
    user_id = socket.assigns.user_id
    charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

    case :learningdb.delete_lesson(charlist_id, charlist_user_id) do
      :ok ->
        Logger.info("✅ [LessonsIndex] Lesson deleted")

        {:noreply,
         socket
         |> assign(lessons: load_lessons(socket.assigns.module_id))
         |> put_flash(:info, "Lesson deleted successfully")}

      {:error, reason} ->
        Logger.error("❌ [LessonsIndex] Delete failed: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to delete lesson")}
    end
  end

  @impl true
  def handle_event("reorder_lesson", %{"id" => lesson_id, "direction" => direction}, socket) do
    Logger.info("📋 [LessonsIndex] Reordering lesson #{lesson_id} #{direction}")

    lessons = socket.assigns.lessons
    lesson_index = Enum.find_index(lessons, &(&1.id == lesson_id))

    if lesson_index do
      new_index =
        case direction do
          "up" -> max(0, lesson_index - 1)
          "down" -> min(length(lessons) - 1, lesson_index + 1)
        end

      if new_index != lesson_index do
        reordered_lessons = swap_lessons(lessons, lesson_index, new_index)
        update_lesson_orders(reordered_lessons)

        {:noreply,
         socket
         |> assign(lessons: load_lessons(socket.assigns.module_id))
         |> put_flash(:info, "Lesson order updated")}
      else
        {:noreply, socket}
      end
    else
      {:noreply, socket}
    end
  end

  defp to_string_safe(value) when is_list(value), do: List.to_string(value)
  defp to_string_safe(value) when is_binary(value), do: value
  defp to_string_safe(nil), do: nil
  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(value), do: to_string(value)

  defp is_verified_instructor?(user_id) do
    try do
      charlist_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

      case :instructordb.is_verified_instructor(charlist_id) do
        true -> true
        _ -> false
      end
    rescue
      _ -> false
    end
  end

  defp load_module_and_path(module_id, path_id, user_id) do
    try do
      Logger.info("🔍 [load_module_and_path] Loading module and path")
      Logger.info("   Module ID: #{inspect(module_id)}")
      Logger.info("   Path ID: #{inspect(path_id)}")

      charlist_module_id =
        if is_binary(module_id), do: String.to_charlist(module_id), else: module_id

      charlist_path_id = if is_binary(path_id), do: String.to_charlist(path_id), else: path_id

      case :learningdb.get_module(charlist_module_id) do
        {:error, :not_found} ->
          Logger.error("❌ [load_module_and_path] Module not found")
          {:error, :not_found}

        module_tuple when is_tuple(module_tuple) ->
          Logger.info("✅ [load_module_and_path] Module tuple size: #{tuple_size(module_tuple)}")
          Logger.info("   Full module tuple: #{inspect(module_tuple)}")

          module = parse_module(module_tuple)
          Logger.info("✅ [load_module_and_path] Parsed module: #{inspect(module)}")

          case :learningdb.get_learning_path(charlist_path_id) do
            {:error, :not_found} ->
              Logger.error("❌ [load_module_and_path] Path not found")
              {:error, :not_found}

            path_tuple when is_tuple(path_tuple) ->
              Logger.info("✅ [load_module_and_path] Path found")

              path = %{
                id: to_string_safe(elem(path_tuple, 1)),
                title: to_string_safe(elem(path_tuple, 2)) || "Untitled Course",
                creator_id: to_string_safe(elem(path_tuple, 4))
              }

              if path.creator_id == user_id do
                {:ok, module, path}
              else
                Logger.error("❌ [load_module_and_path] User is not creator")
                {:error, :unauthorized}
              end

            other ->
              Logger.error("❌ [load_module_and_path] Unexpected path: #{inspect(other)}")
              {:error, :invalid_data}
          end

        other ->
          Logger.error("❌ [load_module_and_path] Unexpected module: #{inspect(other)}")
          {:error, :invalid_data}
      end
    rescue
      error ->
        Logger.error("❌ [load_module_and_path] Exception: #{inspect(error)}")
        Logger.error("   Stacktrace: #{inspect(__STACKTRACE__)}")
        {:error, :exception}
    end
  end

  defp parse_module(module_tuple) do
    Logger.info("🔧 [parse_module] Parsing module tuple")
    Logger.info("   Tuple size: #{tuple_size(module_tuple)}")

    %{
      id: to_string_safe(elem(module_tuple, 1)),
      path_id: to_string_safe(elem(module_tuple, 2)),
      learning_path_id: to_string_safe(elem(module_tuple, 3)),
      creator_id: to_string_safe(elem(module_tuple, 4)),
      creator_name: to_string_safe(elem(module_tuple, 5)),
      creator_family: to_string_safe(elem(module_tuple, 6)),
      title: to_string_safe(elem(module_tuple, 7)) || "Untitled Module",
      description: to_string_safe(elem(module_tuple, 8)) || "",
      module_number: elem(module_tuple, 9) || 1,
      estimated_hours: elem(module_tuple, 10) || 0
    }
  end

  defp load_lessons(module_id) do
    try do
      Logger.info("🔍 [load_lessons] Loading lessons for module: #{inspect(module_id)}")

      charlist_id = if is_binary(module_id), do: String.to_charlist(module_id), else: module_id

      case :learningdb.get_lessons_by_module(charlist_id) do
        lessons when is_list(lessons) ->
          Logger.info("✅ [load_lessons] Found #{length(lessons)} lessons")

          if length(lessons) > 0 do
            Logger.info("   First lesson tuple: #{inspect(Enum.at(lessons, 0))}")
          end

          Enum.map(lessons, fn lesson ->
            Logger.info("🔧 [load_lessons] Processing lesson tuple size: #{tuple_size(lesson)}")

            %{
              id: to_string_safe(elem(lesson, 1)),
              module_id: to_string_safe(elem(lesson, 2)),
              path_id: to_string_safe(elem(lesson, 3)),
              creator_id: to_string_safe(elem(lesson, 4)),
              creator_name: to_string_safe(elem(lesson, 5)),
              creator_family: to_string_safe(elem(lesson, 6)),
              title: to_string_safe(elem(lesson, 7)) || "Untitled Lesson",
              description: to_string_safe(elem(lesson, 8)) || "",
              lesson_number: elem(lesson, 9) || 1,
              content_type: elem(lesson, 10) || :text,
              content_cid: to_string_safe(elem(lesson, 11)),
              duration_minutes: elem(lesson, 12) || 0,
              is_free: elem(lesson, 25) || false,
              allow_preview: elem(lesson, 36) || false
            }
          end)
          |> Enum.sort_by(& &1.lesson_number)

        other ->
          Logger.error("❌ [load_lessons] Unexpected: #{inspect(other)}")
          []
      end
    rescue
      error ->
        Logger.error("❌ [load_lessons] Exception: #{inspect(error)}")
        Logger.error("   Stacktrace: #{inspect(__STACKTRACE__)}")
        []
    end
  end

  defp swap_lessons(lessons, index1, index2) do
    lesson1 = Enum.at(lessons, index1)
    lesson2 = Enum.at(lessons, index2)

    lessons
    |> List.replace_at(index1, %{lesson1 | lesson_number: index2 + 1})
    |> List.replace_at(index2, %{lesson2 | lesson_number: index1 + 1})
  end

  defp update_lesson_orders(lessons) do
    Enum.each(lessons, fn lesson ->
      try do
        charlist_id = if is_binary(lesson.id), do: String.to_charlist(lesson.id), else: lesson.id

        charlist_creator_id =
          if is_binary(lesson.creator_id),
            do: String.to_charlist(lesson.creator_id),
            else: lesson.creator_id

        charlist_creator_name =
          if is_binary(lesson.creator_name),
            do: String.to_charlist(lesson.creator_name),
            else: 'Unknown'

        charlist_creator_family =
          if is_binary(lesson.creator_family),
            do: String.to_charlist(lesson.creator_family),
            else: 'Unknown'

        charlist_title =
          if is_binary(lesson.title), do: String.to_charlist(lesson.title), else: 'Untitled'

        charlist_description =
          if is_binary(lesson.description), do: String.to_charlist(lesson.description), else: ''

        :learningdb.update_lesson(
          charlist_id,
          charlist_creator_id,
          charlist_creator_name,
          charlist_creator_family,
          charlist_title,
          charlist_description,
          lesson.lesson_number,
          lesson.content_type,
          lesson.duration_minutes,
          [],
          [],
          :undefined,
          :undefined,
          :undefined,
          :undefined,
          :undefined,
          :undefined,
          :undefined,
          :undefined
        )
      rescue
        error ->
          Logger.error("❌ Failed to update lesson order: #{inspect(error)}")
          :ok
      end
    end)
  end
end
