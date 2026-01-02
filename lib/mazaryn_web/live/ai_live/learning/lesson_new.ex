defmodule MazarynWeb.AiLive.Learning.LessonNew do
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
    Logger.info("🚀 [LessonNew.mount] Starting")
    Logger.info("   Path ID: #{inspect(path_id)}")
    Logger.info("   Module ID: #{inspect(module_id)}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)
      Logger.info("✅ [LessonNew.mount] User found: #{user_id}")

      if is_verified_instructor?(user_id) do
        Logger.info("✅ [LessonNew.mount] User is verified instructor")

        case load_module_and_path(module_id, path_id, user_id) do
          {:ok, module, path} ->
            Logger.info("✅ [LessonNew.mount] Module and path loaded successfully")
            Logger.info("   Module: #{inspect(module.title)}")
            Logger.info("   Path: #{inspect(path.title)}")

            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(session_uuid: session_uuid)
              |> assign(path_id: path_id)
              |> assign(module_id: module_id)
              |> assign(path: path)
              |> assign(module: module)
              |> assign_form_fields()
              |> assign(current_step: 1)
              |> assign(total_steps: 3)
              |> assign(errors: %{})
              |> assign(submitting: false)
              |> assign(uploading_video: false)
              |> assign(upload_progress: 0)

            Logger.info("✅ [LessonNew.mount] Mount successful")
            {:ok, socket}

          {:error, reason} ->
            Logger.error("❌ [LessonNew.mount] Failed to load module/path: #{inspect(reason)}")
            locale = socket.assigns[:locale] || "en"

            {:ok,
             socket
             |> put_flash(:error, "Module not found or access denied")
             |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}/modules")}
        end
      else
        Logger.error("❌ [LessonNew.mount] User is not verified instructor")
        locale = socket.assigns[:locale] || "en"

        {:ok,
         socket
         |> put_flash(:error, "You must be a verified instructor")
         |> redirect(to: "/#{locale}/ai/learning")}
      end
    else
      {:error, reason} ->
        Logger.error("❌ [LessonNew.mount] Failed to get user: #{inspect(reason)}")

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
    Logger.info("🚀 [LessonNew.mount] Starting with user_id")

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
                |> assign_form_fields()
                |> assign(current_step: 1)
                |> assign(total_steps: 3)
                |> assign(errors: %{})
                |> assign(submitting: false)
                |> assign(uploading_video: false)
                |> assign(upload_progress: 0)

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
  def handle_event("next_step", _params, socket) do
    Logger.info("➡️  [LessonNew] Moving to next step from #{socket.assigns.current_step}")
    current_step = socket.assigns.current_step
    errors = validate_step(current_step, socket.assigns)

    if map_size(errors) == 0 do
      Logger.info("✅ [LessonNew] Validation passed, moving to step #{current_step + 1}")

      {:noreply,
       assign(socket,
         current_step: min(current_step + 1, socket.assigns.total_steps),
         errors: %{}
       )}
    else
      Logger.error("❌ [LessonNew] Validation failed: #{inspect(errors)}")
      {:noreply, assign(socket, errors: errors)}
    end
  end

  @impl true
  def handle_event("prev_step", _params, socket) do
    Logger.info("⬅️  [LessonNew] Moving to previous step from #{socket.assigns.current_step}")
    {:noreply, assign(socket, current_step: max(socket.assigns.current_step - 1, 1), errors: %{})}
  end

  @impl true
  def handle_event("validate", params, socket) do
    Logger.info("📝 [LessonNew] Validating form: #{inspect(Map.keys(params))}")
    socket = update_form_fields(socket, params)
    {:noreply, socket}
  end

  @impl true
  def handle_event("submit", params, socket) do
    Logger.info("💾 [LessonNew] Submitting lesson creation")
    Logger.info("   Params: #{inspect(params)}")

    socket = update_form_fields(socket, params)
    errors = validate_all_steps(socket.assigns)

    if map_size(errors) == 0 do
      Logger.info("✅ [LessonNew] All validation passed, creating lesson")
      socket = assign(socket, submitting: true)

      case create_lesson(socket.assigns) do
        {:ok, lesson_id} ->
          Logger.info("✅ [LessonNew] Lesson created successfully: #{lesson_id}")
          locale = socket.assigns[:locale] || "en"

          {:noreply,
           socket
           |> put_flash(:info, "Lesson created successfully!")
           |> redirect(
             to:
               "/#{locale}/ai/learning/paths/#{socket.assigns.path_id}/modules/#{socket.assigns.module_id}/lessons"
           )}

        {:error, reason} ->
          Logger.error("❌ [LessonNew] Failed to create lesson: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(submitting: false)
           |> put_flash(:error, "Failed to create lesson: #{inspect(reason)}")}
      end
    else
      Logger.error("❌ [LessonNew] Validation failed: #{inspect(errors)}")
      first_error_step = get_first_error_step(errors)
      {:noreply, assign(socket, errors: errors, current_step: first_error_step)}
    end
  end

  defp assign_form_fields(socket) do
    Logger.info("📋 [assign_form_fields] Setting up form fields")
    existing_lessons = load_existing_lessons(socket.assigns.module_id)
    next_order = length(existing_lessons) + 1
    Logger.info("   Next lesson order: #{next_order}")

    socket
    |> assign(title: "")
    |> assign(description: "")
    |> assign(content_type: "video")
    |> assign(video_duration: "")
    |> assign(video_url: "")
    |> assign(text_content: "")
    |> assign(allow_preview: false)
    |> assign(is_free: false)
    |> assign(lesson_order: next_order)
    |> assign(existing_lessons: existing_lessons)
    |> assign(resources: [])
  end

  defp to_string_safe(value) when is_list(value), do: List.to_string(value)
  defp to_string_safe(value) when is_binary(value), do: value
  defp to_string_safe(nil), do: nil
  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(value), do: to_string(value)

  defp is_verified_instructor?(user_id) do
    try do
      Logger.info("🔍 [is_verified_instructor?] Checking: #{inspect(user_id)}")
      charlist_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

      case :instructordb.is_verified_instructor(charlist_id) do
        true ->
          Logger.info("✅ [is_verified_instructor?] User is verified")
          true

        _ ->
          Logger.warn("⚠️  [is_verified_instructor?] User is not verified")
          false
      end
    rescue
      error ->
        Logger.error("❌ [is_verified_instructor?] Exception: #{inspect(error)}")
        false
    end
  end

  defp load_module_and_path(module_id, path_id, user_id) do
    try do
      Logger.info("🔍 [load_module_and_path] Loading module and path")
      Logger.info("   Module ID: #{inspect(module_id)}")
      Logger.info("   Path ID: #{inspect(path_id)}")
      Logger.info("   User ID: #{inspect(user_id)}")

      charlist_module_id =
        if is_binary(module_id), do: String.to_charlist(module_id), else: module_id

      charlist_path_id = if is_binary(path_id), do: String.to_charlist(path_id), else: path_id

      case :learningdb.get_module(charlist_module_id) do
        {:error, :not_found} ->
          Logger.error("❌ [load_module_and_path] Module not found")
          {:error, :not_found}

        module_tuple when is_tuple(module_tuple) ->
          Logger.info("✅ [load_module_and_path] Module found")
          Logger.info("   Module tuple size: #{tuple_size(module_tuple)}")
          Logger.info("   Module tuple: #{inspect(module_tuple)}")

          module = parse_module(module_tuple)
          Logger.info("   Parsed module: #{inspect(module)}")

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

              Logger.info("   Parsed path: #{inspect(path)}")

              if path.creator_id == user_id do
                Logger.info("✅ [load_module_and_path] User is creator, access granted")
                {:ok, module, path}
              else
                Logger.error("❌ [load_module_and_path] User is not creator")
                Logger.error("   Path creator: #{path.creator_id}")
                Logger.error("   Current user: #{user_id}")
                {:error, :unauthorized}
              end

            other ->
              Logger.error("❌ [load_module_and_path] Unexpected path result: #{inspect(other)}")
              {:error, :invalid_data}
          end

        other ->
          Logger.error("❌ [load_module_and_path] Unexpected module result: #{inspect(other)}")
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

  defp load_existing_lessons(module_id) do
    try do
      Logger.info("🔍 [load_existing_lessons] Loading lessons for module: #{inspect(module_id)}")
      charlist_id = if is_binary(module_id), do: String.to_charlist(module_id), else: module_id

      case :learningdb.get_lessons_by_module(charlist_id) do
        lessons when is_list(lessons) ->
          Logger.info("✅ [load_existing_lessons] Found #{length(lessons)} lessons")

          Enum.map(lessons, fn lesson ->
            %{
              id: to_string_safe(elem(lesson, 1)),
              title: to_string_safe(elem(lesson, 7)),
              order: elem(lesson, 9)
            }
          end)
          |> Enum.sort_by(& &1.order)

        other ->
          Logger.error("❌ [load_existing_lessons] Unexpected: #{inspect(other)}")
          []
      end
    rescue
      error ->
        Logger.error("❌ [load_existing_lessons] Exception: #{inspect(error)}")
        []
    end
  end

  defp validate_step(1, assigns) do
    Logger.info("✔️  [validate_step] Validating step 1")
    errors = %{}

    errors =
      if String.trim(assigns.title) == "",
        do: Map.put(errors, :title, "Lesson title is required"),
        else: errors

    errors =
      if String.length(assigns.description) < 30,
        do: Map.put(errors, :description, "Description must be at least 30 characters"),
        else: errors

    Logger.info("   Step 1 errors: #{inspect(errors)}")
    errors
  end

  defp validate_step(2, assigns) do
    Logger.info("✔️  [validate_step] Validating step 2")
    errors = %{}

    errors =
      case assigns.content_type do
        "video" ->
          if String.trim(assigns.video_duration) == "",
            do: Map.put(errors, :video_duration, "Video duration is required"),
            else: errors

        "text" ->
          if String.length(assigns.text_content) < 100,
            do: Map.put(errors, :text_content, "Text content must be at least 100 characters"),
            else: errors

        _ ->
          errors
      end

    Logger.info("   Step 2 errors: #{inspect(errors)}")
    errors
  end

  defp validate_step(3, _assigns) do
    Logger.info("✔️  [validate_step] Validating step 3")
    %{}
  end

  defp validate_all_steps(assigns) do
    %{}
    |> Map.merge(validate_step(1, assigns))
    |> Map.merge(validate_step(2, assigns))
    |> Map.merge(validate_step(3, assigns))
  end

  defp get_first_error_step(errors) do
    cond do
      Map.has_key?(errors, :title) or Map.has_key?(errors, :description) -> 1
      Map.has_key?(errors, :video_duration) or Map.has_key?(errors, :text_content) -> 2
      true -> 3
    end
  end

  defp update_form_fields(socket, params) do
    Enum.reduce(params, socket, fn {key, value}, acc ->
      case key do
        "title" -> assign(acc, title: value)
        "description" -> assign(acc, description: value)
        "content_type" -> assign(acc, content_type: value)
        "video_duration" -> assign(acc, video_duration: value)
        "video_url" -> assign(acc, video_url: value)
        "text_content" -> assign(acc, text_content: value)
        "allow_preview" -> assign(acc, allow_preview: value == "true")
        "is_free" -> assign(acc, is_free: value == "true")
        _ -> acc
      end
    end)
  end

  defp create_lesson(assigns) do
    Logger.info("🔧 [create_lesson] Creating lesson")
    Logger.info("   Module ID: #{inspect(assigns.module_id)}")
    Logger.info("   Path ID: #{inspect(assigns.path_id)}")
    Logger.info("   Creator ID: #{inspect(assigns.user_id)}")
    Logger.info("   Title: #{inspect(assigns.title)}")
    Logger.info("   Lesson Order: #{assigns.lesson_order}")

    module_id = assigns.module_id
    creator_id = assigns.user_id
    title = assigns.title
    description = assigns.description
    lesson_number = assigns.lesson_order
    content_type = String.to_atom(assigns.content_type)
    duration_minutes = parse_integer(assigns.video_duration)

    content_data =
      case content_type do
        :video -> assigns.video_url
        :text -> assigns.text_content
        _ -> ""
      end

    Logger.info("   Content type: #{inspect(content_type)}")
    Logger.info("   Duration: #{duration_minutes}")
    Logger.info("   Content data length: #{String.length(content_data)}")

    try do
      charlist_module_id =
        if is_binary(module_id), do: String.to_charlist(module_id), else: module_id

      charlist_creator_id =
        if is_binary(creator_id), do: String.to_charlist(creator_id), else: creator_id

      charlist_title = String.to_charlist(title)
      charlist_description = String.to_charlist(description)
      charlist_content_data = String.to_charlist(content_data)

      Logger.info("🔧 [create_lesson] Calling learningdb:create_lesson/18")

      result =
        :learningdb.create_lesson(
          charlist_module_id,
          charlist_creator_id,
          'Unknown',
          'Unknown',
          charlist_title,
          charlist_description,
          lesson_number,
          content_type,
          duration_minutes,
          :undefined,
          :undefined,
          :undefined,
          charlist_content_data,
          :undefined,
          [],
          [],
          :undefined,
          :undefined
        )

      Logger.info("🔍 [create_lesson] Result: #{inspect(result)}")

      case result do
        lesson_id when is_binary(lesson_id) or is_list(lesson_id) ->
          id = if is_list(lesson_id), do: List.to_string(lesson_id), else: lesson_id
          Logger.info("✅ [create_lesson] Success: #{id}")
          {:ok, id}

        {:ok, lesson_id} ->
          id = if is_list(lesson_id), do: List.to_string(lesson_id), else: to_string(lesson_id)
          Logger.info("✅ [create_lesson] Success: #{id}")
          {:ok, id}

        {:error, reason} ->
          Logger.error("❌ [create_lesson] Error: #{inspect(reason)}")
          {:error, reason}

        other ->
          Logger.error("❌ [create_lesson] Unexpected: #{inspect(other)}")
          {:error, :unexpected_result}
      end
    rescue
      error ->
        Logger.error("❌ [create_lesson] Exception: #{inspect(error)}")
        Logger.error("   Stacktrace: #{inspect(__STACKTRACE__)}")
        {:error, :creation_failed}
    end
  end

  defp parse_integer(value) when is_binary(value) do
    case Integer.parse(value) do
      {int, _} -> int
      _ -> 0
    end
  end

  defp parse_integer(_), do: 0
end
