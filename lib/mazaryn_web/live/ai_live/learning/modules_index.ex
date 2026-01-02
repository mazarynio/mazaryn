defmodule MazarynWeb.AiLive.Learning.ModulesIndex do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"path_id" => path_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("🚀 [ModulesIndex.mount] Starting")
    Logger.info("   Path ID: #{inspect(path_id)}")
    Logger.info("   Session UUID: #{inspect(session_uuid)}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)
      Logger.info("✅ [ModulesIndex.mount] User found: #{user_id}")

      case load_path_data(path_id) do
        {:ok, path} ->
          Logger.info("✅ [ModulesIndex.mount] Path loaded: #{path.title}")

          if path.creator_id == user_id do
            modules = load_modules(path_id)
            Logger.info("✅ [ModulesIndex.mount] Loaded #{length(modules)} modules")

            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(path_id: path_id)
              |> assign(path: path)
              |> assign(modules: modules)
              |> assign(show_delete_modal: false)
              |> assign(delete_target: nil)

            {:ok, socket}
          else
            Logger.error("❌ [ModulesIndex.mount] User is not the creator")
            locale = socket.assigns[:locale] || "en"

            {:ok,
             socket
             |> put_flash(:error, "You are not authorized to manage this course")
             |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}")}
          end

        {:error, reason} ->
          Logger.error("❌ [ModulesIndex.mount] Failed to load path: #{inspect(reason)}")
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, "Course not found")
           |> redirect(to: "/#{locale}/ai/learning")}
      end
    else
      {:error, reason} ->
        Logger.error("❌ [ModulesIndex.mount] Failed to get user: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(%{"path_id" => path_id}, %{"user_id" => user_id} = _session, socket) do
    Logger.info("🚀 [ModulesIndex.mount] Starting with user_id")
    Logger.info("   Path ID: #{inspect(path_id)}")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)

        case load_path_data(path_id) do
          {:ok, path} ->
            if path.creator_id == uid do
              modules = load_modules(path_id)

              socket =
                socket
                |> assign(user: user)
                |> assign(user_id: uid)
                |> assign(path_id: path_id)
                |> assign(path: path)
                |> assign(modules: modules)
                |> assign(show_delete_modal: false)
                |> assign(delete_target: nil)

              {:ok, socket}
            else
              locale = socket.assigns[:locale] || "en"

              {:ok,
               socket
               |> put_flash(:error, "You are not authorized to manage this course")
               |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}")}
            end

          {:error, _reason} ->
            locale = socket.assigns[:locale] || "en"

            {:ok,
             socket
             |> put_flash(:error, "Course not found")
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
  def handle_event("reorder_modules", %{"old" => old_idx, "new" => new_idx}, socket) do
    Logger.info("📋 [ModulesIndex] Reordering module from #{old_idx} to #{new_idx}")

    modules = socket.assigns.modules
    module_to_move = Enum.at(modules, old_idx)

    updated_modules =
      modules
      |> List.delete_at(old_idx)
      |> List.insert_at(new_idx, module_to_move)
      |> Enum.with_index(1)
      |> Enum.map(fn {mod, new_order} ->
        Map.put(mod, :order, new_order)
      end)

    Enum.each(updated_modules, fn mod ->
      module_id = if is_binary(mod.id), do: String.to_charlist(mod.id), else: mod.id
      :learningdb.update_module_order(module_id, mod.order)
    end)

    {:noreply, assign(socket, modules: updated_modules)}
  end

  @impl true
  def handle_event("delete_module", %{"id" => module_id}, socket) do
    Logger.info("🗑️  [ModulesIndex] Showing delete confirmation for module: #{module_id}")

    {:noreply,
     socket
     |> assign(show_delete_modal: true)
     |> assign(delete_target: module_id)}
  end

  @impl true
  def handle_event("cancel_delete", _params, socket) do
    {:noreply,
     socket
     |> assign(show_delete_modal: false)
     |> assign(delete_target: nil)}
  end

  @impl true
  def handle_event("confirm_delete", _params, socket) do
    module_id = socket.assigns.delete_target
    Logger.info("🗑️  [ModulesIndex] Deleting module: #{module_id}")

    charlist_id = if is_binary(module_id), do: String.to_charlist(module_id), else: module_id
    user_id = socket.assigns.user_id
    charlist_user_id = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

    case :learningdb.delete_module(charlist_id, charlist_user_id) do
      :ok ->
        modules = load_modules(socket.assigns.path_id)

        {:noreply,
         socket
         |> assign(modules: modules)
         |> assign(show_delete_modal: false)
         |> assign(delete_target: nil)
         |> put_flash(:info, "Module deleted successfully")}

      {:error, reason} ->
        Logger.error("❌ [ModulesIndex] Delete failed: #{inspect(reason)}")

        {:noreply,
         socket
         |> assign(show_delete_modal: false)
         |> assign(delete_target: nil)
         |> put_flash(:error, "Failed to delete module")}
    end
  end

  defp to_string_safe(value) when is_list(value), do: List.to_string(value)
  defp to_string_safe(value) when is_binary(value), do: value
  defp to_string_safe(nil), do: nil
  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(value), do: to_string(value)

  defp load_path_data(path_id) do
    try do
      Logger.info("🔍 [load_path_data] Loading path: #{inspect(path_id)}")

      charlist_id = if is_binary(path_id), do: String.to_charlist(path_id), else: path_id

      case :learningdb.get_learning_path(charlist_id) do
        path when is_tuple(path) and tuple_size(path) > 10 ->
          id = to_string_safe(elem(path, 1))
          title = to_string_safe(elem(path, 2))
          creator_id = to_string_safe(elem(path, 4))
          creator_name = to_string_safe(elem(path, 5))

          {:ok,
           %{
             id: id,
             title: title || "Untitled Course",
             creator_id: creator_id,
             creator_name: creator_name || "Unknown"
           }}

        {:error, reason} ->
          {:error, reason}

        other ->
          Logger.error("❌ [load_path_data] Unexpected: #{inspect(other)}")
          {:error, :not_found}
      end
    rescue
      error ->
        Logger.error("❌ [load_path_data] Exception: #{inspect(error)}")
        {:error, :exception}
    end
  end

  defp load_modules(path_id) do
    try do
      Logger.info("🔍 [load_modules] Loading modules for path: #{inspect(path_id)}")

      charlist_id = if is_binary(path_id), do: String.to_charlist(path_id), else: path_id

      case :learningdb.get_modules_by_path(charlist_id) do
        modules when is_list(modules) ->
          Logger.info("✅ [load_modules] Found #{length(modules)} modules")
          Logger.info("   First module tuple: #{inspect(Enum.at(modules, 0))}")

          modules
          |> Enum.map(fn mod ->
            Logger.info("🔧 [load_modules] Processing module tuple size: #{tuple_size(mod)}")
            Logger.info("   Full tuple: #{inspect(mod)}")

            module_id = to_string_safe(elem(mod, 1))
            path_id_from_mod = to_string_safe(elem(mod, 2))
            learning_path_id = to_string_safe(elem(mod, 3))
            creator_id = to_string_safe(elem(mod, 4))
            creator_name = to_string_safe(elem(mod, 5))
            creator_family = to_string_safe(elem(mod, 6))
            title = to_string_safe(elem(mod, 7))
            description = to_string_safe(elem(mod, 8))
            module_number = elem(mod, 9)
            estimated_hours = elem(mod, 10)

            Logger.info("   Module ID: #{inspect(module_id)}")
            Logger.info("   Title: #{inspect(title)}")
            Logger.info("   Description: #{inspect(description)}")
            Logger.info("   Module Number: #{inspect(module_number)}")

            lessons = count_lessons(module_id)

            %{
              id: module_id,
              path_id: path_id_from_mod,
              learning_path_id: learning_path_id,
              creator_id: creator_id,
              creator_name: creator_name,
              creator_family: creator_family,
              title: title || "Untitled Module",
              description: description || "",
              order: module_number || 1,
              estimated_hours: estimated_hours || 0,
              lesson_count: lessons
            }
          end)
          |> Enum.sort_by(& &1.order)

        other ->
          Logger.error("❌ [load_modules] Unexpected: #{inspect(other)}")
          []
      end
    rescue
      error ->
        Logger.error("❌ [load_modules] Exception: #{inspect(error)}")
        Logger.error("   Stacktrace: #{inspect(__STACKTRACE__)}")
        []
    end
  end

  defp count_lessons(module_id) do
    try do
      charlist_id = if is_binary(module_id), do: String.to_charlist(module_id), else: module_id

      case :learningdb.get_lessons_by_module(charlist_id) do
        lessons when is_list(lessons) -> length(lessons)
        _ -> 0
      end
    rescue
      _ -> 0
    end
  end
end
