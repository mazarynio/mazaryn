defmodule MazarynWeb.AiLive.Learning.ModuleNew do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"path_id" => path_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("🚀 [ModuleNew.mount] Starting")
    Logger.info("   Path ID: #{inspect(path_id)}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)
      creator_name = get_user_field(user, :name, "Unknown")
      creator_family = get_user_field(user, :family, "Unknown")

      Logger.info("✅ [ModuleNew.mount] User found: #{user_id}")
      Logger.info("   Creator: #{creator_name} #{creator_family}")

      case load_path_data(path_id) do
        {:ok, path} ->
          if path.creator_id == user_id do
            next_order = get_next_module_order(path_id)
            Logger.info("✅ [ModuleNew.mount] Next order: #{next_order}")

            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(creator_name: creator_name)
              |> assign(creator_family: creator_family)
              |> assign(path_id: path_id)
              |> assign(path: path)
              |> assign(
                form:
                  to_form(%{
                    "title" => "",
                    "description" => "",
                    "estimated_hours" => "",
                    "order" => next_order
                  })
              )
              |> assign(errors: %{})

            {:ok, socket}
          else
            locale = socket.assigns[:locale] || "en"

            {:ok,
             socket
             |> put_flash(:error, "You are not authorized")
             |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}")}
          end

        {:error, reason} ->
          Logger.error("❌ [ModuleNew.mount] Failed: #{inspect(reason)}")
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, "Course not found")
           |> redirect(to: "/#{locale}/ai/learning")}
      end
    else
      {:error, reason} ->
        Logger.error("❌ [ModuleNew.mount] Auth failed: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(%{"path_id" => path_id}, %{"user_id" => user_id} = _session, socket) do
    Logger.info("🚀 [ModuleNew.mount] Starting with user_id")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)
        creator_name = get_user_field(user, :name, "Unknown")
        creator_family = get_user_field(user, :family, "Unknown")

        case load_path_data(path_id) do
          {:ok, path} ->
            if path.creator_id == uid do
              next_order = get_next_module_order(path_id)

              socket =
                socket
                |> assign(user: user)
                |> assign(user_id: uid)
                |> assign(creator_name: creator_name)
                |> assign(creator_family: creator_family)
                |> assign(path_id: path_id)
                |> assign(path: path)
                |> assign(
                  form:
                    to_form(%{
                      "title" => "",
                      "description" => "",
                      "estimated_hours" => "",
                      "order" => next_order
                    })
                )
                |> assign(errors: %{})

              {:ok, socket}
            else
              locale = socket.assigns[:locale] || "en"

              {:ok,
               socket
               |> put_flash(:error, "You are not authorized")
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
  def handle_event("validate", %{"module" => params}, socket) do
    Logger.info("📝 [ModuleNew] Validating: #{inspect(params)}")
    {:noreply, assign(socket, form: to_form(params), errors: %{})}
  end

  @impl true
  def handle_event("save", %{"module" => params}, socket) do
    Logger.info("💾 [ModuleNew] Saving module")
    Logger.info("   Params: #{inspect(params)}")

    case validate_module(params) do
      {:ok, validated} ->
        case create_module(
               validated,
               socket.assigns.path_id,
               socket.assigns.user_id,
               socket.assigns.creator_name,
               socket.assigns.creator_family
             ) do
          {:ok, module_id} ->
            Logger.info("✅ [ModuleNew] Created module: #{module_id}")
            locale = socket.assigns[:locale] || "en"

            {:noreply,
             socket
             |> put_flash(:info, "Module created successfully")
             |> redirect(to: "/#{locale}/ai/learning/paths/#{socket.assigns.path_id}/modules")}

          {:error, reason} ->
            Logger.error("❌ [ModuleNew] Create failed: #{inspect(reason)}")

            {:noreply,
             socket
             |> assign(errors: %{base: "Failed to create module"})
             |> put_flash(:error, "Failed to create module")}
        end

      {:error, errors} ->
        Logger.error("❌ [ModuleNew] Validation failed: #{inspect(errors)}")
        {:noreply, assign(socket, errors: errors)}
    end
  end

  defp validate_module(params) do
    errors = %{}

    errors =
      if is_nil(params["title"]) || String.trim(params["title"]) == "" do
        Map.put(errors, :title, "Title is required")
      else
        errors
      end

    errors =
      if is_nil(params["description"]) || String.trim(params["description"]) == "" do
        Map.put(errors, :description, "Description is required")
      else
        errors
      end

    estimated_hours =
      case params["estimated_hours"] do
        "" ->
          0

        nil ->
          0

        val when is_binary(val) ->
          case Integer.parse(val) do
            {num, _} -> num
            :error -> 0
          end

        val when is_integer(val) ->
          val

        _ ->
          0
      end

    order =
      case params["order"] do
        val when is_binary(val) ->
          case Integer.parse(val) do
            {num, _} -> num
            :error -> 1
          end

        val when is_integer(val) ->
          val

        _ ->
          1
      end

    if map_size(errors) > 0 do
      {:error, errors}
    else
      {:ok,
       %{
         title: String.trim(params["title"]),
         description: String.trim(params["description"]),
         estimated_hours: estimated_hours,
         order: order
       }}
    end
  end

  defp create_module(params, path_id, creator_id, creator_name, creator_family) do
    try do
      charlist_path_id = if is_binary(path_id), do: String.to_charlist(path_id), else: path_id

      charlist_creator_id =
        if is_binary(creator_id), do: String.to_charlist(creator_id), else: creator_id

      charlist_creator_name =
        if is_binary(creator_name), do: String.to_charlist(creator_name), else: 'Unknown'

      charlist_creator_family =
        if is_binary(creator_family), do: String.to_charlist(creator_family), else: 'Unknown'

      charlist_title = String.to_charlist(params.title)
      charlist_description = String.to_charlist(params.description)

      Logger.info("🔧 [create_module] Calling learningdb:create_module/11")
      Logger.info("   Path ID: #{inspect(charlist_path_id)}")
      Logger.info("   Creator ID: #{inspect(charlist_creator_id)}")
      Logger.info("   Creator Name: #{inspect(charlist_creator_name)}")
      Logger.info("   Creator Family: #{inspect(charlist_creator_family)}")
      Logger.info("   Title: #{inspect(charlist_title)}")
      Logger.info("   Order (ModuleNumber): #{params.order}")
      Logger.info("   Estimated Hours: #{params.estimated_hours}")

      result =
        :learningdb.create_module(
          charlist_path_id,
          charlist_creator_id,
          charlist_creator_name,
          charlist_creator_family,
          charlist_title,
          charlist_description,
          params.order,
          params.estimated_hours,
          [],
          [],
          :undefined
        )

      Logger.info("🔍 [create_module] Result: #{inspect(result)}")

      case result do
        module_id when is_binary(module_id) or is_list(module_id) ->
          id = if is_list(module_id), do: List.to_string(module_id), else: module_id
          Logger.info("✅ [create_module] Success: #{id}")
          {:ok, id}

        {:ok, module_id} ->
          id = if is_list(module_id), do: List.to_string(module_id), else: to_string(module_id)
          Logger.info("✅ [create_module] Success: #{id}")
          {:ok, id}

        {:error, reason} ->
          Logger.error("❌ [create_module] Error: #{inspect(reason)}")
          {:error, reason}

        other ->
          Logger.error("❌ [create_module] Unexpected result: #{inspect(other)}")
          {:error, :unexpected_result}
      end
    rescue
      error ->
        Logger.error("❌ [create_module] Exception: #{inspect(error)}")
        Logger.error("   Stacktrace: #{inspect(__STACKTRACE__)}")
        {:error, :exception}
    end
  end

  defp to_string_safe(value) when is_list(value), do: List.to_string(value)
  defp to_string_safe(value) when is_binary(value), do: value
  defp to_string_safe(nil), do: nil
  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(value), do: to_string(value)

  defp get_user_field(user, field, default) do
    case Map.get(user, field) do
      nil -> default
      value when is_binary(value) -> value
      value when is_list(value) -> List.to_string(value)
      _ -> default
    end
  end

  defp load_path_data(path_id) do
    try do
      charlist_id = if is_binary(path_id), do: String.to_charlist(path_id), else: path_id

      case :learningdb.get_learning_path(charlist_id) do
        path when is_tuple(path) and tuple_size(path) > 10 ->
          {:ok,
           %{
             id: to_string_safe(elem(path, 1)),
             title: to_string_safe(elem(path, 2)) || "Untitled Course",
             creator_id: to_string_safe(elem(path, 4))
           }}

        {:error, reason} ->
          {:error, reason}

        _ ->
          {:error, :not_found}
      end
    rescue
      error ->
        Logger.error("❌ Exception: #{inspect(error)}")
        {:error, :exception}
    end
  end

  defp get_next_module_order(path_id) do
    try do
      charlist_id = if is_binary(path_id), do: String.to_charlist(path_id), else: path_id

      case :learningdb.get_modules_by_path(charlist_id) do
        modules when is_list(modules) ->
          if length(modules) == 0 do
            1
          else
            max_order =
              Enum.reduce(modules, 0, fn mod, acc ->
                order = elem(mod, 7) || 0
                max(order, acc)
              end)

            max_order + 1
          end

        _ ->
          1
      end
    rescue
      _ -> 1
    end
  end
end
