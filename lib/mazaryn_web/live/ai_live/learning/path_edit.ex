defmodule MazarynWeb.AiLive.Learning.PathEdit do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"id" => path_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)

      if is_verified_instructor?(user_id) do
        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(session_uuid: session_uuid)
          |> assign(path_id: path_id)
          |> load_learning_path(path_id, user_id)

        {:ok, socket}
      else
        locale = socket.assigns[:locale] || "en"

        {:ok,
         socket
         |> put_flash(:error, "You must be a verified instructor to edit learning paths")
         |> redirect(to: "/#{locale}/ai/learning")}
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
  def mount(%{"id" => path_id}, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)

        if is_verified_instructor?(uid) do
          socket =
            socket
            |> assign(user: user)
            |> assign(user_id: uid)
            |> assign(path_id: path_id)
            |> load_learning_path(path_id, uid)

          {:ok, socket}
        else
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, "You must be a verified instructor to edit learning paths")
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
  def handle_event("validate", params, socket) do
    errors = validate_form(params)
    socket = update_form_fields(socket, params)
    {:noreply, assign(socket, errors: errors)}
  end

  @impl true
  def handle_event("update_field", %{"field" => field, "value" => value}, socket) do
    field_atom = String.to_existing_atom(field)
    {:noreply, assign(socket, [{field_atom, value}])}
  end

  @impl true
  def handle_event("add_tag", %{"tag" => tag}, socket) do
    tag = String.trim(tag)

    if tag != "" and tag not in socket.assigns.tags and length(socket.assigns.tags) < 10 do
      {:noreply, assign(socket, tags: socket.assigns.tags ++ [tag])}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("remove_tag", %{"tag" => tag}, socket) do
    {:noreply, assign(socket, tags: List.delete(socket.assigns.tags, tag))}
  end

  @impl true
  def handle_event("add_outcome", %{"outcome" => outcome}, socket) do
    outcome = String.trim(outcome)

    if outcome != "" and length(socket.assigns.learning_outcomes) < 10 do
      {:noreply, assign(socket, learning_outcomes: socket.assigns.learning_outcomes ++ [outcome])}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("remove_outcome", %{"index" => index}, socket) do
    index = String.to_integer(index)

    {:noreply,
     assign(socket, learning_outcomes: List.delete_at(socket.assigns.learning_outcomes, index))}
  end

  @impl true
  def handle_event("submit", params, socket) do
    errors = validate_form(params)

    if map_size(errors) == 0 do
      socket = assign(socket, submitting: true)

      case update_learning_path(socket.assigns, params) do
        :ok ->
          locale = socket.assigns[:locale] || "en"

          {:noreply,
           socket
           |> put_flash(:info, "Learning path updated successfully!")
           |> redirect(to: "/#{locale}/ai/learning/paths/#{socket.assigns.path_id}")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(submitting: false)
           |> put_flash(:error, "Failed to update learning path: #{inspect(reason)}")}
      end
    else
      {:noreply, assign(socket, errors: errors)}
    end
  end

  defp is_verified_instructor?(user_id) do
    case :instructordb.is_verified_instructor(user_id) do
      true -> true
      _ -> false
    end
  rescue
    _ -> false
  end

  defp load_learning_path(socket, path_id, user_id) do
    case :learningdb.get_learning_path(path_id) do
      {:error, :not_found} ->
        locale = socket.assigns[:locale] || "en"

        socket
        |> put_flash(:error, "Learning path not found")
        |> redirect(to: "/#{locale}/ai/learning")

      path_tuple ->
        case Mazaryn.Schema.Learning.erl_changeset(path_tuple) do
          changeset when is_map(changeset) ->
            path = Ecto.Changeset.apply_changes(changeset)

            if path.creator_id == user_id do
              socket
              |> assign(path: path)
              |> assign(title: path.title || "")
              |> assign(description: path.description || "")
              |> assign(category: path.category || "")
              |> assign(difficulty_level: to_string(path.difficulty_level))
              |> assign(estimated_duration: to_string(div(path.estimated_duration || 0, 3600)))
              |> assign(price: to_string(path.price || 0.0))
              |> assign(tags: path.tags || [])
              |> assign(learning_outcomes: path.learning_outcomes || [])
              |> assign(prerequisites: path.prerequisites || "")
              |> assign(language: path.language || "en")
              |> assign(self_paced: path.self_paced || true)
              |> assign(lifetime_access: path.lifetime_access || true)
              |> assign(errors: %{})
              |> assign(submitting: false)
            else
              locale = socket.assigns[:locale] || "en"

              socket
              |> put_flash(:error, "You don't have permission to edit this learning path")
              |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}")
            end

          _ ->
            locale = socket.assigns[:locale] || "en"

            socket
            |> put_flash(:error, "Invalid learning path data")
            |> redirect(to: "/#{locale}/ai/learning")
        end
    end
  rescue
    _ ->
      locale = socket.assigns[:locale] || "en"

      socket
      |> put_flash(:error, "Failed to load learning path")
      |> redirect(to: "/#{locale}/ai/learning")
  end

  defp validate_form(params) do
    errors = %{}

    errors =
      if String.trim(params["title"] || "") == "" do
        Map.put(errors, :title, "Title is required")
      else
        errors
      end

    errors =
      if String.length(params["description"] || "") < 100 do
        Map.put(errors, :description, "Description must be at least 100 characters")
      else
        errors
      end

    errors =
      if String.trim(params["category"] || "") == "" do
        Map.put(errors, :category, "Category is required")
      else
        errors
      end

    errors =
      if String.trim(params["estimated_duration"] || "") == "" do
        Map.put(errors, :estimated_duration, "Estimated duration is required")
      else
        errors
      end

    errors
  end

  defp update_form_fields(socket, params) do
    Enum.reduce(params, socket, fn {key, value}, acc ->
      case key do
        "title" -> assign(acc, title: value)
        "description" -> assign(acc, description: value)
        "category" -> assign(acc, category: value)
        "difficulty_level" -> assign(acc, difficulty_level: value)
        "estimated_duration" -> assign(acc, estimated_duration: value)
        "price" -> assign(acc, price: value)
        "prerequisites" -> assign(acc, prerequisites: value)
        "language" -> assign(acc, language: value)
        "self_paced" -> assign(acc, self_paced: value == "true")
        "lifetime_access" -> assign(acc, lifetime_access: value == "true")
        _ -> acc
      end
    end)
  end

  defp update_learning_path(assigns, _params) do
    path_id = assigns.path_id
    title = assigns.title
    description = assigns.description
    category = assigns.category
    difficulty_level = String.to_atom(assigns.difficulty_level)
    estimated_duration = parse_duration(assigns.estimated_duration)
    price = parse_float(assigns.price)
    tags = assigns.tags
    learning_outcomes = assigns.learning_outcomes
    prerequisites = assigns.prerequisites

    try do
      updates = [
        {:title, title},
        {:description, description},
        {:category, category},
        {:difficulty_level, difficulty_level},
        {:estimated_duration, estimated_duration},
        {:tags, tags},
        {:prerequisites, prerequisites}
      ]

      case :learningdb.update_learning_path(path_id, updates) do
        :ok ->
          if price != assigns.path.price do
            :learningdb.set_path_price(path_id, price, "USD")
          end

          if learning_outcomes != assigns.path.learning_outcomes do
            :learningdb.set_learning_outcomes(path_id, learning_outcomes)
          end

          :ok

        {:error, reason} ->
          Logger.error("Failed to update learning path: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception updating learning path: #{inspect(error)}")
        {:error, :update_failed}
    end
  end

  defp parse_duration(duration_str) do
    case Integer.parse(duration_str || "0") do
      {hours, _} -> hours * 3600
      _ -> 0
    end
  end

  defp parse_float(float_str) do
    case Float.parse(float_str || "0.0") do
      {value, _} -> value
      _ -> 0.0
    end
  end
end
