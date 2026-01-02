defmodule MazarynWeb.AiLive.Learning.PathNew do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)

      if is_verified_instructor?(user_id) do
        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(session_uuid: session_uuid)
          |> assign_form_fields()
          |> assign(current_step: 1)
          |> assign(total_steps: 4)
          |> assign(uploading_thumbnail: false)
          |> assign(uploading_preview: false)
          |> assign(errors: %{})
          |> assign(submitting: false)
          |> assign(save_as_draft: false)
          |> assign(show_preview: false)

        {:ok, socket}
      else
        locale = socket.assigns[:locale] || "en"

        {:ok,
         socket
         |> put_flash(:error, "You must be a verified instructor to create learning paths")
         |> redirect(to: "/#{locale}/ai/learning/become-instructor")}
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
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)

        if is_verified_instructor?(uid) do
          socket =
            socket
            |> assign(user: user)
            |> assign(user_id: uid)
            |> assign_form_fields()
            |> assign(current_step: 1)
            |> assign(total_steps: 4)
            |> assign(uploading_thumbnail: false)
            |> assign(uploading_preview: false)
            |> assign(errors: %{})
            |> assign(submitting: false)
            |> assign(save_as_draft: false)
            |> assign(show_preview: false)

          {:ok, socket}
        else
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, "You must be a verified instructor to create learning paths")
           |> redirect(to: "/#{locale}/ai/learning/become-instructor")}
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
    current_step = socket.assigns.current_step
    errors = validate_step(current_step, socket.assigns)

    if map_size(errors) == 0 do
      {:noreply,
       assign(socket,
         current_step: min(current_step + 1, socket.assigns.total_steps),
         errors: %{}
       )}
    else
      {:noreply, assign(socket, errors: errors)}
    end
  end

  @impl true
  def handle_event("prev_step", _params, socket) do
    {:noreply, assign(socket, current_step: max(socket.assigns.current_step - 1, 1), errors: %{})}
  end

  @impl true
  def handle_event("go_to_step", %{"step" => step_str}, socket) do
    step = String.to_integer(step_str)

    if step >= 1 and step <= socket.assigns.total_steps do
      {:noreply, assign(socket, current_step: step, errors: %{})}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("validate", params, socket) do
    socket = update_form_fields(socket, params)
    {:noreply, socket}
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
  def handle_event("add_prerequisite", %{"prerequisite" => prereq}, socket) do
    prereq = String.trim(prereq)

    if prereq != "" and length(socket.assigns.prerequisite_list) < 10 do
      {:noreply, assign(socket, prerequisite_list: socket.assigns.prerequisite_list ++ [prereq])}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("remove_prerequisite", %{"index" => index}, socket) do
    index = String.to_integer(index)

    {:noreply,
     assign(socket, prerequisite_list: List.delete_at(socket.assigns.prerequisite_list, index))}
  end

  @impl true
  def handle_event("toggle_preview", _params, socket) do
    {:noreply, assign(socket, show_preview: !socket.assigns.show_preview)}
  end

  @impl true
  def handle_event("save_draft", _params, socket) do
    socket = assign(socket, save_as_draft: true, submitting: true)

    case create_learning_path(socket.assigns, true) do
      {:ok, path_id} ->
        locale = socket.assigns[:locale] || "en"

        {:noreply,
         socket
         |> put_flash(:info, "Draft saved successfully!")
         |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}/edit")}

      {:error, reason} ->
        {:noreply,
         socket
         |> assign(submitting: false, save_as_draft: false)
         |> put_flash(:error, "Failed to save draft: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("submit", params, socket) do
    socket = update_form_fields(socket, params)
    errors = validate_all_steps(socket.assigns)

    if map_size(errors) == 0 do
      socket = assign(socket, submitting: true, save_as_draft: false)

      case create_learning_path(socket.assigns, false) do
        {:ok, path_id} ->
          locale = socket.assigns[:locale] || "en"

          {:noreply,
           socket
           |> put_flash(
             :info,
             "Learning path created successfully! It will be reviewed by administrators."
           )
           |> redirect(to: "/#{locale}/ai/learning/instructor/dashboard")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(submitting: false)
           |> put_flash(:error, "Failed to create learning path: #{inspect(reason)}")}
      end
    else
      first_error_step = get_first_error_step(errors)
      {:noreply, assign(socket, errors: errors, current_step: first_error_step)}
    end
  end

  defp assign_form_fields(socket) do
    socket
    |> assign(title: "")
    |> assign(description: "")
    |> assign(category: "")
    |> assign(subcategory: "")
    |> assign(difficulty_level: "beginner")
    |> assign(estimated_duration: "")
    |> assign(price: "0.0")
    |> assign(currency: "USD")
    |> assign(tags: [])
    |> assign(learning_outcomes: [])
    |> assign(prerequisite_list: [])
    |> assign(prerequisites: "")
    |> assign(thumbnail_cid: nil)
    |> assign(preview_video_cid: nil)
    |> assign(language: "en")
    |> assign(self_paced: true)
    |> assign(lifetime_access: true)
    |> assign(certification_available: false)
    |> assign(track: "general")
  end

  defp is_verified_instructor?(user_id) do
    case :instructordb.is_verified_instructor(user_id) do
      true -> true
      _ -> false
    end
  rescue
    _ -> false
  end

  defp validate_step(1, assigns) do
    errors = %{}

    errors =
      if String.trim(assigns.title) == "",
        do: Map.put(errors, :title, "Title is required"),
        else: errors

    errors =
      if String.length(assigns.description) < 100,
        do: Map.put(errors, :description, "Description must be at least 100 characters"),
        else: errors

    errors =
      if String.trim(assigns.category) == "",
        do: Map.put(errors, :category, "Category is required"),
        else: errors

    errors
  end

  defp validate_step(2, assigns) do
    errors = %{}

    errors =
      if String.trim(assigns.estimated_duration) == "",
        do: Map.put(errors, :estimated_duration, "Estimated duration is required"),
        else: errors

    errors =
      if length(assigns.tags) == 0,
        do: Map.put(errors, :tags, "Add at least one tag"),
        else: errors

    errors =
      if length(assigns.learning_outcomes) == 0,
        do: Map.put(errors, :learning_outcomes, "Add at least one learning outcome"),
        else: errors

    errors
  end

  defp validate_step(3, _assigns), do: %{}
  defp validate_step(4, _assigns), do: %{}

  defp validate_all_steps(assigns) do
    %{}
    |> Map.merge(validate_step(1, assigns))
    |> Map.merge(validate_step(2, assigns))
    |> Map.merge(validate_step(3, assigns))
    |> Map.merge(validate_step(4, assigns))
  end

  defp get_first_error_step(errors) do
    cond do
      Map.has_key?(errors, :title) or Map.has_key?(errors, :description) or
          Map.has_key?(errors, :category) ->
        1

      Map.has_key?(errors, :estimated_duration) or Map.has_key?(errors, :tags) or
          Map.has_key?(errors, :learning_outcomes) ->
        2

      true ->
        3
    end
  end

  defp update_form_fields(socket, params) do
    Enum.reduce(params, socket, fn {key, value}, acc ->
      case key do
        "title" -> assign(acc, title: value)
        "description" -> assign(acc, description: value)
        "category" -> assign(acc, category: value)
        "subcategory" -> assign(acc, subcategory: value)
        "difficulty_level" -> assign(acc, difficulty_level: value)
        "estimated_duration" -> assign(acc, estimated_duration: value)
        "price" -> assign(acc, price: value)
        "currency" -> assign(acc, currency: value)
        "prerequisites" -> assign(acc, prerequisites: value)
        "language" -> assign(acc, language: value)
        "track" -> assign(acc, track: value)
        "self_paced" -> assign(acc, self_paced: value == "true")
        "lifetime_access" -> assign(acc, lifetime_access: value == "true")
        "certification_available" -> assign(acc, certification_available: value == "true")
        _ -> acc
      end
    end)
  end

  defp create_learning_path(assigns, is_draft) do
    user_id = assigns.user_id
    creator_name = assigns.user.username || ""
    creator_family = ""

    title = assigns.title
    description = assigns.description
    category = assigns.category
    difficulty_level = String.to_atom(assigns.difficulty_level)
    estimated_duration = parse_duration(assigns.estimated_duration)
    price = parse_float(assigns.price)
    tags = assigns.tags
    language = assigns.language
    track = assigns.track || "general"
    subcategory = assigns.subcategory || ""

    try do
      path_id =
        :learningdb.create_learning_path(
          user_id,
          creator_name,
          creator_family,
          title,
          description,
          difficulty_level,
          estimated_duration,
          track,
          category,
          subcategory,
          tags,
          price,
          "USD",
          language,
          <<>>
        )

      {:ok, path_id}
    rescue
      error ->
        Logger.error("Exception creating learning path: #{inspect(error)}")
        Logger.error("Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
        {:error, :creation_failed}
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
