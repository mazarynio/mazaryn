defmodule MazarynWeb.AiLive.Learning.BecomeInstructor do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("🔵 BecomeInstructor mount - session_uuid: #{session_uuid}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)
      Logger.info("🔵 User loaded - user_id: #{user_id}")

      socket =
        socket
        |> assign(user: user)
        |> assign(user_id: user_id)
        |> assign(session_uuid: session_uuid)
        |> assign(form_step: 1)
        |> assign(total_steps: 3)
        |> assign(name: user.username || "")
        |> assign(family: "")
        |> assign(bio: "")
        |> assign(expertise: "")
        |> assign(reason: "")
        |> assign(portfolio_url: "")
        |> assign(linkedin_url: "")
        |> assign(years_experience: "")
        |> assign(teaching_experience: "")
        |> assign(errors: %{})
        |> assign(submitting: false)
        |> assign(application_submitted: false)
        |> check_existing_status()

      {:ok, socket}
    else
      {:error, reason} ->
        Logger.error("🔴 Failed to get user: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info("🔵 BecomeInstructor mount - user_id: #{user_id}")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)
        Logger.info("🔵 User loaded - uid: #{uid}")

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: uid)
          |> assign(form_step: 1)
          |> assign(total_steps: 3)
          |> assign(name: user.username || "")
          |> assign(family: "")
          |> assign(bio: "")
          |> assign(expertise: "")
          |> assign(reason: "")
          |> assign(portfolio_url: "")
          |> assign(linkedin_url: "")
          |> assign(years_experience: "")
          |> assign(teaching_experience: "")
          |> assign(errors: %{})
          |> assign(submitting: false)
          |> assign(application_submitted: false)
          |> check_existing_status()

        {:ok, socket}

      {:error, reason} ->
        Logger.error("🔴 Failed to get user: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("validate", params, socket) do
    Logger.info("🟡 Validate event - step: #{socket.assigns.form_step}")
    Logger.debug("Params: #{inspect(params)}")

    socket =
      socket
      |> update_form_fields(params)
      |> assign(errors: validate_current_step(socket.assigns.form_step, params))

    {:noreply, socket}
  end

  @impl true
  def handle_event("next_step", params, socket) do
    current_step = socket.assigns.form_step
    Logger.info("🟢 Next step event - current step: #{current_step}")
    Logger.debug("Params: #{inspect(params)}")

    errors = validate_current_step(current_step, params)
    Logger.debug("Validation errors: #{inspect(errors)}")

    if map_size(errors) == 0 do
      Logger.info("✅ Validation passed - moving to step #{current_step + 1}")

      socket =
        socket
        |> update_form_fields(params)
        |> assign(form_step: current_step + 1)
        |> assign(errors: %{})

      {:noreply, socket}
    else
      Logger.warning("❌ Validation failed - errors: #{inspect(errors)}")

      socket =
        socket
        |> update_form_fields(params)
        |> assign(errors: errors)

      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("prev_step", _params, socket) do
    Logger.info("🔙 Previous step event - current: #{socket.assigns.form_step}")
    {:noreply, assign(socket, form_step: max(1, socket.assigns.form_step - 1))}
  end

  @impl true
  def handle_event("submit_request", params, socket) do
    Logger.info("🚀 SUBMIT REQUEST EVENT TRIGGERED")
    Logger.debug("Params received: #{inspect(params)}")
    Logger.info("Current form step: #{socket.assigns.form_step}")

    socket = update_form_fields(socket, params)

    Logger.info("Form data after update:")
    Logger.info("  - name: #{socket.assigns.name}")
    Logger.info("  - family: #{socket.assigns.family}")
    Logger.info("  - bio length: #{String.length(socket.assigns.bio)}")
    Logger.info("  - expertise: #{socket.assigns.expertise}")
    Logger.info("  - years_experience: #{socket.assigns.years_experience}")
    Logger.info("  - reason length: #{String.length(socket.assigns.reason)}")

    errors = validate_all_steps(socket.assigns)
    Logger.info("Validation errors: #{inspect(errors)}")

    if map_size(errors) == 0 do
      Logger.info("✅ All validation passed - submitting to instructordb")
      socket = assign(socket, submitting: true)

      case submit_instructor_request(socket.assigns, params) do
        :ok ->
          Logger.info("✅✅✅ SUBMISSION SUCCESSFUL!")

          {:noreply,
           socket
           |> assign(submitting: false)
           |> assign(application_submitted: true)}

        {:error, reason} ->
          Logger.error("❌❌❌ SUBMISSION FAILED: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(submitting: false)
           |> put_flash(:error, "Failed to submit request: #{inspect(reason)}")}
      end
    else
      Logger.error("❌ Validation failed on submit - errors: #{inspect(errors)}")
      {:noreply, assign(socket, errors: errors, form_step: 1)}
    end
  end

  @impl true
  def handle_event(event_name, params, socket) do
    Logger.warning("⚠️ Unhandled event: #{event_name}")
    Logger.debug("Params: #{inspect(params)}")
    {:noreply, socket}
  end

  defp check_existing_status(socket) do
    user_id = socket.assigns.user_id
    Logger.info("🔍 Checking instructor status for user: #{user_id}")

    case :instructordb.is_verified_instructor(user_id) do
      true ->
        Logger.info("✅ User is already verified instructor")
        locale = socket.assigns[:locale] || "en"

        socket
        |> put_flash(:info, "You are already a verified instructor")
        |> redirect(to: "/#{locale}/ai/learning/instructor/dashboard")

      _ ->
        Logger.info("❌ User is not verified, checking pending requests")

        case :instructordb.get_pending_requests() do
          requests when is_list(requests) ->
            Logger.debug("Found #{length(requests)} pending requests")

            existing_request =
              Enum.find(requests, fn req ->
                Map.get(req, :user_id) == user_id && Map.get(req, :status) == :pending
              end)

            if existing_request do
              Logger.info("⏳ User has pending request")

              socket
              |> assign(has_pending_request: true)
              |> assign(request_date: Map.get(existing_request, :requested_at))
            else
              Logger.info("✅ No pending request found")
              assign(socket, has_pending_request: false)
            end

          _ ->
            Logger.info("❌ Could not get pending requests")
            assign(socket, has_pending_request: false)
        end
    end
  rescue
    error ->
      Logger.error("🔴 Exception checking status: #{inspect(error)}")
      assign(socket, has_pending_request: false)
  end

  defp validate_current_step(1, params) do
    Logger.debug("Validating step 1")
    errors = %{}

    errors =
      if String.trim(params["name"] || "") == "" do
        Map.put(errors, :name, "Name is required")
      else
        errors
      end

    errors =
      if String.trim(params["family"] || "") == "" do
        Map.put(errors, :family, "Family name is required")
      else
        errors
      end

    errors =
      if String.length(params["bio"] || "") < 50 do
        Map.put(errors, :bio, "Bio must be at least 50 characters")
      else
        errors
      end

    errors
  end

  defp validate_current_step(2, params) do
    Logger.debug("Validating step 2")
    errors = %{}

    errors =
      if String.trim(params["expertise"] || "") == "" do
        Map.put(errors, :expertise, "Expertise area is required")
      else
        errors
      end

    errors =
      if String.trim(params["years_experience"] || "") == "" do
        Map.put(errors, :years_experience, "Years of experience is required")
      else
        errors
      end

    errors
  end

  defp validate_current_step(3, params) do
    Logger.debug("Validating step 3")
    errors = %{}

    errors =
      if String.length(params["reason"] || "") < 100 do
        Map.put(
          errors,
          :reason,
          "Please provide at least 100 characters explaining why you want to teach"
        )
      else
        errors
      end

    errors
  end

  defp validate_all_steps(assigns) do
    Logger.debug("Validating all steps")

    %{}
    |> validate_step_1(assigns)
    |> validate_step_2(assigns)
    |> validate_step_3(assigns)
  end

  defp validate_step_1(errors, assigns) do
    errors =
      if String.trim(assigns.name) == "" do
        Map.put(errors, :name, "Name is required")
      else
        errors
      end

    errors =
      if String.trim(assigns.family) == "" do
        Map.put(errors, :family, "Family name is required")
      else
        errors
      end

    if String.length(assigns.bio) < 50 do
      Map.put(errors, :bio, "Bio must be at least 50 characters")
    else
      errors
    end
  end

  defp validate_step_2(errors, assigns) do
    errors =
      if String.trim(assigns.expertise) == "" do
        Map.put(errors, :expertise, "Expertise area is required")
      else
        errors
      end

    if String.trim(assigns.years_experience) == "" do
      Map.put(errors, :years_experience, "Years of experience is required")
    else
      errors
    end
  end

  defp validate_step_3(errors, assigns) do
    if String.length(assigns.reason) < 100 do
      Map.put(errors, :reason, "Please provide at least 100 characters")
    else
      errors
    end
  end

  defp update_form_fields(socket, params) do
    Logger.debug("Updating form fields with params: #{inspect(Map.keys(params))}")

    Enum.reduce(params, socket, fn {key, value}, acc ->
      case key do
        "name" -> assign(acc, name: value)
        "family" -> assign(acc, family: value)
        "bio" -> assign(acc, bio: value)
        "expertise" -> assign(acc, expertise: value)
        "reason" -> assign(acc, reason: value)
        "portfolio_url" -> assign(acc, portfolio_url: value)
        "linkedin_url" -> assign(acc, linkedin_url: value)
        "years_experience" -> assign(acc, years_experience: value)
        "teaching_experience" -> assign(acc, teaching_experience: value)
        "_target" -> acc
        _ -> acc
      end
    end)
  end

  defp submit_instructor_request(assigns, _params) do
    user_id = assigns.user_id
    name = assigns.name
    family = assigns.family
    reason = assigns.reason

    Logger.info("📤 Calling instructordb.request_verification")
    Logger.info("  user_id: #{user_id}")
    Logger.info("  name: #{name}")
    Logger.info("  family: #{family}")
    Logger.info("  reason length: #{String.length(reason)}")

    try do
      result = :instructordb.request_verification(user_id, name, family, reason)
      Logger.info("📥 instructordb result: #{inspect(result)}")

      case result do
        :ok ->
          Logger.info("✅ instructordb returned :ok")
          :ok

        other ->
          Logger.warning("⚠️ instructordb returned: #{inspect(other)}")
          :ok
      end
    rescue
      error ->
        Logger.error("🔴🔴🔴 Exception in submit_instructor_request!")
        Logger.error("Error: #{inspect(error)}")
        Logger.error("Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
        {:error, :submission_failed}
    end
  end
end
