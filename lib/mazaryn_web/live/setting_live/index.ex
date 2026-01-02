defmodule MazarynWeb.SettingLive.Index do
  use MazarynWeb, :live_view
  alias Account.User
  alias Core.UserClient
  require Logger

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("🔧 Mounting SettingLive.Index")

    case Account.Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        socket =
          socket
          |> assign(:user, user)
          |> assign(:session_uuid, session_uuid)
          |> assign(:locale, socket.assigns[:locale] || "en")
          |> assign(:changeset_username, %{})
          |> assign(:changeset_email, %{})
          |> assign(:changeset_password, %{})
          |> assign(:username_errors, [])
          |> assign(:username_password_error, nil)
          |> assign(:email_errors, [])
          |> assign(:email_password_error, nil)
          |> assign(:password_errors, [])
          |> assign(:show_success, false)
          |> assign(:success_message, "")

        {:ok, socket}

      {:error, reason} ->
        Logger.error("❌ Failed to get user: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session not found")
         |> push_redirect(to: "/#{@locale}/login")}
    end
  end

  def handle_event("validate_username", %{"username" => username}, socket) do
    errors = validate_username_field(username)
    {:noreply, assign(socket, username_errors: errors, show_success: false)}
  end

  def handle_event("validate_username_password", %{"current_password" => _pass}, socket) do
    {:noreply, assign(socket, username_password_error: nil, show_success: false)}
  end

  def handle_event(
        "save_username",
        %{"username" => new_username, "current_password" => current_pass},
        socket
      ) do
    Logger.info("💾 Saving username: #{new_username}")

    errors = validate_username_field(new_username)

    if errors == [] do
      current_username = socket.assigns.user.username

      case UserClient.change_username(current_username, current_pass, new_username) do
        :ok ->
          Logger.info("✅ Username updated successfully")

          case Account.Users.one_by_id(socket.assigns.user.id) do
            {:ok, updated_user} ->
              {:noreply,
               socket
               |> assign(:user, updated_user)
               |> assign(:username_errors, [])
               |> assign(:username_password_error, nil)
               |> assign(:show_success, true)
               |> assign(:success_message, "Username updated successfully!")
               |> put_flash(:info, "Username updated successfully")}

            _ ->
              {:noreply,
               socket
               |> assign(:username_errors, [])
               |> assign(:username_password_error, nil)
               |> assign(:show_success, true)
               |> assign(:success_message, "Username updated successfully!")
               |> put_flash(:info, "Username updated successfully")}
          end

        {:error, reason} ->
          Logger.error("❌ Username update failed: #{inspect(reason)}")
          error_msg = parse_error_message(reason)
          {:noreply, assign(socket, username_password_error: error_msg, show_success: false)}

        _ ->
          {:noreply,
           assign(socket,
             username_password_error: "Failed to update username",
             show_success: false
           )}
      end
    else
      {:noreply, assign(socket, username_errors: errors, show_success: false)}
    end
  end

  def handle_event("validate_email", %{"email" => email}, socket) do
    errors = validate_email_field(email)
    {:noreply, assign(socket, email_errors: errors, show_success: false)}
  end

  def handle_event("validate_email_password", %{"current_password" => _pass}, socket) do
    {:noreply, assign(socket, email_password_error: nil, show_success: false)}
  end

  def handle_event(
        "save_email",
        %{"email" => new_email, "current_password" => current_pass},
        socket
      ) do
    Logger.info("💾 Saving email: #{new_email}")

    errors = validate_email_field(new_email)

    if errors == [] do
      username = socket.assigns.user.username

      case UserClient.change_mail(username, current_pass, new_email) do
        :ok ->
          Logger.info("✅ Email updated successfully")

          case Account.Users.one_by_id(socket.assigns.user.id) do
            {:ok, updated_user} ->
              {:noreply,
               socket
               |> assign(:user, updated_user)
               |> assign(:email_errors, [])
               |> assign(:email_password_error, nil)
               |> assign(:show_success, true)
               |> assign(:success_message, "Email updated successfully!")
               |> put_flash(:info, "Email updated successfully")}

            _ ->
              {:noreply,
               socket
               |> assign(:email_errors, [])
               |> assign(:email_password_error, nil)
               |> assign(:show_success, true)
               |> assign(:success_message, "Email updated successfully!")
               |> put_flash(:info, "Email updated successfully")}
          end

        {:error, reason} ->
          Logger.error("❌ Email update failed: #{inspect(reason)}")
          error_msg = parse_error_message(reason)
          {:noreply, assign(socket, email_password_error: error_msg, show_success: false)}

        _ ->
          {:noreply,
           assign(socket, email_password_error: "Failed to update email", show_success: false)}
      end
    else
      {:noreply, assign(socket, email_errors: errors, show_success: false)}
    end
  end

  def handle_event("validate_password", params, socket) do
    current = Map.get(params, "current_password", "")
    new_pass = Map.get(params, "new_password", "")
    confirm = Map.get(params, "confirm_password", "")

    errors = validate_password_fields(current, new_pass, confirm)

    {:noreply, assign(socket, password_errors: errors, show_success: false)}
  end

  def handle_event(
        "save_password",
        %{
          "current_password" => current_password,
          "new_password" => new_password,
          "confirm_password" => confirm_password
        },
        socket
      ) do
    Logger.info("💾 Saving password")

    errors = validate_password_fields(current_password, new_password, confirm_password)

    if errors == [] do
      username = socket.assigns.user.username

      case UserClient.change_pass(username, current_password, new_password) do
        :ok ->
          Logger.info("✅ Password updated successfully")

          {:noreply,
           socket
           |> assign(:password_errors, [])
           |> assign(:show_success, true)
           |> assign(:success_message, "Password updated successfully!")
           |> put_flash(:info, "Password updated successfully")}

        {:error, reason} ->
          Logger.error("❌ Password update failed: #{inspect(reason)}")
          error_msg = parse_error_message(reason)
          {:noreply, assign(socket, password_errors: [error_msg], show_success: false)}

        _ ->
          {:noreply,
           assign(socket, password_errors: ["Failed to update password"], show_success: false)}
      end
    else
      {:noreply, assign(socket, password_errors: errors, show_success: false)}
    end
  end

  defp validate_username_field(username) do
    errors = []

    errors =
      if String.length(username) < 3 do
        ["Username must be at least 3 characters" | errors]
      else
        errors
      end

    errors =
      if String.length(username) > 30 do
        ["Username must be less than 30 characters" | errors]
      else
        errors
      end

    errors =
      if !Regex.match?(~r/^[a-zA-Z0-9_]+$/, username) do
        ["Username can only contain letters, numbers, and underscores" | errors]
      else
        errors
      end

    Enum.reverse(errors)
  end

  defp validate_email_field(email) do
    errors = []

    errors =
      if !Regex.match?(~r/^[^\s@]+@[^\s@]+\.[^\s@]+$/, email) do
        ["Invalid email format" | errors]
      else
        errors
      end

    Enum.reverse(errors)
  end

  defp validate_password_fields(current, new_pass, confirm) do
    errors = []

    errors =
      if String.length(current) == 0 do
        ["Current password is required" | errors]
      else
        errors
      end

    errors =
      if String.length(new_pass) < 8 do
        ["New password must be at least 8 characters" | errors]
      else
        errors
      end

    errors =
      if String.length(new_pass) > 60 do
        ["New password must be less than 60 characters" | errors]
      else
        errors
      end

    errors =
      if new_pass != confirm do
        ["Passwords do not match" | errors]
      else
        errors
      end

    Enum.reverse(errors)
  end

  defp parse_error_message(reason) when is_atom(reason) do
    case reason do
      :wrong_password -> "Current password is incorrect"
      :wrong_username_or_password -> "Current password is incorrect"
      :user_not_exist -> "User not found"
      :username_existed -> "Username already exists"
      :email_existed -> "Email already exists"
      :email_exist -> "Email already exists"
      :username_exist -> "Username already exists"
      :timeout -> "Request timed out, please try again"
      _ -> "An error occurred: #{reason}"
    end
  end

  defp parse_error_message(reason), do: "An error occurred: #{inspect(reason)}"
end
