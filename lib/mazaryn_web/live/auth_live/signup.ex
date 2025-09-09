defmodule MazarynWeb.AuthLive.Signup do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper
  alias Mazaryn.Signup
  alias Account.UserNotifier
  alias Core.NotifEvent
  alias MazarynWeb.Router.Helpers, as: Routes
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => key}, socket) do
    changeset =
      Signup.Form.changeset(%Signup.Form{})
      |> Map.put(:action, :insert)

    {:ok, assign(socket, key: key, changeset: changeset)}
  end

  @impl true
  def handle_event("save", %{"form" => params}, socket) do
    Logger.info("Signup form submitted with params: #{inspect(params)}")
    if Map.get(params, "form_disabled", nil) != "true" do
      changeset =
        Signup.Form.changeset(%Signup.Form{}, params)
        |> Ecto.Changeset.put_change(:form_submitted, true)
        |> Ecto.Changeset.put_change(:form_disabled, true)
        |> Map.put(:action, :insert)

      send(self(), {:create_user, changeset})

      {:noreply, assign(socket, changeset: changeset)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("validate", %{"form" => params}, socket) do
    changeset = Signup.Form.changeset(%Signup.Form{}, params) |> Map.put(:action, :insert)
    {:noreply, assign(socket, changeset: changeset)}
  end

  @impl true
  def handle_event("blur_username", _value, socket) do
    blur_event("username", socket)
  end

  @impl true
  def handle_event("blur_email", _value, socket) do
    blur_event("email", socket)
  end

  @impl true
  def handle_event("blur_password", _value, socket) do
    blur_event("password", socket)
  end

  @impl true
  def handle_event(
        "toggle",
        %{"value" => _value},
        %{assigns: %{:changeset => changeset}} = socket
      ) do
    changeset =
      changeset
      |> Ecto.Changeset.put_change(:accepts_conditions, true)
      |> Map.put(:action, :insert)

    {:noreply, assign(socket, changeset: changeset)}
  end

  @impl true
  def handle_event("toggle", _value, %{assigns: %{:changeset => changeset}} = socket) do
    changeset =
      changeset
      |> Ecto.Changeset.put_change(:accepts_conditions, false)
      |> Map.put(:action, :insert)

    {:noreply, assign(socket, changeset: changeset)}
  end

  defp blur_event(field, %{assigns: %{:changeset => changeset}} = socket) do
    changeset =
      changeset
      |> Ecto.Changeset.put_change(:"#{field}_touched", true)
      |> Map.put(:action, :insert)

    {:noreply, assign(socket, changeset: changeset)}
  end

  @impl true
  def handle_info({:create_user, changeset}, %{assigns: %{key: key, locale: locale}} = socket) do
    case Signup.Form.create_user(changeset) do
      {:ok, %Account.User{email: email, id: user_id}} ->
        Logger.info("User created successfully: #{email}")

        case create_user_session(key, email) do
          {:ok, _session} ->
            Logger.info("Session created successfully for user: #{email}")

            try do
              NotifEvent.welcome(user_id)
            rescue
              error ->
                Logger.warn("Failed to send welcome notification: #{inspect(error)}")
            end

            Process.send_after(self(), {:redirect_to_approve, locale}, 200)

            {:noreply,
             socket
             |> put_flash(:info, "Welcome! Your account has been created successfully.")
             |> assign(:user_created, true)}

          {:error, reason} ->
            Logger.error("Failed to create session for #{email}: #{inspect(reason)}")

            changeset =
              changeset
              |> Ecto.Changeset.add_error(:email, "Account created but login failed. Please try logging in.")
              |> Ecto.Changeset.put_change(:form_disabled, false)

            {:noreply, assign(socket, changeset: changeset)}
        end

      :username_and_email_existed ->
        Logger.info("Attempted to create existing user account")

        changeset =
          changeset
          |> Ecto.Changeset.add_error(:email, "This account already exists. Please login instead.")
          |> Ecto.Changeset.put_change(:form_disabled, false)

        {:noreply, assign(socket, changeset: changeset)}

      {:error, changeset} ->
        Logger.error("Failed to create user: #{inspect(changeset.errors)}")

        changeset =
          changeset
          |> Ecto.Changeset.put_change(:form_disabled, false)
          |> add_generic_error_if_needed()

        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  @impl true
  def handle_info({:redirect_to_approve, locale}, socket) do
    Logger.info("Redirecting to approve page for locale: #{locale}")

    if Map.get(socket.assigns, :user_created, false) do
      {:noreply, push_navigate(socket, to: ~p"/#{locale}/approve")}
    else
      Logger.warn("Attempted redirect without user creation confirmation")
      {:noreply, push_navigate(socket, to: ~p"/#{locale}/login")}
    end
  end

  defp create_user_session(session_key, email) do
    try do
      result = insert_session_token(session_key, email)

      case result do
        {:ok, _} = success -> success
        {:error, _} = error -> error
        other when not is_nil(other) -> {:ok, other}
        _ -> {:error, :session_creation_failed}
      end
    rescue
      error ->
        Logger.error("Session creation exception: #{inspect(error)}")
        {:error, :session_creation_exception}
    end
  end

  defp add_generic_error_if_needed(changeset) do
    if Enum.empty?(changeset.errors) do
      Ecto.Changeset.add_error(changeset, :email, "Unable to create account. Please try again.")
    else
      changeset
    end
  end
end
