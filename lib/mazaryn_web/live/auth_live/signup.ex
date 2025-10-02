defmodule MazarynWeb.AuthLive.Signup do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper
  alias Mazaryn.Signup
  alias Account.Users
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
  def handle_info({:create_user, changeset}, %{assigns: %{locale: locale, key: key}} = socket) do
    username = Ecto.Changeset.get_field(changeset, :username)
    password = Ecto.Changeset.get_field(changeset, :password)
    email = Ecto.Changeset.get_field(changeset, :email)

    case Users.register(username, password, email) do
      {:ok, user_id} ->
        Logger.info("User created successfully: #{email}")

        try do
          NotifEvent.welcome(user_id)
        rescue
          error ->
            Logger.warn("Failed to send welcome notification: #{inspect(error)}")
        end

        insert_session_token(key, email)
        Process.send_after(self(), {:redirect_to_approve, locale}, 200)

        {:noreply,
         socket
         |> put_flash(:info, "Account created successfully! Please check your email to verify your account.")
         |> assign(:user_created, true)
         |> assign(:changeset, changeset)}

      :username_and_email_existed ->
        Logger.info("Attempted to create existing user account")

        changeset =
          changeset
          |> Ecto.Changeset.add_error(:email, "This account already exists. Please login instead.")
          |> Ecto.Changeset.put_change(:form_disabled, false)

        {:noreply, assign(socket, changeset: changeset)}

      {:error, reason} ->
        Logger.error("Failed to create user: #{inspect(reason)}")

        error_message = case reason do
          :timeout -> "Registration timed out. Please try again."
          :email_delivery_failed -> "Account created but email verification couldn't be sent. Please contact support."
          :token_setup_failed -> "Account created but verification setup failed. Please contact support."
          _ -> "Unable to create account. Please try again."
        end

        changeset =
          changeset
          |> Ecto.Changeset.add_error(:email, error_message)
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

  defp add_generic_error_if_needed(changeset) do
    if Enum.empty?(changeset.errors) do
      Ecto.Changeset.add_error(changeset, :email, "Unable to create account. Please try again.")
    else
      changeset
    end
  end
end
