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
    Logger.debug("Signup mount - session_uuid: #{key}")

    {:ok,
     socket
     |> assign_new(:key, fn -> key end)
     |> assign_new(:changeset, fn ->
       Signup.Form.changeset(%Signup.Form{}) |> Map.put(:action, :insert)
     end)}
  end

  @impl true
  def handle_params(_params, _uri, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("save", %{"form" => params}, socket) do
    Logger.info(
      "Signup form submitted - username: #{params["username"]}, email: #{params["email"]}"
    )

    if Map.get(params, "form_disabled", nil) != "true" do
      changeset =
        Signup.Form.changeset(%Signup.Form{}, params)
        |> Ecto.Changeset.put_change(:form_submitted, true)
        |> Ecto.Changeset.put_change(:form_disabled, true)
        |> Map.put(:action, :insert)

      Logger.debug("Sending :create_user message to self()")
      send(self(), {:create_user, changeset})

      {:noreply, assign(socket, changeset: changeset)}
    else
      Logger.warn("Form submission rejected - form_disabled was true")
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

    Logger.info("Creating user - username: #{username}, email: #{email}")
    Logger.debug("Changeset valid?: #{changeset.valid?}")

    if not changeset.valid? do
      Logger.error("Changeset invalid - errors: #{inspect(changeset.errors)}")
    end

    Logger.debug("Calling Users.register/3...")
    start_time = System.monotonic_time(:millisecond)

    result = Users.register(username, password, email)

    elapsed = System.monotonic_time(:millisecond) - start_time
    Logger.debug("Users.register completed in #{elapsed}ms - result: #{inspect(result)}")

    case result do
      {:ok, user_id} ->
        Logger.info("✓ User created successfully - user_id: #{user_id}, email: #{email}")

        try do
          Logger.debug("Sending welcome notification...")
          NotifEvent.welcome(user_id)
          Logger.debug("✓ Welcome notification sent")
        rescue
          error ->
            Logger.warn("✗ Failed to send welcome notification: #{inspect(error)}")

            Logger.debug(
              "Welcome notification stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}"
            )
        end

        Logger.debug("Inserting session token for key: #{key}")
        insert_session_token(key, email)

        Logger.debug("Scheduling redirect to approve page")
        Process.send_after(self(), {:redirect_to_approve, locale}, 200)

        {:noreply,
         socket
         |> put_flash(
           :info,
           "Account created successfully! Please check your email to verify your account."
         )
         |> assign(:user_created, true)
         |> assign(:changeset, changeset)}

      :username_and_email_existed ->
        Logger.info(
          "✗ Registration failed - user already exists (username: #{username}, email: #{email})"
        )

        changeset =
          changeset
          |> Ecto.Changeset.add_error(
            :email,
            "This account already exists. Please login instead."
          )
          |> Ecto.Changeset.put_change(:form_disabled, false)

        {:noreply, assign(socket, changeset: changeset)}

      {:error, reason} ->
        Logger.error("✗ User registration FAILED - username: #{username}, email: #{email}")

        try do
          Logger.error("Error reason type: #{inspect(reason.__struct__)}")
        rescue
          _ -> Logger.error("Error reason: #{inspect(reason)}")
        end

        Logger.error("Full error details: #{inspect(reason, pretty: true, limit: :infinity)}")

        error_message =
          case reason do
            :timeout ->
              Logger.error("Error type: timeout")
              "Registration timed out. Please try again."

            :email_delivery_failed ->
              Logger.error("Error type: email_delivery_failed")
              "Account created but email verification couldn't be sent. Please contact support."

            :token_setup_failed ->
              Logger.error("Error type: token_setup_failed")
              "Account created but verification setup failed. Please contact support."

            :unexpected_error ->
              Logger.error(
                "Error type: unexpected_error (THIS IS THE GENERIC ERROR - check Users.register function)"
              )

              "Unable to create account. Please try again."

            %Ecto.Changeset{} = cs ->
              Logger.error("Error type: Ecto.Changeset")
              Logger.error("Changeset errors: #{inspect(cs.errors)}")

              errors =
                Enum.map(cs.errors, fn {field, {msg, _}} -> "#{field}: #{msg}" end)
                |> Enum.join(", ")

              "Validation failed: #{errors}"

            other when is_atom(other) ->
              Logger.error("Error type: atom - #{inspect(other)}")
              "Unable to create account. Please try again."

            other ->
              Logger.error("Error type: unknown - #{inspect(other)}")
              "Unable to create account. Please try again."
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
    Logger.info(
      "Redirect triggered - locale: #{locale}, user_created: #{Map.get(socket.assigns, :user_created, false)}"
    )

    if Map.get(socket.assigns, :user_created, false) do
      Logger.info("✓ Redirecting to approve page")
      {:noreply, push_navigate(socket, to: ~p"/#{locale}/approve")}
    else
      Logger.warn("✗ Redirect aborted - user not created, redirecting to login instead")
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
