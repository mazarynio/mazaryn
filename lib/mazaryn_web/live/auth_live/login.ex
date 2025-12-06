defmodule MazarynWeb.AuthLive.Login do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.User
  alias Mazaryn.Login
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => key}, socket) do
    Logger.debug("Login mount - session_uuid: #{key}")

    changeset =
      Login.Form.changeset(%Login.Form{}, %{})
      |> Map.put(:action, :insert)

    {:ok, assign(socket, key: key, changeset: changeset)}
  end

  @impl true
  def handle_event(
        "save",
        %{"form" => params},
        %{assigns: %{:key => key, :locale => locale}} = socket
      ) do
    email = params["email"]
    Logger.info("Login attempt - email: #{email}")
    Logger.debug("Form params: #{inspect(Map.drop(params, ["password"]))}")

    if Map.get(params, "form_disabled", nil) != "true" do
      changeset =
        Login.Form.changeset(%Login.Form{}, params)
        |> Ecto.Changeset.put_change(:form_submitted, true)
        |> Ecto.Changeset.put_change(:form_disabled, false)
        |> Map.put(:action, :insert)

      Logger.debug("Changeset valid?: #{changeset.valid?}")

      if not changeset.valid? do
        Logger.warn("Changeset validation failed - errors: #{inspect(changeset.errors)}")
      end

      Logger.debug("Getting user by email...")
      user_result = Login.Form.get_user_by_email(changeset)

      try do
        Logger.debug("get_user_by_email result: #{inspect(user_result.__struct__)}")
      rescue
        _ -> Logger.debug("get_user_by_email result: #{inspect(user_result)}")
      end

      case user_result do
        %User{email: user_email} = user ->
          Logger.info("✓ User found in login form validation - email: #{user_email}")
          Logger.debug("Fetching full user record from database...")

          case Account.Users.one_by_email(user_email) do
            {:ok, db_user} ->
              Logger.info(
                "✓ User record retrieved - id: #{db_user.id}, verified: #{db_user.verified}"
              )

              Logger.debug(
                "User details: #{inspect(Map.take(db_user, [:id, :username, :email, :verified]))}"
              )

              is_production = System.get_env("PHX_HOST") == "mazaryn.io"

              Logger.debug(
                "Environment check - PHX_HOST: #{System.get_env("PHX_HOST")}, is_production: #{is_production}"
              )

              if is_production and not db_user.verified do
                Logger.warn(
                  "✗ Login blocked - email not verified in production (email: #{user_email})"
                )

                changeset =
                  changeset
                  |> Ecto.Changeset.add_error(
                    :email,
                    "Please verify your email address before logging in. Check your inbox for the verification link."
                  )
                  |> Ecto.Changeset.put_change(:form_disabled, false)

                {:noreply, assign(socket, changeset: changeset)}
              else
                Logger.info("✓ Login successful - inserting session token")
                Logger.debug("Session key: #{key}")

                insert_session_token(key, user_email)

                Logger.debug("User record: #{inspect(db_user)}")

                redirect_path = Routes.live_path(socket, MazarynWeb.HomeLive.Home, locale)
                Logger.info("✓ Redirecting to home - path: #{redirect_path}")

                {:noreply,
                 push_redirect(socket,
                   to: redirect_path
                 )}
              end

            {:error, reason} ->
              Logger.error("✗ Failed to retrieve user from database - email: #{user_email}")
              Logger.error("Database error: #{inspect(reason)}")

              changeset =
                changeset
                |> Ecto.Changeset.add_error(:email, "Invalid email or password")
                |> Ecto.Changeset.put_change(:form_disabled, false)

              {:noreply, assign(socket, changeset: changeset)}
          end

        changeset ->
          Logger.warn("✗ User not found or password invalid - email: #{email}")
          Logger.debug("Changeset errors: #{inspect(changeset.errors)}")

          changeset =
            changeset
            |> Ecto.Changeset.put_change(:form_disabled, false)

          {:noreply, assign(socket, changeset: changeset)}
      end
    else
      Logger.warn("Login attempt rejected - form_disabled was true")
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("save", param, socket) do
    Logger.warn("Login save handler called with unexpected params")
    Logger.debug("Unexpected params: #{inspect(param)}")
    {:noreply, socket}
  end

  @impl true
  def handle_event("validate", %{"form" => params}, socket) do
    email = params["email"]
    Logger.debug("Login form validation - email: #{email}")

    changeset = Login.Form.changeset(%Login.Form{}, params) |> Map.put(:action, :insert)

    if not changeset.valid? do
      Logger.debug("Validation errors: #{inspect(changeset.errors)}")
    end

    {:noreply, assign(socket, changeset: changeset)}
  end

  @impl true
  def handle_event("blur_email", _value, socket) do
    blur_event("email", socket)
  end

  @impl true
  def handle_event("blur_password", _value, socket) do
    blur_event("password", socket)
  end

  def blur_event(field, %{assigns: %{:changeset => changeset}} = socket) do
    changeset =
      changeset
      |> Ecto.Changeset.put_change(:"#{field}_touched", true)
      |> Map.put(:action, :insert)

    {:noreply, assign(socket, changeset: changeset)}
  end
end
