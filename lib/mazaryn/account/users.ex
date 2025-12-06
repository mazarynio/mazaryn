defmodule Account.Users do
  @moduledoc """
  User API
  """

  alias Account.User
  alias Core.UserClient
  alias Mazaryn.Mailer
  alias Account.UserNotifier
  require Logger

  import Phoenix.Controller, only: [url: 2]

  def signing_salt do
    salt = MazarynWeb.Endpoint.config(:live_view)[:signing_salt]

    salt ||
      raise MazarynWeb.AuthenticationError, message: "missing signing_salt"
  end

  def get_by_session_uuid(session_uuid) do
    Logger.debug("get_by_session_uuid called - session_uuid: #{session_uuid}")

    case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
      [{_, token}] ->
        Logger.debug("✓ Token found in ETS")

        token
        |> verify_token()
        |> one_by_email()

      _ ->
        Logger.debug("✗ Token not found in ETS")
        nil
    end
  end

  def verify_token(token) do
    Logger.debug("Verifying token...")

    MazarynWeb.Endpoint
    |> Phoenix.Token.verify(signing_salt(), token, max_age: 806_400)
    |> case do
      {:ok, email} ->
        Logger.debug("✓ Token verified - email: #{email}")
        email

      error ->
        Logger.warn("✗ Token verification failed: #{inspect(error)}")
        nil
    end
  end

  def insert_avatar(id, avatar_url) do
    Logger.info("insert_avatar - id: #{id}, url: #{avatar_url}")

    try do
      case Core.UserClient.insert_avatar(id, avatar_url) do
        {:error, :timeout} ->
          Logger.warning("✗ Avatar insert timed out for user #{id}")
          {:error, "Avatar upload timed out"}

        {:error, reason} ->
          Logger.error("✗ Avatar insert failed for user #{id}: #{inspect(reason)}")
          {:error, "Failed to update avatar"}

        result ->
          Logger.debug("Avatar insert result: #{inspect(result)}")

          case User.erl_changeset_safe(result) do
            {:ok, changeset} ->
              Logger.debug("✓ Avatar changeset created")
              User.build(changeset)

            {:error, reason} ->
              Logger.error("✗ Invalid avatar update result for user #{id}: #{reason}")
              {:error, "Invalid avatar data"}
          end
      end
    rescue
      error ->
        Logger.error("✗ Exception in insert_avatar for user #{id}: #{inspect(error)}")
        {:error, "Avatar update failed"}
    end
  end

  def insert_banner(id, banner_url) do
    Logger.info("insert_banner - id: #{id}, url: #{banner_url}")

    try do
      case Core.UserClient.insert_banner(id, banner_url) do
        {:error, :timeout} ->
          Logger.warning("✗ Banner insert timed out for user #{id}")
          {:error, "Banner upload timed out"}

        {:error, reason} ->
          Logger.error("✗ Banner insert failed for user #{id}: #{inspect(reason)}")
          {:error, "Failed to update banner"}

        result ->
          Logger.debug("Banner insert result: #{inspect(result)}")

          case User.erl_changeset_safe(result) do
            {:ok, changeset} ->
              Logger.debug("✓ Banner changeset created")
              User.build(changeset)

            {:error, reason} ->
              Logger.error("✗ Invalid banner update result for user #{id}: #{reason}")
              {:error, "Invalid banner data"}
          end
      end
    rescue
      error ->
        Logger.error("✗ Exception in insert_banner for user #{id}: #{inspect(error)}")
        {:error, "Banner update failed"}
    end
  end

  @spec one_by_username(keyword) :: %User{} | nil | {:error, atom()}
  def one_by_username(username) do
    Logger.debug("one_by_username - username: #{username}")

    try do
      case Core.UserClient.get_user(username) do
        :not_exist ->
          Logger.info("✗ User not found: #{username}")
          {:error, :not_found}

        {:error, :timeout} ->
          Logger.warning("✗ User lookup timed out for username: #{username}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("✗ User lookup failed for username #{username}: #{inspect(reason)}")
          {:error, reason}

        erl_user ->
          Logger.debug("✓ User data retrieved from UserClient")

          case User.erl_changeset_safe(erl_user) do
            {:ok, changeset} ->
              Logger.debug("✓ Changeset created for #{username}")
              User.build(changeset)

            {:error, reason} ->
              Logger.error("✗ Invalid user data for username #{username}: #{reason}")
              {:error, :invalid_data}
          end
      end
    rescue
      error ->
        Logger.error("✗ Exception in one_by_username for #{username}: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def one_by_email(email) do
    Logger.debug("one_by_email - email: #{email}")

    try do
      case Core.UserClient.get_user_by_email(email) do
        :user_not_exist ->
          Logger.info("✗ User not found: #{email}")
          {:error, :user_not_exist}

        {:error, :timeout} ->
          Logger.warning("✗ User lookup timed out for email: #{email}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("✗ User lookup failed for email #{email}: #{inspect(reason)}")
          {:error, reason}

        erl_user ->
          Logger.debug("✓ User data retrieved from UserClient for #{email}")

          case User.erl_changeset_safe(erl_user) do
            {:ok, changeset} ->
              Logger.debug("✓ Changeset created for #{email}")
              User.build(changeset)

            {:error, reason} ->
              Logger.error("✗ Invalid user data for email #{email}: #{reason}")
              {:error, :invalid_data}
          end
      end
    rescue
      error ->
        Logger.error("✗ Exception in one_by_email for #{email}: #{inspect(error)}")
        Logger.error("Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
        {:error, :lookup_failed}
    end
  end

  def one_by_id(id) do
    Logger.debug("one_by_id called with: #{inspect(id)}")

    Logger.debug(
      "id type: #{if is_binary(id), do: "binary", else: if(is_list(id), do: "charlist", else: "unknown")}"
    )

    try do
      case Core.UserClient.get_user_by_id(id) do
        :user_not_exist ->
          Logger.info("✗ User not found: #{id}")
          nil

        {:error, :timeout} ->
          Logger.warning("✗ User lookup timed out for ID: #{id}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("✗ User lookup failed for ID #{id}: #{inspect(reason)}")
          {:error, reason}

        erl_user ->
          Logger.debug("✓ Got erl_user tuple for id #{id}")

          case User.erl_changeset_safe(erl_user) do
            {:ok, changeset} ->
              Logger.debug("✓ Changeset created successfully for id #{id}")
              Logger.debug("Changeset valid?: #{changeset.valid?}")

              case User.build(changeset) do
                {:ok, user} ->
                  Logger.debug("✓ User built successfully - username: #{user.username}")
                  {:ok, user}

                {:error, reason} ->
                  Logger.error("✗ Failed to build user for id #{id}: #{inspect(reason)}")
                  {:error, reason}
              end

            {:error, reason} ->
              Logger.error("✗ Invalid user data for ID #{id}: #{reason}")
              {:error, :invalid_data}
          end
      end
    rescue
      error ->
        Logger.error("✗ Exception in one_by_id for #{id}: #{inspect(error)}")
        Logger.error("Stacktrace: #{inspect(__STACKTRACE__)}")
        {:error, :lookup_failed}
    end
  end

  def list() do
    Logger.debug("list() called")

    try do
      Core.UserClient.get_all()
    rescue
      error ->
        Logger.error("✗ Exception in list(): #{inspect(error)}")
        []
    end
  end

  def register(username, pass, email) do
    Logger.info("═══════════════════════════════════════════════════════════")
    Logger.info("Account.Users.register called")
    Logger.info("  username: #{username}")
    Logger.info("  email: #{email}")
    Logger.info("  password length: #{String.length(pass)}")
    Logger.info("═══════════════════════════════════════════════════════════")

    try do
      Logger.debug("Step 1: Calling Core.UserClient.register...")
      start_time = System.monotonic_time(:millisecond)

      result = UserClient.register(username, pass, email)

      elapsed = System.monotonic_time(:millisecond) - start_time
      Logger.debug("UserClient.register completed in #{elapsed}ms")
      Logger.debug("Result type: #{inspect(result)}")

      case result do
        {:error, :timeout} ->
          Logger.error("✗✗✗ User registration TIMED OUT for #{username}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("✗✗✗ User registration FAILED for #{username}")
          Logger.error("Error reason: #{inspect(reason)}")
          {:error, reason}

        user_id when is_list(user_id) ->
          Logger.info("✓ UserClient.register succeeded - user_id: #{user_id}")

          Logger.debug("Step 2: Generating verification token...")
          verification_token = User.generate_verification_token()
          Logger.debug("✓ Token generated: #{String.slice(verification_token, 0..10)}...")

          Logger.debug("Step 3: Setting verification token in database...")

          case Core.UserClient.set_verification_token(user_id, verification_token) do
            :ok ->
              Logger.debug("✓ Verification token set successfully")

              send_emails = Application.get_env(:mazaryn, :email)[:send_emails]
              Logger.debug("Step 4: Email sending enabled? #{send_emails}")

              if send_emails do
                Logger.debug("Step 5: Preparing verification email...")

                host = Application.get_env(:mazaryn, MazarynWeb.Endpoint)[:url][:host]
                Logger.debug("Host from config: #{host}")

                verification_url = "https://#{host}/en/verify-email/#{verification_token}"
                Logger.info("Verification URL: #{verification_url}")

                Logger.debug("Step 6: Sending email via UserNotifier...")

                email_result =
                  UserNotifier.deliver_verification_email(
                    %{email: email, username: username},
                    verification_url
                  )

                Logger.debug("Email delivery result: #{inspect(email_result)}")

                case email_result do
                  {:ok, _} ->
                    Logger.info("✓✓✓ Verification email sent successfully to #{email}")
                    {:ok, user_id}

                  {:error, reason} ->
                    Logger.error("✗ Failed to send verification email to #{email}")
                    Logger.error("Email error: #{inspect(reason)}")
                    Logger.warn("User created but email not sent - returning success anyway")
                    {:ok, user_id}
                end
              else
                Logger.info("Email sending disabled in config - skipping verification email")
                {:ok, user_id}
              end

            {:error, reason} ->
              Logger.error("✗✗✗ Failed to set verification token for #{username}")
              Logger.error("Token setup error: #{inspect(reason)}")
              {:error, :token_setup_failed}
          end

        res ->
          Logger.error("✗✗✗ Unexpected registration result for #{username}")
          Logger.error("Result was: #{inspect(res)}")
          Logger.error("Expected: user_id (charlist) or {:error, reason}")
          {:error, :registration_failed}
      end
    rescue
      error ->
        Logger.error("✗✗✗ EXCEPTION during registration for #{username}")

        try do
          Logger.error("Exception type: #{inspect(error.__struct__)}")
        rescue
          _ -> Logger.error("Exception: #{inspect(error)}")
        end

        Logger.error("Exception details: #{inspect(error, pretty: true, limit: :infinity)}")
        Logger.error("Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
        {:error, :registration_failed}
    end
  end

  def verify_email(token) do
    Logger.info("verify_email called - token: #{String.slice(token, 0..10)}...")

    try do
      case Core.UserClient.verify_email_token(token) do
        {:ok, user_id} ->
          Logger.info("✓ Email verified successfully for user #{user_id}")
          {:ok, user_id}

        {:error, :token_not_found} ->
          Logger.warning("✗ Invalid verification token")
          {:error, :invalid_token}

        {:error, reason} ->
          Logger.error("✗ Email verification failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("✗ Exception during email verification: #{inspect(error)}")
        {:error, :verification_failed}
    end
  end

  def login(email, pass) do
    Logger.info("login attempt - email: #{email}")

    try do
      Logger.debug("Step 1: Looking up user by email...")

      case one_by_email(email) do
        {:ok, user} ->
          Logger.info("✓ User found - id: #{user.id}, verified: #{user.verified}")

          is_production = System.get_env("PHX_HOST") == "mazaryn.io"
          Logger.debug("Environment: #{if is_production, do: "PRODUCTION", else: "DEVELOPMENT"}")

          if is_production do
            if user.verified do
              Logger.debug("Step 2: User is verified, proceeding with login...")

              case UserClient.login(email, pass) do
                :logged_in ->
                  Logger.info("✓✓✓ Login successful for #{email}")
                  {:ok, :logged_in}

                {:error, :timeout} ->
                  Logger.warning("✗ Login timed out for #{email}")
                  {:error, :timeout}

                {:error, reason} ->
                  Logger.error("✗ Login failed for #{email}: #{inspect(reason)}")
                  {:error, reason}

                res ->
                  Logger.error("✗ Unexpected login result for #{email}: #{inspect(res)}")
                  {:error, res}
              end
            else
              Logger.warning("✗ Login blocked - account not verified: #{email}")
              {:error, :email_not_verified}
            end
          else
            Logger.debug("Development mode - skipping verification check")

            case UserClient.login(email, pass) do
              :logged_in ->
                Logger.info("✓✓✓ Login successful for #{email}")
                {:ok, :logged_in}

              {:error, :timeout} ->
                Logger.warning("✗ Login timed out for #{email}")
                {:error, :timeout}

              {:error, reason} ->
                Logger.error("✗ Login failed for #{email}: #{inspect(reason)}")
                {:error, reason}

              res ->
                Logger.error("✗ Unexpected login result for #{email}: #{inspect(res)}")
                {:error, res}
            end
          end

        {:error, :user_not_exist} ->
          Logger.info("✗ Login attempt for non-existent user: #{email}")
          {:error, :invalid_credentials}

        {:error, reason} ->
          Logger.error("✗ User lookup failed during login for #{email}: #{inspect(reason)}")
          {:error, :login_failed}
      end
    rescue
      error ->
        Logger.error("✗ Exception during login for #{email}: #{inspect(error)}")
        Logger.error("Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
        {:error, :login_failed}
    end
  end

  def follow(follower, following) do
    Logger.info("follow - follower: #{follower}, following: #{following}")

    try do
      case UserClient.follow(follower, following) do
        {:atomic, :ok} ->
          Logger.debug("✓ Follow successful")
          :ok

        {:error, :timeout} ->
          Logger.warning("✗ Follow operation timed out")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("✗ Follow failed: #{inspect(reason)}")
          {:error, reason}

        res ->
          Logger.error("✗ Unexpected follow result: #{inspect(res)}")
          {:error, res}
      end
    rescue
      error ->
        Logger.error("✗ Exception during follow: #{inspect(error)}")
        {:error, :follow_failed}
    end
  end

  def get_following(id) do
    Logger.debug("get_following - id: #{id}")

    try do
      case UserClient.get_following(id) do
        :not_exist ->
          Logger.info("No followings found for user #{id}")
          []

        {:error, :timeout} ->
          Logger.warning("✗ Get following timed out for user #{id}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("✗ Get following failed for user #{id}: #{inspect(reason)}")
          {:error, reason}

        erl_following ->
          Logger.debug("✓ Following list retrieved for #{id}")
          erl_following
      end
    rescue
      error ->
        Logger.error("✗ Exception in get_following for user #{id}: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def reset_password(%User{} = _user) do
    {:ok, :reseted}
  end

  def list_users() do
    Logger.debug("list_users called")

    try do
      case UserClient.get_all() do
        {:error, reason} ->
          Logger.error("✗ Failed to get all users: #{inspect(reason)}")
          []

        users when is_list(users) ->
          Logger.debug("✓ Got #{length(users)} user IDs")

          users
          |> Enum.map(fn user_id ->
            case one_by_id(user_id) do
              {:ok, user} ->
                user

              {:error, reason} ->
                Logger.warning("Failed to get user #{user_id}: #{inspect(reason)}")
                nil
            end
          end)
          |> Enum.filter(&(&1 != nil))

        _ ->
          Logger.error("✗ Unexpected result from get_all users")
          []
      end
    rescue
      error ->
        Logger.error("✗ Exception in list_users: #{inspect(error)}")
        []
    end
  end

  def create_user(username, password, email) do
    Logger.info("create_user - username: #{username}, email: #{email}")

    try do
      case UserClient.register(username, password, email) do
        {:error, reason} ->
          Logger.error("✗ User creation failed for #{username}: #{inspect(reason)}")
          {:error, reason}

        user_id when is_list(user_id) ->
          Logger.info("✓ User created - user_id: #{user_id}")
          one_by_id(user_id)

        other ->
          Logger.error("✗ Unexpected user creation result for #{username}: #{inspect(other)}")
          {:error, :creation_failed}
      end
    rescue
      error ->
        Logger.error("✗ Exception in create_user for #{username}: #{inspect(error)}")
        {:error, :creation_failed}
    end
  end

  @spec get_user_by_id(charlist()) :: map() | {:error, atom()}
  def get_user_by_id(id) do
    Logger.debug("get_user_by_id - id: #{id}")

    try do
      charlist_id = to_charlist(id)

      case Core.UserClient.get_user_by_id(charlist_id) do
        {:error, :timeout} ->
          Logger.warning("✗ Get user by ID timed out: #{id}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("✗ Get user by ID failed for #{id}: #{inspect(reason)}")
          {:error, reason}

        :user_not_exist ->
          Logger.info("✗ User not found by ID: #{id}")
          {:error, :not_found}

        erl_user ->
          Logger.debug("✓ User data retrieved for #{id}")

          case User.erl_changeset_safe(erl_user) do
            {:ok, changeset} ->
              case User.build(changeset) do
                {:ok, user} ->
                  Logger.debug("✓ User built successfully")
                  user

                {:error, reason} ->
                  Logger.error("✗ Failed to build user from changeset: #{inspect(reason)}")
                  {:error, :build_failed}
              end

            {:error, reason} ->
              Logger.error("✗ Invalid user data for ID #{id}: #{reason}")
              {:error, :invalid_data}
          end
      end
    rescue
      error ->
        Logger.error("✗ Exception in get_user_by_id for #{id}: #{inspect(error)}")
        {:error, :lookup_failed}
    catch
      :exit, reason ->
        Logger.error("✗ Exit in get_user_by_id for #{id}: #{inspect(reason)}")
        {:error, :process_exit}
    end
  end
end
