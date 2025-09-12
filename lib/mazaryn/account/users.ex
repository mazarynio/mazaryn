defmodule Account.Users do
  @moduledoc """
  User API
  """

  alias Account.User
  alias Core.UserClient
  alias Mazaryn.Mailer
  require Logger

  def signing_salt do
    salt = MazarynWeb.Endpoint.config(:live_view)[:signing_salt]

    salt ||
      raise MazarynWeb.AuthenticationError, message: "missing signing_salt"
  end

  def get_by_session_uuid(session_uuid) do
    case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
      [{_, token}] ->
        token
        |> verify_token()
        |> one_by_email()

      _ ->
        nil
    end
  end

  def verify_token(token) do
    MazarynWeb.Endpoint
    |> Phoenix.Token.verify(signing_salt(), token, max_age: 806_400)
    |> case do
      {:ok, email} -> email
      _ -> nil
    end
  end

  def insert_avatar(id, avatar_url) do
    try do
      case Core.UserClient.insert_avatar(id, avatar_url) do
        {:error, :timeout} ->
          Logger.warning("Avatar insert timed out for user #{id}")
          {:error, "Avatar upload timed out"}

        {:error, reason} ->
          Logger.error("Avatar insert failed for user #{id}: #{inspect(reason)}")
          {:error, "Failed to update avatar"}

        result ->
          case User.erl_changeset_safe(result) do
            {:ok, changeset} ->
              User.build(changeset)
            {:error, reason} ->
              Logger.error("Invalid avatar update result for user #{id}: #{reason}")
              {:error, "Invalid avatar data"}
          end
      end
    rescue
      error ->
        Logger.error("Exception in insert_avatar for user #{id}: #{inspect(error)}")
        {:error, "Avatar update failed"}
    end
  end

  def insert_banner(id, banner_url) do
    try do
      case Core.UserClient.insert_banner(id, banner_url) do
        {:error, :timeout} ->
          Logger.warning("Banner insert timed out for user #{id}")
          {:error, "Banner upload timed out"}

        {:error, reason} ->
          Logger.error("Banner insert failed for user #{id}: #{inspect(reason)}")
          {:error, "Failed to update banner"}

        result ->
          case User.erl_changeset_safe(result) do
            {:ok, changeset} ->
              User.build(changeset)
            {:error, reason} ->
              Logger.error("Invalid banner update result for user #{id}: #{reason}")
              {:error, "Invalid banner data"}
          end
      end
    rescue
      error ->
        Logger.error("Exception in insert_banner for user #{id}: #{inspect(error)}")
        {:error, "Banner update failed"}
    end
  end

  @spec one_by_username(keyword) :: %User{} | nil | {:error, atom()}
  def one_by_username(username) do
    try do
      case Core.UserClient.get_user(username) do
        :not_exist ->
          Logger.info("User not found: #{username}")
          {:error, :not_found}

        {:error, :timeout} ->
          Logger.warning("User lookup timed out for username: #{username}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("User lookup failed for username #{username}: #{inspect(reason)}")
          {:error, reason}

        erl_user ->
          case User.erl_changeset_safe(erl_user) do
            {:ok, changeset} ->
              User.build(changeset)
            {:error, reason} ->
              Logger.error("Invalid user data for username #{username}: #{reason}")
              {:error, :invalid_data}
          end
      end
    rescue
      error ->
        Logger.error("Exception in one_by_username for #{username}: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def one_by_email(email) do
    try do
      case Core.UserClient.get_user_by_email(email) do
        :user_not_exist ->
          Logger.info("User not found: #{email}")
          {:error, :user_not_exist}

        {:error, :timeout} ->
          Logger.warning("User lookup timed out for email: #{email}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("User lookup failed for email #{email}: #{inspect(reason)}")
          {:error, reason}

        erl_user ->
          case User.erl_changeset_safe(erl_user) do
            {:ok, changeset} ->
              User.build(changeset)
            {:error, reason} ->
              Logger.error("Invalid user data for email #{email}: #{reason}")
              {:error, :invalid_data}
          end
      end
    rescue
      error ->
        Logger.error("Exception in one_by_email for #{email}: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def one_by_id(id) do
    try do
      case Core.UserClient.get_user_by_id(id) do
        :user_not_exist ->
          Logger.info("User not found: #{id}")
          nil

        {:error, :timeout} ->
          Logger.warning("User lookup timed out for ID: #{id}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("User lookup failed for ID #{id}: #{inspect(reason)}")
          {:error, reason}

        erl_user ->
          case User.erl_changeset_safe(erl_user) do
            {:ok, changeset} ->
              case User.build(changeset) do
                {:ok, user} -> {:ok, user}
                {:error, reason} -> {:error, reason}
              end
            {:error, reason} ->
              Logger.error("Invalid user data for ID #{id}: #{reason}")
              {:error, :invalid_data}
          end
      end
    rescue
      error ->
        Logger.error("Exception in one_by_id for #{id}: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def list() do
    try do
      Core.UserClient.get_all()
    rescue
      error ->
        Logger.error("Exception in list(): #{inspect(error)}")
        []
    end
  end

  def register(username, pass, email) do
    try do
      case UserClient.register(username, pass, email) do
        {:error, :timeout} ->
          Logger.warning("User registration timed out for #{username}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("User registration failed for #{username}: #{inspect(reason)}")
          {:error, reason}

        user_id when is_list(user_id) ->
          case Mail.UserEmail.register_email(username, email) |> Mailer.deliver() do
            {:ok, _} ->
              {:ok, user_id}

            {:error, reason} ->
              Logger.error("Failed to send registration email to #{email}: #{inspect(reason)}")
              {:error, :email_delivery_failed}
          end

        res ->
          Logger.error("Unexpected registration result for #{username}: #{inspect(res)}")
          {:error, :registration_failed}
      end
    rescue
      error ->
        Logger.error("Exception during registration for #{username}: #{inspect(error)}")
        {:error, :registration_failed}
    end
  end

  def login(email, pass) do
    try do
      case UserClient.login(email, pass) do
        :logged_in ->
          {:ok, :logged_in}

        {:error, :timeout} ->
          Logger.warning("Login timed out for #{email}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("Login failed for #{email}: #{inspect(reason)}")
          {:error, reason}

        res ->
          Logger.error("Unexpected login result for #{email}: #{inspect(res)}")
          {:error, res}
      end
    rescue
      error ->
        Logger.error("Exception during login for #{email}: #{inspect(error)}")
        {:error, :login_failed}
    end
  end

  def follow(follower, following) do
    try do
      case UserClient.follow(follower, following) do
        {:atomic, :ok} ->
          :ok

        {:error, :timeout} ->
          Logger.warning("Follow operation timed out: #{follower} -> #{following}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("Follow failed #{follower} -> #{following}: #{inspect(reason)}")
          {:error, reason}

        res ->
          Logger.error("Unexpected follow result #{follower} -> #{following}: #{inspect(res)}")
          {:error, res}
      end
    rescue
      error ->
        Logger.error("Exception during follow #{follower} -> #{following}: #{inspect(error)}")
        {:error, :follow_failed}
    end
  end

  def get_following(id) do
    try do
      case UserClient.get_following(id) do
        :not_exist ->
          Logger.info("No followings found for user #{id}")
          []

        {:error, :timeout} ->
          Logger.warning("Get following timed out for user #{id}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("Get following failed for user #{id}: #{inspect(reason)}")
          {:error, reason}

        erl_following ->
          erl_following
      end
    rescue
      error ->
        Logger.error("Exception in get_following for user #{id}: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def reset_password(%User{} = _user) do
    {:ok, :reseted}
  end

  def list_users() do
    try do
      case UserClient.get_all() do
        {:error, reason} ->
          Logger.error("Failed to get all users: #{inspect(reason)}")
          []

        users when is_list(users) ->
          users
          |> Enum.map(fn user_id ->
            case one_by_id(user_id) do
              {:ok, user} -> user
              {:error, reason} ->
                Logger.warning("Failed to get user #{user_id}: #{inspect(reason)}")
                nil
            end
          end)
          |> Enum.filter(&(&1 != nil))

        _ ->
          Logger.error("Unexpected result from get_all users")
          []
      end
    rescue
      error ->
        Logger.error("Exception in list_users: #{inspect(error)}")
        []
    end
  end

  def create_user(username, password, email) do
    try do
      case UserClient.register(username, password, email) do
        {:error, reason} ->
          Logger.error("User creation failed for #{username}: #{inspect(reason)}")
          {:error, reason}

        user_id when is_list(user_id) ->
          one_by_id(user_id)

        other ->
          Logger.error("Unexpected user creation result for #{username}: #{inspect(other)}")
          {:error, :creation_failed}
      end
    rescue
      error ->
        Logger.error("Exception in create_user for #{username}: #{inspect(error)}")
        {:error, :creation_failed}
    end
  end

  @spec get_user_by_id(charlist()) :: map() | {:error, atom()}
  def get_user_by_id(id) do
    try do
      charlist_id = to_charlist(id)

      case Core.UserClient.get_user_by_id(charlist_id) do
        {:error, :timeout} ->
          Logger.warning("Get user by ID timed out: #{id}")
          {:error, :timeout}

        {:error, reason} ->
          Logger.error("Get user by ID failed for #{id}: #{inspect(reason)}")
          {:error, reason}

        :user_not_exist ->
          Logger.info("User not found by ID: #{id}")
          {:error, :not_found}

        erl_user ->
          case User.erl_changeset_safe(erl_user) do
            {:ok, changeset} ->
              case User.build(changeset) do
                {:ok, user} -> user
                {:error, reason} ->
                  Logger.error("Failed to build user from changeset for ID #{id}: #{inspect(reason)}")
                  {:error, :build_failed}
              end
            {:error, reason} ->
              Logger.error("Invalid user data for ID #{id}: #{reason}")
              {:error, :invalid_data}
          end
      end
    rescue
      error ->
        Logger.error("Exception in get_user_by_id for #{id}: #{inspect(error)}")
        {:error, :lookup_failed}
    catch
      :exit, reason ->
        Logger.error("Exit in get_user_by_id for #{id}: #{inspect(reason)}")
        {:error, :process_exit}
    end
  end
end
