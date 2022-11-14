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

  def insert_avatar(username, avatar_url) do
    username
    |> Core.UserClient.insert_avatar(avatar_url)
    |> User.erl_changeset()
    |> User.build()
  end

  def insert_banner(username, banner_url) do
    username
    |> Core.UserClient.insert_banner(banner_url)
    |> User.erl_changeset()
    |> User.build()
  end

  @spec one_by_username(keyword) :: %User{} | nil
  def one_by_username(username) do
    case Core.UserClient.get_user(username) do
      :not_exist ->
        Logger.error("[Users] Failed to find #{username}")
        :ok

      erl_user ->
        erl_user
        |> User.erl_changeset()
        |> User.build()
    end
  end

  def one_by_email(email) do
    case Core.UserClient.get_user_by_email(email) do
      :user_not_exist ->
        Logger.error("[Users] Failed to find #{email}")
        {:error, :user_not_exist}

      erl_user ->
        erl_user
        |> User.erl_changeset()
        |> User.build()
    end
  end

  def one_by_id(id) do
    case Core.UserClient.get_user_by_id(id) do
      :user_not_exist ->
        Logger.error("[Users] Failed to find #{id}")
        nil

      erl_user ->
        erl_user
        |> User.erl_changeset()
        |> User.build()
    end
  end

  def list() do
    Core.UserClient.get_all()
  end

  def register(username, pass, email) do
    case UserClient.register(username, pass, email) do
      user_id when is_binary(user_id) ->
        username
        |> Mail.UserEmail.register_email(email)
        |> Mailer.deliver()

      res ->
        Logger.error("[Users] Failed to register user #{username}: #{res}")
        {:error, res}
    end
  end

  def login(username, pass) do
    case UserClient.login(username, pass) do
      :logged_in ->
        {:ok, :logged_in}

      res ->
        Logger.error("[Users] Failed to login #{username}: #{res}")
        {:error, res}
    end
  end

  def follow(follower, following) do
    case UserClient.follow(follower, following) do
      {:atomic, :ok} ->
        :ok

      res ->
        Logger.error("[Users] Failed to follow #{follower} -> #{following}")
        {:error, res}
    end
  end

  def get_following(id) do
    case UserClient.get_following(id) do
      :not_exist ->
        Logger.error("[Users] Failed to find followings #{id}")
        :ok

      erl_following ->
        erl_following
    end
  end

  def reset_password(%User{} = _user) do
    {:ok, :reseted}
  end
end
