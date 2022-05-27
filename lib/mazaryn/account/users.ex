defmodule Account.Users do
  @moduledoc """
  User API
  """
  alias Account.User
  alias Core.UserClient
  alias Mazaryn.Mailer
  require Logger

  @spec one_by_username(keyword) :: %User{} | nil
  def one_by_username(username) do
    case Core.UserClient.getting_user(username) do
      :not_exist ->
        Logger.error("[Users] Failed to find #{username}")
        :ok
      erl_user ->
        User.new(erl_user)
    end
  end

  def one_by_email(email) do
    case Core.UserClient.get_user_by_mail(email) do
      :not_exist ->
        Logger.error("[Users] Failed to find #{email}")
        nil
      erl_user ->
        User.new(erl_user)
    end

  end

  def list() do
    Core.UserClient.getting_users()
  end


  def register(username, pass, email) do
    case UserClient.register(username, pass, email) do
      :ok ->
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
      :logged_in -> autheticate_user(username)
      res ->
       Logger.error("[Users] Failed to login #{username}: #{res}")
       {:error, res}
    end
  end

  defp autheticate_user(_username) do
    nil
  end

  def follow(follower, following) do
    case UserClient.follow(follower, following) do
      {:atomic, :ok} -> :ok
      res ->
       Logger.error("[Users] Failed to follow #{follower} -> #{following}")
       {:error, res}
    end
  end
end
