defmodule Account.Users do
  @moduledoc """
  User API
  """

  import Ecto.Query

  alias Mazaryn.Repo
  alias Account.User
  alias Core.UserClient
  alias Mazaryn.Mailer
  require Logger

  @spec query :: Ecto.Query.t()
  defp query do
    from user in User
  end

  defp query_user_profile do
    from user in User
    #eft_join: wallet in assoc(user, :wallets),
    #left_join: post in assoc(user, :posts)
  end

  defp preload_all(query) do
    query
  end

  def one_by_email2(email) do
    from(user in query(),
    where: user.email == ^email)
    |> preload_all()
    |> Repo.one()
  end

  def user_profile_by_email(email) do
    from(user in query_user_profile(),
    where: user.email == ^email)
    |> preload_all()
    |> Repo.one()
  end

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
      :user_not_exist ->
        Logger.error("[Users] Failed to find #{email}")
        nil
      erl_user ->
        erl_user
        |> List.first()
        |> User.new()
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
      :logged_in -> {:ok, :logged_in}
      res ->
       Logger.error("[Users] Failed to login #{username}: #{res}")
       {:error, res}
    end
  end

  def follow(follower, following) do
    case UserClient.follow(follower, following) do
      {:atomic, :ok} -> :ok
      res ->
       Logger.error("[Users] Failed to follow #{follower} -> #{following}")
       {:error, res}
    end
  end

  def reset_password(%User{} = _user) do
    {:ok, :reseted}
  end
end
