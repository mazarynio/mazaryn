defmodule Account.Users do
  @moduledoc """
  User API
  """

  alias Core.UserClient
  alias Mazaryn.Mailer
  require Logger


  def register(username, pass, email) do
    IO.inspect(UserClient.register(username, pass, email), label: 4343434)

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
end
