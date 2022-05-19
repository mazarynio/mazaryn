defmodule Account.Users do
  @moduledoc """
  User API
  """

  alias Core.UserClient
  alias Mazaryn.Mailer
  require Logger

  def register(username, pass, email) do
    case UserClient.register(username, pass, email) do
      {:atomic, :ok} ->
        username
        |> Mail.UserEmail.register_email(email)
        |> Mailer.deliver()
      res ->
        Logger.error("[Users] Failed to register user #{username}: #{res}")
    end
  end
end
