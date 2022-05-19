defmodule Mazaryn.Users do
  @moduledoc """
  User API
  """

  alias Core.UserClient
  require Logger

  def register(username, pass, email) do
    case UserClient.register(username, pass, email) do
      :error ->
        Logger.error("[Users] Failed to register user #{username}")
      _ ->
        username
        |> Mailer.UserEmail.register_email(email)
        |> Mailer.deliver()
    end
  end
end
