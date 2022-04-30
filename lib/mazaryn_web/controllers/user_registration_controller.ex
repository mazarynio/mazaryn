defmodule MazarynWeb.UserRegistrationController do
  use MazarynWeb, :controller

  alias Core.UserClient

  def show(conn, %{"username" => username}) do
    case UserClient.get_user!(username) do
      {:error, msg} ->
        render(conn, "error.json", message: msg)
      {:ok, user} ->
        render(conn, "show.json", user: user)
    end
  end
end
