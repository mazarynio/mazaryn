defmodule MazarynWeb.ConfirmAccountController do
  use MazarynWeb, :controller

  def index(conn, %{"token" => token}) do
    case validate_user(String.to_integer(token)) do
      {:ok, _user} ->
        conn
        |> put_flash(:info, "Verified account")
        |> redirect(to: "/")

      _ ->
        conn
        |> put_flash(:error, "The verification link is invalid")
        |> redirect(to: "/")
    end
  end

  def index(conn, _params) do
    conn
    |> put_flash(:error, "The verification link is invalid.")
    |> redirect(to: "/")
  end

  defp validate_user(token_id) do
    token_id
    |> Core.UserClient.validate()
    |> Account.User.erl_changeset()
    |> Account.User.build()
  end
end
