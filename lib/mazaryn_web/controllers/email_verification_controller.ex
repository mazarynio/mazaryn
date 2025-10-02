defmodule MazarynWeb.EmailVerificationController do
  use MazarynWeb, :controller

  alias Account.Users
  require Logger

  def verify(conn, %{"token" => token, "locale" => locale}) do
    case Users.verify_email(token) do
      {:ok, _user_id} ->
        conn
        |> put_flash(:info, "Your email has been verified successfully! You can now log in.")
        |> redirect(to: ~p"/#{locale}/login")

      {:error, :invalid_token} ->
        conn
        |> put_flash(:error, "Invalid or expired verification token. Please request a new verification email.")
        |> redirect(to: ~p"/#{locale}/signup")

      {:error, reason} ->
        Logger.error("Email verification failed: #{inspect(reason)}")
        conn
        |> put_flash(:error, "Verification failed. Please try again or contact support.")
        |> redirect(to: ~p"/#{locale}/signup")
    end
  end

  def verify(conn, %{"token" => token}) do
    case Users.verify_email(token) do
      {:ok, _user_id} ->
        conn
        |> put_flash(:info, "Your email has been verified successfully! You can now log in.")
        |> redirect(to: ~p"/en/login")

      {:error, :invalid_token} ->
        conn
        |> put_flash(:error, "Invalid or expired verification token. Please request a new verification email.")
        |> redirect(to: ~p"/en/signup")

      {:error, reason} ->
        Logger.error("Email verification failed: #{inspect(reason)}")
        conn
        |> put_flash(:error, "Verification failed. Please try again or contact support.")
        |> redirect(to: ~p"/en/signup")
    end
  end
end
