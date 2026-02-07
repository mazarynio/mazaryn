defmodule MazarynWeb.EmailVerificationController do
  use MazarynWeb, :controller
  alias Account.Users
  require Logger

  def verify(conn, %{"token" => token}) do
    Logger.info("Email verification attempt - token: #{String.slice(token, 0..10)}...")

    case Users.verify_email(token) do
      {:ok, user_id} ->
        Logger.info("✓ Email verified successfully for user #{user_id}")

        conn
        |> put_flash(:info, "Your email has been verified successfully! You can now log in.")
        |> redirect(to: "/#{conn.assigns.locale}/verification-success")

      {:error, :invalid_token} ->
        Logger.warning("✗ Invalid verification token")

        conn
        |> put_flash(:error, "Invalid or expired verification link. Please try signing up again.")
        |> redirect(to: "/#{conn.assigns.locale}/signup")

      {:error, reason} ->
        Logger.error("✗ Email verification failed: #{inspect(reason)}")

        conn
        |> put_flash(:error, "Verification failed. Please try again or contact support.")
        |> redirect(to: "/#{conn.assigns.locale}/signup")
    end
  end
end
