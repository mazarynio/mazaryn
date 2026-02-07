defmodule MazarynWeb.AuthLive.Reset do
  use MazarynWeb, :live_view
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, email: "", error: nil, success: false)}
  end

  @impl true
  def handle_event("reset", %{"email" => email}, socket) do
    Logger.info("Password reset requested for email: #{email}")

    case Users.request_password_reset(email) do
      {:ok, :email_sent} ->
        Logger.info("✓ Password reset email sent to #{email}")
        {:noreply, assign(socket, success: true, error: nil, email: email)}

      {:error, :user_not_found} ->
        Logger.warning("✗ Password reset failed - user not found: #{email}")
        {:noreply, assign(socket, error: "No account found with this email address", success: false)}

      {:error, :email_send_failed} ->
        Logger.error("✗ Failed to send password reset email to #{email}")
        {:noreply, assign(socket, error: "Failed to send reset email. Please try again later.", success: false)}

      {:error, reason} ->
        Logger.error("✗ Password reset failed for #{email}: #{inspect(reason)}")
        {:noreply, assign(socket, error: "An error occurred. Please try again.", success: false)}
    end
  end

  @impl true
  def handle_event("validate", %{"email" => email}, socket) do
    {:noreply, assign(socket, email: email)}
  end
end
