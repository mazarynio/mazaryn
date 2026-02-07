defmodule MazarynWeb.AuthLive.ResetPassword do
  use MazarynWeb, :live_view
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"token" => token}, _session, socket) do
    Logger.info("Reset password page loaded with token: #{String.slice(token, 0..10)}...")

    case Users.verify_reset_token(token) do
      {:ok, user_id} ->
        Logger.info("✓ Valid reset token for user: #{user_id}")
        {:ok, assign(socket, token: token, user_id: user_id, password: "", password_confirmation: "", error: nil, success: false)}

      {:error, :invalid_token} ->
        Logger.warning("✗ Invalid reset token")
        {:ok, assign(socket, token: nil, user_id: nil, password: "", password_confirmation: "", error: "Invalid or expired reset link", success: false)}

      {:error, reason} ->
        Logger.error("✗ Token verification failed: #{inspect(reason)}")
        {:ok, assign(socket, token: nil, user_id: nil, password: "", password_confirmation: "", error: "An error occurred", success: false)}
    end
  end

  @impl true
  def handle_event("reset_password", %{"password" => password, "password_confirmation" => password_confirmation}, socket) do
    Logger.info("Password reset submission for user: #{socket.assigns.user_id}")

    cond do
      is_nil(socket.assigns.token) ->
        {:noreply, assign(socket, error: "Invalid reset link")}

      String.length(password) < 8 ->
        {:noreply, assign(socket, error: "Password must be at least 8 characters")}

      String.length(password) > 20 ->
        {:noreply, assign(socket, error: "Password must be at most 20 characters")}

      password != password_confirmation ->
        {:noreply, assign(socket, error: "Passwords do not match")}

      true ->
        case Users.reset_password_with_token(socket.assigns.token, password) do
          {:ok, :password_reset} ->
            Logger.info("✓ Password reset successful for user: #{socket.assigns.user_id}")
            Process.send_after(self(), {:redirect_to_login, socket.assigns.locale}, 2000)
            {:noreply, assign(socket, success: true, error: nil)}

          {:error, :invalid_token} ->
            Logger.warning("✗ Invalid token during password reset")
            {:noreply, assign(socket, error: "Invalid or expired reset link")}

          {:error, reason} ->
            Logger.error("✗ Password reset failed: #{inspect(reason)}")
            {:noreply, assign(socket, error: "Failed to reset password. Please try again.")}
        end
    end
  end

  @impl true
  def handle_event("validate", %{"password" => password, "password_confirmation" => password_confirmation}, socket) do
    {:noreply, assign(socket, password: password, password_confirmation: password_confirmation)}
  end

  @impl true
  def handle_info({:redirect_to_login, locale}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{locale}/login")}
  end
end
