defmodule MazarynWeb.HomeLive.Approve do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info(user_id: user_id)
    {:ok, do_mount(user_id, socket)}
  end

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    user_email = get_user_email_from_session(session_uuid)
    {:ok, do_mount(user_email, socket)}
  end

  @impl true
  def handle_event("resend_verification", _params, %{assigns: %{user: user}} = socket) do
    if Application.get_env(:mazaryn, :email)[:send_emails] do
      verification_token = Account.User.generate_verification_token()

      case Core.UserClient.set_verification_token(user.id, verification_token) do
        :ok ->
          verification_url = MazarynWeb.Router.Helpers.email_verification_url(MazarynWeb.Endpoint, :verify, verification_token)

          case Account.UserNotifier.deliver_verification_email(%{email: user.email, username: user.username}, verification_url) do
            {:ok, _} ->
              {:noreply, put_flash(socket, :info, "Verification email sent successfully. Please check your inbox.")}
            {:error, _reason} ->
              {:noreply, put_flash(socket, :error, "Failed to send verification email. Please try again later.")}
          end

        {:error, _reason} ->
          {:noreply, put_flash(socket, :error, "Failed to generate verification token. Please try again later.")}
      end
    else
      {:noreply, put_flash(socket, :info, "Email sending is disabled in development mode.")}
    end
  end

  defp do_mount(email, socket) when is_binary(email) do
    case Users.one_by_email(email) do
      {:ok, user} ->
        is_local = System.get_env("PHX_HOST") != "mazaryn.io"

        socket
        |> assign(search: "")
        |> assign(user: user)
        |> assign(is_local_env: is_local)

      {:error, _reason} ->
        socket
        |> put_flash(:error, "User not found")
        |> assign(search: "")
        |> assign(user: nil)
        |> assign(is_local_env: true)
    end
  end

  defp do_mount(email, socket) do
    do_mount(to_string(email), socket)
  end

  defp get_user_email_from_session(session_uuid) do
    case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
      [{_, token}] ->
        token
        |> Users.verify_token()
      _ ->
        nil
    end
  end
end
