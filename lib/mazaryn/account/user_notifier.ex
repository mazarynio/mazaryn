defmodule Account.UserNotifier do
  import Swoosh.Email

  alias Mazaryn.Mailer

  @support_email "support@mazaryn.io"
  @noreply_email "noreply@mazaryn.io"
  @admin_email   "admin@mazaryn.io"

  # Delivers the email using the application mailer.
  defp deliver(recipient, subject, text_body, html_body \\ nil) do
    email =
      new()
      |> to(recipient)
      |> from({"Mazaryn", @admin_email})
      |> subject(subject)
      |> text_body(text_body)

    email =
      if html_body do
        email |> html_body(html_body)
      else
        email
      end

    case Mailer.deliver(email) do
      {:ok, _} -> {:ok, :email_sent}
      {:error, reason} ->
        Logger.error("Failed to send email: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Deliver instructions to confirm account.
  """
  def deliver_confirmation_instructions(user, url) do
    text_body = """
    Hi #{user.email},
    You can confirm your account by visiting the URL below:
    #{url}
    If you didn't create an account with us, please ignore this.
    """

    html_body = """
    <p>Hi #{user.email},</p>
    <p>You can confirm your account by visiting the URL below:</p>
    <p><a href="#{url}">Confirm my account</a></p>
    <p>If you didn't create an account with us, please ignore this.</p>
    """

    case deliver(user.email, "Confirmation instructions", text_body, html_body) do
      {:ok, _} -> {:ok, :email_sent}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Deliver instructions to reset a user password.
  """
  def deliver_reset_password_instructions(user, url) do
    text_body = """
    Hi #{user.email},
    You can reset your password by visiting the URL below:
    #{url}
    If you didn't request this change, please ignore this.
    """

    html_body = """
    <p>Hi #{user.email},</p>
    <p>You can reset your password by visiting the URL below:</p>
    <p><a href="#{url}">Reset my password</a></p>
    <p>If you didn't request this change, please ignore this.</p>
    """

    deliver(user.email, "Reset password instructions", text_body, html_body)
  end

  @doc """
  Deliver instructions to update a user email.
  """
  def deliver_update_email_instructions(user, url) do
    text_body = """
    Hi #{user.email},
    You can change your email by visiting the URL below:
    #{url}
    If you didn't request this change, please ignore this.
    """

    html_body = """
    <p>Hi #{user.email},</p>
    <p>You can change your email by visiting the URL below:</p>
    <p><a href="#{url}">Update my email</a></p>
    <p>If you didn't request this change, please ignore this.</p>
    """

    deliver(user.email, "Update email instructions", text_body, html_body)
  end
end
