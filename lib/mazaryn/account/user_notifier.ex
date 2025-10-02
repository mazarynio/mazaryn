defmodule Account.UserNotifier do
  import Swoosh.Email

  alias Mazaryn.Mailer

  require Logger

  @support_email "support@mazaryn.io"
  @noreply_email "noreply@mazaryn.io"
  @admin_email   "admin@mazaryn.io"

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
  Deliver email verification instructions.
  """
  def deliver_verification_email(user, url) do
    text_body = """
    Hi #{user.username || user.email},

    Welcome to Mazaryn! Please verify your email address by clicking the link below:

    #{url}

    This verification link will expire in 24 hours.

    If you didn't create an account with us, please ignore this email.

    Best regards,
    The Mazaryn Team
    """

    html_body = """
    <div style="font-family: Arial, sans-serif; max-width: 600px; margin: 0 auto; padding: 20px;">
      <div style="text-align: center; margin-bottom: 30px;">
        <h1 style="color: #2563eb; margin-bottom: 10px;">Welcome to Mazaryn!</h1>
      </div>

      <div style="background-color: #f8fafc; padding: 30px; border-radius: 8px; border-left: 4px solid #2563eb;">
        <h2 style="color: #1e40af; margin-bottom: 20px;">Verify Your Email Address</h2>
        <p style="color: #374151; line-height: 1.6; margin-bottom: 25px;">
          Hi #{user.username || user.email},
        </p>
        <p style="color: #374151; line-height: 1.6; margin-bottom: 25px;">
          Thank you for joining Mazaryn! To complete your registration and start using your account,
          please verify your email address by clicking the button below:
        </p>

        <div style="text-align: center; margin: 30px 0;">
          <a href="#{url}"
             style="background-color: #2563eb; color: white; padding: 14px 30px; text-decoration: none;
                    border-radius: 6px; font-weight: bold; display: inline-block;">
            Verify Email Address
          </a>
        </div>

        <p style="color: #6b7280; font-size: 14px; line-height: 1.6; margin-bottom: 20px;">
          <strong>Note:</strong> This verification link will expire in 24 hours for security reasons.
        </p>

        <p style="color: #6b7280; font-size: 14px; line-height: 1.6;">
          If you can't click the button above, copy and paste this URL into your browser:
          <br>
          <span style="word-break: break-all; color: #2563eb;">#{url}</span>
        </p>
      </div>

      <div style="margin-top: 30px; text-align: center; color: #6b7280; font-size: 14px;">
        <p>If you didn't create an account with Mazaryn, please ignore this email.</p>
        <p style="margin-top: 20px;">
          Best regards,<br>
          <strong>The Mazaryn Team</strong>
        </p>
      </div>
    </div>
    """

    case deliver(user.email, "Verify your Mazaryn account", text_body, html_body) do
      {:ok, _} -> {:ok, :email_sent}
      {:error, reason} -> {:error, reason}
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
