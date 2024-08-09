defmodule Account.UserNotifier do
  import Swoosh.Email

  alias Mazaryn.Mailer

  @support_email "support@mazaryn.io"
  @noreply_email "noreply@mazaryn.io"

  # Delivers the email using the application mailer.
  defp deliver(recipient, subject, body) do
    email =
      new()
      |> to(recipient)
      |> from({"Mazaryn", @noreply_email})
      |> reply_to(@support_email)
      |> subject(subject)
      |> text_body(body)

    Mailer.deliver(email)
  end

  @doc """
  Deliver instructions to confirm account.
  """
  def deliver_confirmation_instructions(user, url) do
    deliver(user.email, "Confirmation instructions", """
    ==============================
    Hi #{user.email},
    You can confirm your account by visiting the URL below:

    <a href="#{url}">#{url}</a>

    If you didn't create an account with us, please ignore this.
    ==============================
    """)
  end

  @doc """
  Deliver instructions to reset a user password.
  """
  def deliver_reset_password_instructions(user, url) do
    deliver(user.email, "Reset password instructions", """
    ==============================
    Hi #{user.email},
    You can reset your password by visiting the URL below:
    #{url}
    If you didn't request this change, please ignore this.
    ==============================
    """)
  end

  @doc """
  Deliver instructions to update a user email.
  """
  def deliver_update_email_instructions(user, url) do
    deliver(user.email, "Update email instructions", """
    ==============================
    Hi #{user.email},
    You can change your email by visiting the URL below:
    #{url}
    If you didn't request this change, please ignore this.
    ==============================
    """)
  end
end
