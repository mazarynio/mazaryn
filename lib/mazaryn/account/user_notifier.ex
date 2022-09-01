defmodule Account.UserNotifier do
  use Phoenix.Swoosh, view: Sample.UserNotifierView

  def welcome(user) do
    new()
    |> from("no-reply@mazaryn.io")
    |> to(user.email)
    |> subject("Hello, Noble!")
    |> render_body("welcome.html", %{name: name})
  end

  defp deliver(to, body) do
    require Logger
    Logger.debug(body)
    {:ok, %{to: to, body: body}}
  end

  def deliver_confirmation_instructions(user, url) do
    deliver(user.email, """
    ==============================
    Hi #{user.email},
    You can confirm your account by visiting the URL below:
    #{url}
    If you didn't create an account with us, please ignore this.
    ==============================
    """)
  end

  def deliver_reset_password_instructions(user, url) do
    deliver(user.email, """
    ==============================
    Hi #{user.email},
    You can reset your password by visiting the URL below:
    #{url}
    If you didn't request this change, please ignore this.
    ==============================
    """)
  end

  def deliver_update_email_instructions(user, url) do
    deliver(user.email, """
    ==============================
    Hi #{user.email},
    You can change your email by visiting the URL below:
    #{url}
    If you didn't request this change, please ignore this.
    ==============================
    """)
  end
end
