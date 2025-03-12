defmodule Mail.UserEmail do
  import Swoosh.Email

  alias Mazaryn.Mailer

  @from {"Mazaryn", "admin@mazaryn.io"}

  def register_email(username, user_email) do
    assigns = %{username: username}
    subject = "Welcome to Mazaryn"

    body = Mail.HTML.render_html_body(assigns, "users", "register")

    # Build the email
    base_email()
    |> to({username, user_email})
    |> subject(subject)
    |> html_body(body)
    |> Mailer.deliver()
  end

  defp base_email do
    new()
    |> from(@from)
  end
end
