defmodule Mail.UserEmail do
  alias Swoosh.Email

  def register_email(username, user_email) do
    assigns = Map.new(username: username)
    subject = "Welcome to Mazaryn"

    body = Mail.HTML.render_html_body(assigns, "users", "register")

    base_email()
    |> Email.to({username, user_email})
    |> Email.subject(subject)
    |> Email.html_body(body)
  end

  defp base_email do
    Email.new()
    |> Email.from(from())
  end

  def from, do: {"Mazaryn", "support@mazaryn.io"}
  def html_body(body), do: body
end
