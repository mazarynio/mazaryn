defmodule Mailer.UserEmail do
  alias Swoosh.Email

  def register_email(username, user_email) do
    #assigns = Map.new(username: username)
    subject = "Welcome to Mazaryn"

    base_email()
    |> Email.to({username, user_email})
    |> Email.subject(subject)
    |> Email.html_body("Thanks for joining Mazaryn!")
  end

  defp base_email do
    Email.new
    |> Email.from(from())
  end

  def from, do: {"Mazaryn", "support@mazaryn.com"}
  def html_body(body), do: body
end
