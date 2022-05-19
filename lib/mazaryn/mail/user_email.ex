defmodule Mail.UserEmail do
  import Swoosh.Email

  def welcome(user) do
    new()
    |> to({user.username, user.email})
    |> from({"Mazaryn", "no_reply@mazaryn.xyz"})
    |> subject("Welcome to Mazaryn SN")
    |> html_body("<h1>Hello #{user.username}</h1>")
    |> text_body("Hello #{user.username}\n")
  end
end
