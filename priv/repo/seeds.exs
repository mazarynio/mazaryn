# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Mazaryn.Repo.insert!(%Mazaryn.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias Mazaryn.Signup.Form

params = %{
  email: "user2@example.com",
  password: "password!123!",
  password_confirmation: "password!123!",
  accepts_conditions: true
}

Form.changeset(%Form{}, params)
|> Form.create_user()
