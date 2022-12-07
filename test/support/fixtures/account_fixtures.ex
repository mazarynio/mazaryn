defmodule Mazaryn.AccountFixtures do
  alias Mazaryn.Signup.Form

  @moduledoc """
  This module defines test helpers for creating
  entities via the `Account.User` context.
  """

  def username, do: "username#{System.unique_integer()}"
  def unique_user_email, do: "user#{System.unique_integer()}@example.com"
  def valid_user_password, do: "password!123"

  def valid_user_attributes(attrs \\ %{}) do
    Enum.into(attrs, %{
      "username" => username(),
      "email" => unique_user_email(),
      "password" => valid_user_password()
    })
  end

  def user_fixture(attrs \\ %{}) do
    attrs = valid_user_attributes(attrs)

    {:ok, user} =
      %Account.User{}
      |> Account.User.changeset(attrs)
      |> Form.create_user()

    user
  end
end
