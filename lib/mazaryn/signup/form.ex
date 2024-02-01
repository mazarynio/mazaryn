defmodule Mazaryn.Signup.Form do
  @moduledoc """
  This module defines the Ecto schema and functions for handling signup forms.
  """
  use Ecto.Schema

  import Ecto.Changeset

  alias Account.Users
  require Logger

  @password_message "Password must be between 8 and 20 characters"

  schema "signup_form" do
    field(:username, :string)
    field(:email, :string)
    field(:password, :string)
    field(:password_confirmation, :string)
    field(:accepts_conditions, :boolean)
    field(:username_touched, :boolean)
    field(:email_touched, :boolean)
    field(:password_touched, :boolean)
    field(:form_submitted, :boolean)
    field(:form_disabled, :boolean)
  end

  @required_attrs [
    :username,
    :email,
    :password,
    :password_confirmation,
    :accepts_conditions
  ]

  @optional_attrs [
    :username_touched,
    :email_touched,
    :password_touched,
    :form_submitted,
    :form_disabled
  ]

  def changeset(user, params \\ %{}) do
    user
    |> cast(params, @required_attrs ++ @optional_attrs)
    |> validate_required(:username, message: "Username is required")
    |> validate_required(:email, message: "Email address is required")
    |> validate_required(:password, message: "Password is required")
    |> validate_confirmation(:password, message: "Does not match password")
    |> validate_acceptance(:accepts_conditions, message: "Please agree with terms of service")
    |> validate_format(:email, ~r/@/)
    |> validate_length(:password, min: 8, max: 20, message: @password_message)
  end

  def create_user(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def create_user(%Ecto.Changeset{} = changeset) do
    username = changeset |> Ecto.Changeset.get_field(:username)
    email = changeset |> Ecto.Changeset.get_field(:email)
    password = changeset |> Ecto.Changeset.get_field(:password)

    case Core.UserClient.register(username, password, email) do
      :username_and_email_existed ->
        :username_and_email_existed

      id ->
        Users.one_by_id(id)
    end
  end
end
