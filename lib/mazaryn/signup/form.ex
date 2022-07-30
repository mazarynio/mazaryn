defmodule Mazaryn.Signup.Form do
  use Ecto.Schema

  import Ecto.Changeset

  alias Account.User
  require Logger

  @password_message "Password must be between 8 and 20 characters"

  schema "signup_form" do
    field(:username, :string)
    field(:email, :string)
    field(:phone, :integer)
    field(:country)
    field(:password, :string)
    field(:password_confirmation, :string)
    field(:accepts_conditions, :boolean)
    field(:username_touched, :boolean)
    field(:email_touched, :boolean)
    field(:phone_touched, :boolean)
    field(:country_touched, :boolean)
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
      :ok ->
        %User{username: username, email: email, password: password}

      :username_existed ->
        :username_existed

      other ->
        other
    end
  end
end
