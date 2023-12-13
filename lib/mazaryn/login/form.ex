defmodule Mazaryn.Login.Form do
  @moduledoc """
  This module defines the Ecto schema and functions for handling signup forms.
  """
  use Ecto.Schema

  import Ecto.Changeset

  alias Account.User
  require Logger

  @password_message "Password must be between 8 and 20 characters"

  schema "login_form" do
    field(:email, :string)
    field(:password, :string)
    field(:email_touched, :boolean)
    field(:password_touched, :boolean)
    field(:form_submitted, :boolean)
    field(:form_disabled, :boolean)
  end

  @required_attrs [
    :email,
    :password
  ]

  @optional_attrs [
    :email_touched,
    :password_touched,
    :form_submitted,
    :form_disabled
  ]

  def changeset(user, params \\ %{}) do
    user
    |> cast(params, @required_attrs ++ @optional_attrs)
    |> validate_required(:email, message: "Email address is required")
    |> validate_required(:password, message: "Password is required")
    |> validate_format(:email, ~r/@/)
    |> validate_length(:password, min: 8, max: 20, message: @password_message)
  end

  def get_user_by_email(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def get_user_by_email(%Ecto.Changeset{} = changeset) do
    changeset
    |> verify_user()
    |> return_changeset(changeset)
  end

  def verify_user(%Ecto.Changeset{} = changeset) do
    email = changeset |> Ecto.Changeset.get_field(:email)
    password = changeset |> Ecto.Changeset.get_field(:password)

    case Core.UserClient.login(email, password) do
      :logged_in ->
        %User{email: email}

      :wrong_username_or_password ->
        :wrong_username_or_password

      error ->
        Logger.info(error: error)
        false
    end
  end

  def return_changeset(:wrong_username_or_password, changeset) do
    Logger.info(changeset: changeset)

    Ecto.Changeset.add_error(
      changeset,
      :password,
      "The email address or password you entered is incorrect, please try again."
    )
  end

  def return_changeset(false, changeset) do
    Ecto.Changeset.add_error(
      changeset,
      :password,
      "Error, please try again."
    )
  end

  def return_changeset(%User{} = user, _changeset), do: user
end
