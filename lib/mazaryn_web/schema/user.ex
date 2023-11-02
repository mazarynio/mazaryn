defmodule MazarynWeb.Schema.User do
  use Absinthe.Schema.Notation

  object :user do
    field(:username, :string)
    field(:email, :string)
    field(:password, :string)
  end

  object :user_login do
    field(:email, :string)
    field(:username, :string)
    field(:password, :string)
  end

  object :find_user_by_id do
    field(:username, :string)
    field(:email, :string)
    field(:password, :string)
  end

  object :find_user_by_email do
    field(:email, :string)
    field(:username, :string)
    field(:password, :string)
  end

  object :find_user_by_username do
    field(:username, :string)
    field(:email, :string)
    field(:password, :string)
  end

  object :create_user do
    field(:username, :string)
    field(:email, :string)
    field(:password, :string)
  end
end
