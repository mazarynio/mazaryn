defmodule MazarynWeb.Schema.User do
  use Absinthe.Schema.Notation

  object :user do
    field(:username, :string)
    field(:email, :string)
    field(:password, :string)
  end

  object :user_login do
    field(:email, :string)
    field(:password, :string)
  end 
end
