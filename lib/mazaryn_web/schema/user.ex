defmodule MazarynWeb.Schema.User do
  use Absinthe.Schema.Notation

  object :user do
    field(:id, :string)
    field(:username, :string)
    field(:email, :string)
    field(:verified, :boolean)
    field(:private, :boolean)
    field(:level, :integer)
    field(:country, :string)
    field(:avatar_url, :string)
    field(:banner_url, :string)
    field(:address, :string)
    field(:date_created, :string)
    field(:last_activity, :string)
  end

  input_object :user_input do
    field(:username, non_null(:string))
    field(:email, non_null(:string))
    field(:password, non_null(:string))
  end

  input_object :login_input do
    field(:email, :string)
    field(:username, :string)
    field(:password, non_null(:string))
  end
end
