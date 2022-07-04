defmodule MazarynWeb.Schema do
  use Absinthe.Schema
  import_types MazarynWeb.Schema.User

  query do
    @desc "Get all users"
    field :users, list_of(:user) do
      resolve &Account.UserResolver.all/2
    end
  end
end
