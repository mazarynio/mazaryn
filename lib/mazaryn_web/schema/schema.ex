defmodule MazarynWeb.Schema do
  use Absinthe.Schema
  import_types(MazarynWeb.Schema.User)
  import_types(MazarynWeb.Schema.Post)

  query do
    @desc "Get all users"
    field :users, list_of(:user) do
      resolve(&Resolvers.UserResolver.all/2)
    end

    @desc "Get all posts"
    field :posts, list_of(:post) do
      resolve(&Resolvers.PostResolver.all/2)
    end

    @desc "Get all wallets"
    field :wallets, list_of(:hedera_wallet)
      resolve()
  end
end
