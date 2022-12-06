defmodule MazarynWeb.Schema do
  use Absinthe.Schema
  import_types(MazarynWeb.Schema.User)
  import_types(MazarynWeb.Schema.Post)
  import_types(MazarynWeb.Schema.HederaWallet)

  alias Resolvers.PostResolver
  alias Resolvers.UserResolver
  alias Resolvers.WalletResolvers

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
    field :wallets, list_of(:hedera_wallet) do
      resolve(&Resolvers.WalletResolvers.all/2)
    end
  end
end
