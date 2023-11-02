defmodule MazarynWeb.Schema do
  use Absinthe.Schema
  import_types(MazarynWeb.Schema.User)
  import_types(MazarynWeb.Schema.Post)
  import_types(MazarynWeb.Schema.HederaWallet)

  alias Resolvers.PostResolver
  alias Resolvers.UserResolver
  alias Resolvers.WalletResolver

  query do
    @desc "Get all users"
    field :users, list_of(:user) do
      resolve(&Resolvers.UserResolver.all/2)
    end

    @desc "Create a User"
    field :create_user, list_of(:user) do
      resolve(&Resolvers.UserResolver.create_user/3)
    end

    @desc "User Login"
    field :user_login, list_of(:user_login) do
      resolve(&Resolvers.UserResolver.user_login/2)
    end

    @desc "find user by id"
    field :find_user_by_id, :find_user_by_id do
      arg(:id, non_null(:id))
      resolve(&Resolvers.UserResolver.find_user_by_id/2)
    end

    @desc "find user by email"
    field :find_user_by_email, :find_user_by_email do
      arg(:email, non_null(:string))
      resolve(&Resolvers.UserResolver.find_user_by_email/2)
    end

    @desc "find user by username"
    field :find_user_username, :find_user_by_username do
      arg(:username, non_null(:string))
      resolve(&Resolvers.UserResolver.find_user_by_username/2)
    end

    @desc "Get all posts"
    field :posts, list_of(:post) do
      resolve(&Resolvers.PostResolver.all/2)
    end

    @desc "find post by id"
    field :find_post_by_id, :find_post_by_id do
      arg(:id, non_null(:id))
      resolve(&Resolvers.PostResolver.find_post_by_id/2)
    end

    @desc "find post by author"
    field :find_post_by_author, list_of(:post) do
      arg(:author, non_null(:string))
      resolve(&Resolvers.PostResolver.find_post_by_author/2)
    end

    @desc "find post by hashtag"
    field :find_post_by_hashtag, list_of(:post) do
      arg(:hashtag, non_null(:string))
      resolve(&Resolvers.PostResolver.find_post_by_hashtag/2)
    end

    @desc "Create a Wallet"
    field :create_wallet, list_of(:hedera_wallet) do
      resolve(&Resolvers.WalletResolver.create_wallet/1)
    end

    @desc "Get all wallets"
    field :wallets, list_of(:hedera_wallet) do
      resolve(&Resolvers.WalletResolver.all/2)
    end
  end
end
