defmodule MazarynWeb.Schema do
  use Absinthe.Schema
  import_types(MazarynWeb.Schema.User)
  import_types(MazarynWeb.Schema.Post)
  import_types(MazarynWeb.Schema.HederaWallet)

  alias Resolvers.PostResolver
  alias Resolvers.UserResolver
  alias Resolvers.WalletResolver

  mutation do
    @desc "Create a user"
    field :create_user, type: :user do
      arg(:username, non_null(:string))
      arg(:password, non_null(:string))
      arg(:email, non_null(:string))

      resolve(&UserResolver.create_user/2)
    end

    @desc "Create a post"
    field :create_post, type: :post do
      arg(:author, non_null(:string))
      arg(:content, non_null(:string))
      arg(:media, list_of(:string))
      arg(:hashtag, non_null(:string))
      arg(:mention, non_null(:string))
      arg(:link_url, non_null(:string))

      resolve(&PostResolver.create/2)
    end

    @desc "Create a wallet"
    field :create_wallet, type: :hedera_wallet do
      arg(:password, non_null(:string))

      resolve(&WalletResolver.create_wallet/1)
    end
  end

  query do
    @desc "Get all users"
    field :users, list_of(:user) do
      resolve(&UserResolver.all/2)
    end

    @desc "User Login"
    field :user_login, list_of(:user_login) do
      resolve(&UserResolver.user_login/2)
    end

    @desc "find user by id"
    field :find_user_by_id, :find_user_by_id do
      arg(:id, non_null(:id))
      resolve(&UserResolver.find_user_by_id/2)
    end

    @desc "find user by email"
    field :find_user_by_email, :find_user_by_email do
      arg(:email, non_null(:string))
      resolve(&UserResolver.find_user_by_email/2)
    end

    @desc "find user by username"
    field :find_user_username, :find_user_by_username do
      arg(:username, non_null(:string))
      resolve(&Resolvers.UserResolver.find_user_by_username/2)
    end

    @desc "Get all posts"
    field :posts, list_of(:post) do
      resolve(&PostResolver.all/2)
    end

    @desc "find post by id"
    field :find_post_by_id, :find_post_by_id do
      arg(:id, non_null(:id))
      resolve(&PostResolver.find_post_by_id/2)
    end

    @desc "find post by author"
    field :find_post_by_author, list_of(:post) do
      arg(:author, non_null(:string))
      resolve(&PostResolver.find_post_by_author/2)
    end

    @desc "find post by hashtag"
    field :find_post_by_hashtag, list_of(:post) do
      arg(:hashtag, non_null(:string))
      resolve(&PostResolver.find_post_by_hashtag/2)
    end

    @desc "Get all wallets"
    field :wallets, list_of(:hedera_wallet) do
      resolve(&WalletResolver.all/2)
    end
  end
end
