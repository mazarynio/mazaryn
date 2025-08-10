defmodule MazarynWeb.Schema do
  use Absinthe.Schema
  import_types(MazarynWeb.Schema.User)
  import_types(MazarynWeb.Schema.Post)
  import_types(MazarynWeb.Schema.HederaWallet)

  alias Resolvers.PostResolver
  alias Resolvers.UserResolver


  mutation do
    @desc "Create a user"
    field :create_user, :user do
      arg(:username, non_null(:string))
      arg(:password, non_null(:string))
      arg(:email, non_null(:string))

      resolve(&UserResolver.create_user/3)
    end

    @desc "Create a post"
    field :create_post, :post do
      arg(:author, non_null(:string))
      arg(:content, non_null(:string))
      arg(:media, list_of(:string))
      arg(:hashtag, non_null(:string))
      arg(:mention, non_null(:string))
      arg(:link_url, non_null(:string))

      resolve(&PostResolver.create/3)
    end

  end

  query do
    @desc "Get all users"
    field :users, list_of(:user) do
      resolve(&UserResolver.all/3)
    end

    @desc "User Login - Returns authenticated user"
    field :user_login, list_of(:user) do
      arg(:email, :string)
      arg(:username, :string)
      arg(:password, non_null(:string))
      resolve(&UserResolver.user_login/3)
    end

    @desc "Find user by id"
    field :find_user_by_id, :user do
      arg(:id, non_null(:id))
      resolve(&UserResolver.find_user_by_id/3)
    end

    @desc "Find user by email"
    field :find_user_by_email, :user do
      arg(:email, non_null(:string))
      resolve(&UserResolver.find_user_by_email/3)
    end

    @desc "Find user by username"
    field :find_user_by_username, :user do
      arg(:username, non_null(:string))
      resolve(&UserResolver.find_user_by_username/3)
    end

    @desc "Get all posts"
    field :posts, list_of(:post) do
      resolve(&PostResolver.all/3)
    end

  end
end
