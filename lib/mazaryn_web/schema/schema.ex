defmodule MazarynWeb.Schema do
  use Absinthe.Schema
  import_types(MazarynWeb.Schema.User)
  import_types(MazarynWeb.Schema.Post)

  query do
    @desc "Get all users"
    field :users, list_of(:user) do
      resolve(&Account.UserResolver.all/2)
    end

    @desc "Get all posts"
    field :posts, list_of(:post) do
      resolve(&Post.PostResolver.all/2)
    end
  end
end
