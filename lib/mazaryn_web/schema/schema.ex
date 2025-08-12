defmodule MazarynWeb.Schema do
  use Absinthe.Schema

  import_types(MazarynWeb.Schema.User)
  import_types(MazarynWeb.Schema.Post)
  import_types(MazarynWeb.Schema.HederaWallet)

  alias Resolvers.PostResolver
  alias Resolvers.UserResolver

  object :success_response do
    field(:success, :boolean)
    field(:message, :string)
  end

  mutation do
    # User mutations
    @desc "Create a user (simple)"
    field :create_user, :user do
      arg(:username, non_null(:string))
      arg(:password, non_null(:string))
      arg(:email, non_null(:string))
      resolve(&UserResolver.create_user/3)
    end

    @desc "Sign up a user (with validation)"
    field :signup_user, :user do
      arg(:input, non_null(:signup_input))
      resolve(&UserResolver.signup_user/3)
    end

    # Post mutations
    @desc "Create a post"
    field :create_post, :post do
      arg(:input, non_null(:post_input))
      resolve(&PostResolver.create/3)
    end

    @desc "Update a post"
    field :update_post, :post do
      arg(:input, non_null(:update_post_input))
      resolve(&PostResolver.update_post/3)
    end

    @desc "Delete a post"
    field :delete_post, :success_response do
      arg(:id, non_null(:string))
      resolve(&PostResolver.delete_post/3)
    end

    # Comment mutations
    @desc "Create a comment"
    field :create_comment, :comment do
      arg(:input, non_null(:comment_input))
      resolve(&PostResolver.create_comment/3)
    end

    @desc "Update a comment"
    field :update_comment, :comment do
      arg(:input, non_null(:update_comment_input))
      resolve(&PostResolver.update_comment/3)
    end

    @desc "Delete a comment"
    field :delete_comment, :success_response do
      arg(:comment_id, non_null(:string))
      arg(:post_id, non_null(:string))
      resolve(&PostResolver.delete_comment/3)
    end

    # Reply mutations
    @desc "Create a reply to a comment"
    field :create_reply, :reply do
      arg(:input, non_null(:reply_input))
      resolve(&PostResolver.create_reply/3)
    end

    @desc "Delete a reply"
    field :delete_reply, :success_response do
      arg(:id, non_null(:string))
      resolve(&PostResolver.delete_reply/3)
    end

    # Like mutations
    @desc "Like a post"
    field :like_post, :success_response do
      arg(:user_id, non_null(:string))
      arg(:post_id, non_null(:string))
      resolve(&PostResolver.like_post/3)
    end

    @desc "Unlike a post"
    field :unlike_post, :success_response do
      arg(:like_id, non_null(:string))
      arg(:post_id, non_null(:string))
      resolve(&PostResolver.unlike_post/3)
    end

    @desc "Like a comment"
    field :like_comment, :success_response do
      arg(:user_id, non_null(:string))
      arg(:comment_id, non_null(:string))
      resolve(&PostResolver.like_comment/3)
    end
  end

  query do
    # User queries
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

    # Post queries
    @desc "Get all posts"
    field :posts, list_of(:post) do
      resolve(&PostResolver.all/3)
    end

    @desc "Get home feed posts"
    field :home_posts, list_of(:post) do
      resolve(&PostResolver.get_home_posts/3)
    end

    @desc "Get post by ID"
    field :post_by_id, :post do
      arg(:id, non_null(:string))
      resolve(&PostResolver.get_post_by_id/3)
    end

    @desc "Get posts by author"
    field :posts_by_author, list_of(:post) do
      arg(:author, non_null(:string))
      resolve(&PostResolver.get_posts_by_author/3)
    end

    @desc "Get posts by user ID"
    field :posts_by_user_id, list_of(:post) do
      arg(:user_id, non_null(:string))
      resolve(&PostResolver.get_posts_by_user_id/3)
    end

    @desc "Get posts by hashtag"
    field :posts_by_hashtag, list_of(:post) do
      arg(:hashtag, non_null(:string))
      resolve(&PostResolver.get_posts_by_hashtag/3)
    end

    @desc "Get post likes"
    field :post_likes, list_of(:integer) do
      arg(:post_id, non_null(:string))
      resolve(&PostResolver.get_post_likes/3)
    end

    # Comment queries
    @desc "Get comments by post ID"
    field :comments_by_post, list_of(:comment) do
      arg(:post_id, non_null(:string))
      resolve(&PostResolver.get_comments_by_post/3)
    end

    @desc "Get comment by ID"
    field :comment_by_id, :comment do
      arg(:id, non_null(:string))
      resolve(&PostResolver.get_comment_by_id/3)
    end

    @desc "Get comment likes"
    field :comment_likes, list_of(:string) do
      arg(:comment_id, non_null(:string))
      resolve(&PostResolver.get_comment_likes/3)
    end

    # Reply queries
    @desc "Get replies by comment ID"
    field :replies_by_comment, list_of(:reply) do
      arg(:comment_id, non_null(:string))
      resolve(&PostResolver.get_replies_by_comment/3)
    end

    @desc "Get reply by ID"
    field :reply_by_id, :reply do
      arg(:id, non_null(:string))
      resolve(&PostResolver.get_reply_by_id/3)
    end
  end
end
