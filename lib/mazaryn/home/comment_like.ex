defmodule Home.CommentLike do
  use Ecto.Schema

  schema "comment_likes" do
    field :like_type, :string
    belongs_to :comment, Home.Comment
    belongs_to :user, Account.User

    # timestamps()
  end
end
