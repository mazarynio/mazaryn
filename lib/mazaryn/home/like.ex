defmodule Home.Like do
  use Ecto.Schema
  import Ecto.Changeset

  schema "likes" do
    field(:like_id, :integer)
    belongs_to(:post, Home.Post)
    belongs_to(:user, Account.User)

    # timestamps()
  end
end
