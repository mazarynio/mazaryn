defmodule Home.Like do
  use Ecto.Schema
  import Ecto.Changeset

  schema "likes" do
    field(:like_type, :string)
    belongs_to(:post, Home.Post)
    belongs_to(:user, Account.User)

    # timestamps()
  end

  @doc false
  def changeset(like, params \\ %{}) do
    like
    |> cast(params, [:like_type, :post_id])
    |> validate_required([:like_type, :post_id])
  end
end
