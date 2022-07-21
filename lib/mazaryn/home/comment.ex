defmodule Home.Comment do
  use Ecto.Schema
  import Ecto.Changeset

  schema "comments" do
    field(:body, :string)
    field(:total_likes, :integer, default: 0)
    field(:profile_tags, {:array, :string}, default: [])
    field(:removed, :boolean, default: false)
    belongs_to(:post, Home.Post)
    belongs_to(:user, Account.User)
    has_many(:likes, Home.CommentLike)
    has_many(:notifications, Home.Notification)

    # timestamps()
  end

  @required_attrs [
    :body
  ]

  def changeset(comment, params \\ %{}) do
    comment
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
    |> validate_length(:body, max: 500)
  end
end
