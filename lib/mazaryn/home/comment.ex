defmodule Home.Comment do
  use Ecto.Schema
  import Ecto.Changeset

  schema "comments" do
    field :body, :string
    field :profile_tags, {:array, :string}
    field :removed, :boolean, default: false
    belongs_to :post, Home.Post
    belongs_to :user, Account.User

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
