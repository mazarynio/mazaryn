defmodule Home.Post do

  use Ecto.Schema

  import Ecto.Changeset

  schema "posts" do
    field :body, :string
    field(:likes_count, :integer, default: 0)
    field :gif_url, :string
    field :removed, :boolean, default: false
    field :pinned, :boolean, default: false
    field :profile_tags, {:array, :string}, default: []
    has_many(:likes, Home.Like)
    has_many(:comments, Home.Comment)
    belongs_to(:user, Account.User)

    # timestamps()
  end

  @required_attrs [
    :body
  ]

  def changeset(post, params \\  %{}) do
    post
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
  end
end
