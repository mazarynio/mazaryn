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

  def create_post(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def create_post(%Ecto.Changeset{} = changeset) do
    author = changeset |> Ecto.Changeset.get_field(:author)
    content = changeset |> Ecto.Changeset.get_field(:content)

    case Core.PostClient.create_post(author, content) do
      :ok ->
        %Post{author: author, content: content}

      :post_existed ->
        :post_existed

      other ->
        other
    end
  end
end
