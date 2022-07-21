defmodule Home.Post do
  use Ecto.Schema
  import Ecto.Changeset

  schema "posts" do
    field(:body, :string)
    field(:likes_count, :integer, default: 0)
    field(:gif_url, :string)
    field(:privacy, :string)
    field(:removed, :boolean, default: false)
    field(:pinned, :boolean, default: false)
    field(:profile_tags, {:array, :string}, default: [])
    has_many(:likes, Home.Like, on_delete: :nilify_all)
    has_many(:comments, Home.Comment, on_delete: :nilify_all)
    belongs_to(:user, Account.User)

    # timestamps()
  end

  @required_attrs [
    :body,
    :user_id,
    :privacy
  ]

  def changeset(post, params \\ %{}) do
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
