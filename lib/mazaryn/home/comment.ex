defmodule Home.Comment do
  use Ecto.Schema

  alias Mazaryn.Repo

  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

  schema "comments" do
    field(:body, :string)
    field(:profile_tags, {:array, :string}, default: [])
    field(:removed, :boolean, default: false)
    belongs_to(:post, Home.Post)
    belongs_to(:user, Account.User)
    has_many(:likes, Home.CommentLike)

    # timestamps()
  end

  @required_attrs [
    :body
  ]

  def changeset(%__MODULE__{} = comment, params \\ %{}) do
    comment
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
    |> validate_length(:body, max: 500)
  end

  def add_comment(%__MODULE__{} = comment, params \\ %{}) do
    comment
    |> changeset(params)
    |> Repo.insert()
  end

  def delete_comment(%__MODULE__{} = comment) do
    comment
    |> Repo.delete()
  end

  def update_comment(%__MODULE__{} = comment, params \\ %{}) do
    comment
    |> changeset(params)
    |> Repo.update()
  end

  def all_comments() do
    Repo.all(__MODULE__)
  end

  def comments_from_post(post_id) do
    query =
      from(c in __MODULE__,
        where: c.post == ^post_id,
        select: c
      )

    Repo.all(query)
  end

  def comments_from_user(user_id) do
    query =
      from(c in __MODULE__,
        where: c.user == ^user_id,
        select: c
      )

    Repo.all(query)
  end

  def comment_from_post_and_user(post_id, user_id) do
    query =
      from(c in __MODULE__,
        where: c.post == ^post_id and c.user == ^user_id,
        select: c
      )

    Repo.all(query)
  end
end
