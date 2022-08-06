defmodule Home.Like do
  alias Mazaryn.Repo

  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

  schema "likes" do
    field(:like_type, :string)
    belongs_to(:post, Home.Post)
    belongs_to(:user, Account.User)

    # timestamps()
  end

  @doc false
  def changeset(like, params \\ %{}) do
    like
    |> cast(params, [:like_type, :post_id, :user])
    |> validate_required([:like_type, :post_id, :user])
  end

  def add_like(%__MODULE__{} = like, params \\ %{}) do
    like
    |> changeset(params)
    |> Repo.insert()
  end

  def delete_like(like) do
    like
    |> Repo.delete()
  end

  def update_like(%__MODULE__{} = like, params \\ %{}) do
    like
    |> changeset(params)
    |> Repo.update()
  end

  def all_likes_for_post do
    Repo.all(Like)
  end

  def likes_from_post(post_id) do
    query =
      from(l in __MODULE__,
        where: l.post_id == ^post_id
      )

    Repo.all(query)
  end

  def likes_from_user(user_id) do
    query =
      from(l in __MODULE__,
        where: l.user_id == ^user_id,
        select: l
      )

    Repo.all(query)
  end

  def like_from_post_and_user(post_id, user_id) do
    query =
      from(l in __MODULE__,
        where: l.post_id == ^post_id and l.user_id == ^user_id,
        select: l
      )

    Repo.one(query)
  end

  def like_from_post_and_like_type(post_id, like_type) do
    query =
      from(l in __MODULE__,
        where: l.post_id == ^post_id and l.like_type == ^like_type,
        select: l
      )

    Repo.one(query)
  end

  def like_from_user_and_like_type(user_id, like_type) do
    query =
      from(l in __MODULE__,
        where: l.user_id == ^user_id and l.like_type == ^like_type,
        select: l
      )

    Repo.one(query)
  end

  def like_from_post_and_user_and_like_type(post_id, user_id, like_type) do
    query =
      from(l in __MODULE__,
        where: l.post_id == ^post_id and l.user_id == ^user_id and l.like_type == ^like_type,
        select: l
      )

    Repo.one(query)
  end

  def likes_from_post_and_like_type(post_id, like_type) do
    query =
      from(l in __MODULE__,
        where: l.post_id == ^post_id and l.like_type == ^like_type,
        select: l
      )

    Repo.all(query)
  end

  def likes_from_user_and_like_type(user_id, like_type) do
    query =
      from(l in __MODULE__,
        where: l.user_id == ^user_id and l.like_type == ^like_type,
        select: l
      )

    Repo.all(query)
  end

  def likes_from_post_and_user_and_like_type(post_id, user_id, like_type) do
    query =
      from(l in __MODULE__,
        where: l.post_id == ^post_id and l.user_id == ^user_id and l.like_type == ^like_type,
        select: l
      )

    Repo.all(query)
  end
end
