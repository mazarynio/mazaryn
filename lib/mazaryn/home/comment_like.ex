defmodule Home.CommentLike do
  alias Home.{Post, Comment}
  alias Mazaryn.Repo

  use Ecto.Schema

  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

  schema "comment_likes" do
    field(:like_type, :string)
    belongs_to(:comment, Comment)
    belongs_to(:user, Account.User)

    # timestamps()
  end

  def add_comment_like(%__MODULE__{} = like, params \\ %{}) do
    like
    |> cast(params, [:like_type, :comment_id, :user])
    |> validate_required([:like_type, :comment_id, :user])
    |> Repo.insert()
  end

  def delete_comment_like(%__MODULE__{} = like) do
    like
    |> Repo.delete()
  end

  def update_comment_like(%__MODULE__{} = like, params \\ %{}) do
    like
    |> cast(params, [:like_type, :comment_id, :user])
    |> validate_required([:like_type, :comment_id, :user])
    |> Repo.update()
  end

  def comment_likes_from_comment(comment_id) do
    query =
      from(c in __MODULE__,
        where: c.comment == ^comment_id,
        select: c
      )

    Repo.all(query)
  end

  def comment_likes_from_user(user_id) do
    query =
      from(c in __MODULE__,
        where: c.user == ^user_id,
        select: c
      )

    Repo.all(query)
  end

  def comment_likes_from_comment_and_user(comment_id, user_id) do
    query =
      from(c in __MODULE__,
        where: c.comment == ^comment_id and c.user == ^user_id,
        select: c
      )

    Repo.all(query)
  end

  def all_comment_likes do
    Repo.all(__MODULE__)
  end

  def comment_likes_from_post(post_id) do
    query =
      from(
        p in Post,
        where: p.id == ^post_id,
        join: c in Comment,
        on: c.post_id == p.id,
        join: cl in __MODULE__,
        on: cl.comment == c.id,
        select: [p, c, cl]
      )

    Repo.all(query)
  end

  def comment_like_from_comment_and_user(comment_id, user_id) do
    query =
      from(c in __MODULE__,
        where: c.comment == ^comment_id and c.user == ^user_id,
        select: c
      )

    Repo.one(query)
  end

  def comment_like_from_comment_and_like_type(comment_id, like_type) do
    query =
      from(c in __MODULE__,
        where: c.comment == ^comment_id and c.like_type == ^like_type,
        select: c
      )

    Repo.one(query)
  end

  def comment_like_from_post_and_like_type(post_id, like_type) do
    query =
      from(
        p in Post,
        where: p.id == ^post_id,
        join: c in Comment,
        on: c.post_id == p.id,
        join: cl in __MODULE__,
        where: cl.comment == c.id and cl.like_type == ^like_type,
        select: [p, c, cl]
      )

    Repo.one(query)
  end

  def comment_like_from_post_and_user(post_id, user_id) do
    query =
      from(
        p in Post,
        where: p.id == ^post_id,
        join: c in Comment,
        on: c.post_id == p.id,
        join: cl in __MODULE__,
        where: cl.comment == c.id and c.user == ^user_id,
        select: [p, c, cl]
      )

    Repo.one(query)
  end

  def comment_like_from_post_and_comment_and_like_type(post_id, like_type) do
    query =
      from(
        p in Post,
        where: p.id == ^post_id,
        join: c in Comment,
        on: c.post_id == p.id,
        join: cl in __MODULE__,
        where: cl.comment == c.id and cl.like_type == ^like_type,
        select: [p, c, cl]
      )

    Repo.one(query)
  end

  def comment_like_from_post_and_comment_and_user(post_id, user_id) do
    query =
      from(
        p in Post,
        where: p.id == ^post_id,
        join: c in Comment,
        on: c.post_id == p.id,
        join: cl in __MODULE__,
        where: cl.comment == c.id and cl.user == ^user_id,
        select: [p, c, cl]
      )

    Repo.one(query)
  end

  def comment_like_from_post_and_comment_and_user_and_like_type(
        post_id,
        user_id,
        like_type
      ) do
    query =
      from(
        p in Post,
        where: p.id == ^post_id,
        join: c in Comment,
        on: c.post_id == p.id,
        join: cl in __MODULE__,
        where:
          cl.comment == c.id and cl.user == ^user_id and
            cl.like_type == ^like_type,
        select: [p, c, cl]
      )

    Repo.one(query)
  end
end
