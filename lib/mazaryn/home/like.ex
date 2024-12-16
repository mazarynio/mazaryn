defmodule Home.Like do
  # alias Mazaryn.Repo

  use Ecto.Schema
  import Ecto.Changeset
  # import Ecto.Query, only: [from: 2]

  embedded_schema do
    field(:post_id, :string)
    field(:comment_id, :string)
    field(:user_id, :string)
    field(:date_created, :utc_datetime)
    field(:data, :map)
    # field(:like_type, :string)
    # belongs_to(:post, Home.Post)
    # belongs_to(:user, Account.User)

    # timestamps()
  end

  def erl_changeset({:like, id, post_id, comment_id, user_id, date_created, data}) do
    %__MODULE__{}
    |> change(%{
      id: id,
      post_id: post_id,
      comment_id: comment_id,
      user_id: user_id,
      date_created: handle_datetime(date_created),
      data: data
    })
  end

  def erl_changeset(value), do: raise(value)

  def build(changeset) do
    apply_action(changeset, :build)
  end

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  # @doc false
  # def changeset(like, params \\ %{}) do
  #   like
  #   |> cast(params, [:like_type, :post_id, :user])
  #   |> validate_required([:like_type, :post_id, :user])
  # end

  # def add_like(%__MODULE__{} = like, params \\ %{}) do
  #   like
  #   |> changeset(params)
  #   |> Repo.insert()
  # end

  # def delete_like(like) do
  #   like
  #   |> Repo.delete()
  # end

  # def update_like(%__MODULE__{} = like, params \\ %{}) do
  #   like
  #   |> changeset(params)
  #   |> Repo.update()
  # end

  # def all_likes_for_post do
  #   Repo.all(Like)
  # end

  # def likes_from_post(post_id) do
  #   query =
  #     from(l in __MODULE__,
  #       where: l.post_id == ^post_id
  #     )

  #   Repo.all(query)
  # end

  # def likes_from_user(user_id) do
  #   query =
  #     from(l in __MODULE__,
  #       where: l.user_id == ^user_id,
  #       select: l
  #     )

  #   Repo.all(query)
  # end

  # def like_from_post_and_user(post_id, user_id) do
  #   query =
  #     from(l in __MODULE__,
  #       where: l.post_id == ^post_id and l.user_id == ^user_id,
  #       select: l
  #     )

  #   Repo.one(query)
  # end

  # def like_from_post_and_like_type(post_id, like_type) do
  #   query =
  #     from(l in __MODULE__,
  #       where: l.post_id == ^post_id and l.like_type == ^like_type,
  #       select: l
  #     )

  #   Repo.one(query)
  # end

  # def like_from_user_and_like_type(user_id, like_type) do
  #   query =
  #     from(l in __MODULE__,
  #       where: l.user_id == ^user_id and l.like_type == ^like_type,
  #       select: l
  #     )

  #   Repo.one(query)
  # end

  # def like_from_post_and_user_and_like_type(post_id, user_id, like_type) do
  #   query =
  #     from(l in __MODULE__,
  #       where: l.post_id == ^post_id and l.user_id == ^user_id and l.like_type == ^like_type,
  #       select: l
  #     )

  #   Repo.one(query)
  # end

  # def likes_from_post_and_like_type(post_id, like_type) do
  #   query =
  #     from(l in __MODULE__,
  #       where: l.post_id == ^post_id and l.like_type == ^like_type,
  #       select: l
  #     )

  #   Repo.all(query)
  # end

  # def likes_from_user_and_like_type(user_id, like_type) do
  #   query =
  #     from(l in __MODULE__,
  #       where: l.user_id == ^user_id and l.like_type == ^like_type,
  #       select: l
  #     )

  #   Repo.all(query)
  # end

  # def likes_from_post_and_user_and_like_type(post_id, user_id, like_type) do
  #   query =
  #     from(l in __MODULE__,
  #       where: l.post_id == ^post_id and l.user_id == ^user_id and l.like_type == ^like_type,
  #       select: l
  #     )

  #   Repo.all(query)
  # end
end
