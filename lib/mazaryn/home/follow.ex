defmodule Home.Follow do
  use Ecto.Schema

  alias Mazaryn.Repo


  import Ecto.Changeset
  import Ecto.Query, only: [from: 2]

  schema "follows" do
    belongs_to(:following, Account.User)
    belongs_to(:follower, Account.User)
  end

  @required_attrs [
    :following,
    :follower
  ]

  def changeset(post, params \\ %{}) do
    post
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
  end

  def add_follow(%__MODULE__{} = follow, params \\ %{}) do
    follow
    |> changeset(params)
    |> Repo.insert()
  end

  def delete_follow(%__MODULE__{} = follow) do
    follow
    |> Repo.delete()
  end

  def update_follow(%__MODULE__{} = follow, params \\ %{}) do
    follow
    |> changeset(params)
    |> Repo.update()
  end

  def all_follows do
    Repo.all(__MODULE__)
  end

  def follows_from_user(user_id) do
    query =
      from(f in __MODULE__,
        where: f.follower == ^user_id,
        select: f
      )

    Repo.all(query)
  end

  def follows_from_user_following(user_id) do
    query =
      from(f in __MODULE__,
        where: f.following == ^user_id,
        select: f
      )

    Repo.all(query)
  end

  def follows_from_user_following_followers(user_id) do
    query =
      from(f in __MODULE__,
        where: f.following == ^user_id,
        select: f
      )

    Repo.all(query)
  end

  def follow_from_following_and_follower(following, follower) do
    query =
      from(f in __MODULE__,
        where: f.following == ^following and f.follower == ^follower,
        select: f
      )

    Repo.one(query)
  end
end
