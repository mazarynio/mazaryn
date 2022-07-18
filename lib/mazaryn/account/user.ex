defmodule Account.User do
  @moduledoc """
  User Struct
  """
  use Ecto.Schema

  import Ecto.Changeset

  # defstruct username: nil,
  #   email: nil,
  #   following: [],
  #   follower: [],
  #   blocking: [],
  #   saved_posts: [],
  #   other_info: [],
  #   private: nil,
  #   date_created: nil,
  #   date_updated: nil,
  #   password: nil

  def new_posts(posts) do
    for post <- posts, do: Home.Post.new(post)
  end

  def new(users) when is_list(users) do
    for user <- users, do: new(user)
  end

  def new({:user, username, password, email, following, follower, blocked, saved_posts, other_info, private, date_created, date_updated}) do
    struct(Account.User, %{
      username: username,
      password: password,
      email: email,
      follower: Account.User.new(follower),
      blocked: Account.User.new(blocked),
      following: Account.User.new(following),
      saved_posts: new_posts(saved_posts),
      posts: new_posts([]),
      other_info: other_info,
      location: "Rio de Janeiro",
      date_created: date_created,
      date_updated: date_updated,
      private: private,
      role: "Bitcoin Design Contributor | UI/UX Designer"})
  end

  @primary_key {:username, :string, []}
  @derive {Phoenix.Param, key: :username}
  schema "users" do
    field :email, :string
    field :password, :string, virtual: true
    field :other_info, :string
    field :location, :string
    field :birthday, :date
    field :role, :string
    field :private, :boolean
    field :date_created, :utc_datetime
    field :date_updated, :utc_datetime
    has_many(:wallets, Mazaryn.Wallet)
    has_many(:posts, Home.Post)
    has_many(:following, Account.User)
    has_many(:follower, Account.User)
    has_many(:blocked, Account.User)
    has_many(:saved_posts, Home.Post)
  end

  @required_attrs [
    :username,
    :email,
    :password
  ]

  def changeset(user, params \\ %{}) do
    user
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
    |> validate_format(:email, ~r/@/)
    |> validate_length(:password, min: 8, max: 20, message: "Password must be between 8 and 20 characters")
    |> create_password_hash()
  end

  def create_password_hash(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def create_password_hash(%Ecto.Changeset{} = changeset) do
    password_hash =
      changeset
      |> Ecto.Changeset.get_field(:password)
      |> :erlpass.hash()

    changeset
    |> Ecto.Changeset.put_change(:password, password_hash)
  end
end
