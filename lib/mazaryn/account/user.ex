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
    posts
  end

  def new(users) when is_list(users) do
    for user <- users, do: new(user)
  end

  def new(
        {_user, username, password, email, phone, country, saved_posts, following, follower, blocked, other_info,
         _other, private, date_created, date_updated}
      ) do
    struct(Account.User, %{
      username: username,
      password: password,
      email: email,
      phone: phone,
      country: country,
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
      role: "Bitcoin Design Contributor | UI/UX Designer"
    })
  end

  @derive {Phoenix.Param, key: :username}
  schema "users" do
    field(:username, :string)
    field(:email, :string)
    field(:password, :string, virtual: true)
    field(:phone, :integer)
    field(:country, :string)
    field(:birthday, :date)
    field(:private, :boolean)
    field(:avatar_url, :string)
    field(:bio, :string)
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
    field(:followers_count, :integer, default: 0)
    field(:following_count, :integer, default: 0)
    field(:posts_count, :integer, default: 0)
    has_many(:wallets, Mazaryn.Wallet)
    has_many(:posts, Home.Post)
    has_many(:likes, Home.Like)
    has_many(:comments, Home.Comment)
    has_many(:following, Account.User)
    has_many(:follower, Account.User)
    has_many(:blocked, Account.User)
    has_many(:saved_posts, Home.Post)
    has_many(:notifications, Home.Notification)
  end

  @required_attrs [
    :username,
    :email,
    :password,
    :phone,
    :country
  ]

  def changeset(user, params \\ %{}) do
    user
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
    |> validate_format(:email, ~r/@/)
    |> validate_length(:password,
      min: 8,
      max: 20,
      message: "Password must be between 8 and 20 characters"
    )
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
