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

  # def new({:user, username, password, email, following, follower, blocking, saved_posts, other_info, private, date_created, date_updated}) do
  #   struct(Account.User, %{username: username, password: password, email: email, follower: follower, blocking: blocking, following: following, saved_posts: saved_posts, other_info: other_info, date_created: date_created, date_updated: date_updated, private: private})
  # end

  schema "users" do
    field :username, :string
    field :email, :string
    field :password, :string, virtual: true
    has_many(:wallets, Mazaryn.Wallet)

    # timestamps()
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
