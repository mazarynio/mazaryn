defmodule Account.User do
  @moduledoc """
  Embedded schema to represent Account.User
  """
  use Ecto.Schema

  import Ecto.Changeset

  alias Mazaryn.Post
  alias Mazaryn.Posts

  @optional_fields ~w(
    id
  )a

  embedded_schema do
    field(:username, :string)
    field(:email, :string)
    field(:password, :string, virtual: true)
    field(:private, :boolean)
    field(:other_info, {:array, :string})
    field(:media, {:array, :string})
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)

    # TODO: ADD to mnesia
    field(:avatar_url, :string)
    field(:country, :string)

    embeds_many(:posts, Post)
    embeds_many(:following, Account.User)
    embeds_many(:follower, Account.User)
    embeds_many(:blocked, Account.User)
    embeds_many(:saved_posts, Post)

    embeds_many(:notifications, Home.Notification)

    # has_many(:follower, Account.User)
    # has_many(:blocked, Account.User)
    # has_many(:saved_posts, Home.Post)
    # has_many(:notifications, Home.Notification)
  end

  @required_attrs [
    :username,
    :email,
    :password
  ]

  def erl_changeset(
        {:user, username, id, password, email, media, posts, following, follower, blocked,
         saved_posts, other_info, private, date_created, date_updated}
      ) do
    %__MODULE__{}
    |> cast(
      %{
        username: username,
        id: id,
        password: password,
        email: email,
        media: media,
        posts: preload_posts(posts),
        following: following,
        follower: follower,
        blocked: blocked,
        saved_posts: saved_posts,
        other_info: other_info,
        private: private,
        date_created: date_created,
        date_updated: date_updated
      },
      @optional_fields ++ @required_attrs
    )
    |> cast_embed(:posts, required: false, with: &Post.changeset/2)
    |> validate_required(@required_attrs)
  end

  defp preload_posts(posts) do
    for post_id <- posts do
      {:ok, post} = Posts.one_by_id(post_id)
      Map.from_struct(post)
    end
  end

  def changeset(user, params \\ %{}) do
    user
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
    |> validate_format(:email, ~r/@/)
    |> validate_length(:password,
      min: 8,
      max: 60,
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

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
