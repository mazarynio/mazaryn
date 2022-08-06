defmodule Mazaryn.Post do
  @moduledoc """
  Embedded schema to represent Mazaryn.Post
  """

  use Ecto.Schema

  import Ecto.Changeset

  @optional_fields ~w(
    id
    media
    photo_url
  )a

  @required_fields ~w(
    content
  )a

  # date_created
  embedded_schema do
    field(:content, :string)
    field(:media, {:array, :string})
    field(:date_created, :utc_datetime)
    field(:photo_url, :string)

    belongs_to(:author, Account.User)
    has_many(:comments, Mazaryn.Comment)
  end

  def erl_changeset(
        {:post, id, content, comments, media, author, other, date_created, date_updated}
      ) do
    %__MODULE__{}
    |> changeset(%{
      id: id,
      content: content,
      comments: comments,
      media: media,
      author: author,
      other: other,
      date_created: date_created,
      date_updated: date_updated
    })
  end

  def changeset(%__MODULE__{} = struct, attrs) do
    struct
    |> cast(attrs, @optional_fields ++ @required_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
