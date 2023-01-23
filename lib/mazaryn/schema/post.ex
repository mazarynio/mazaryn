defmodule Mazaryn.Schema.Post do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Post
  """

  use Ecto.Schema
  alias Timex
  import Ecto.Changeset

  @optional_fields ~w(
    id
    media
    hashtag
    photo_url
    author
    other
    comments
    likes
    profile_tags
    date_created
    date_updated
  )a

  # date_created
  # date_updated
  @required_fields ~w(
    content
  )a

  # date_created
  embedded_schema do
    field(:content, :string)
    field(:media, {:array, :string}, default: [])
    field(:hashtag, :string)
    field(:author, :string)
    field(:other, {:array, :string}, default: [])
    # TODO: Add to mnesia
    field(:comments, {:array, :string}, default: [])
    field(:profile_tags, {:array, :string}, default: [])
    field(:like_count, :integer, default: 0)

    field(:photo_url, :string)
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
  end

  def erl_changeset(
        {:post, id, content, comments, likes, media, hashtag, author, other, date_created, date_updated}
      ) do
    %__MODULE__{}
    |> change(%{
      id: id,
      content: content,
      comments: comments,
      likes: likes,
      media: media,
      hashtag: hashtag,
      author: author,
      other: other,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated)
    })
  end

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  def changeset(post, attrs \\ %{}) do
    post
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
