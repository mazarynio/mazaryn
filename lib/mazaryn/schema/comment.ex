defmodule Mazaryn.Schema.Comment do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Comment
  """

  use Ecto.Schema

  import Ecto.Changeset

  @optional_fields ~w(
    id
    user_id
    content_status
    date_created
    likes
    replies
    ipns
    data
    like_comment_event
  )a

  @required_fields ~w(
    content
    author
    post_id
  )a

  embedded_schema do
    field(:user_id, :string)
    field(:content, :string)
    field(:content_status, :string)
    field(:date_created, :date)
    field(:likes, :string)
    field(:replies, :string)
    field(:ipns, :string)
    field(:author, :string)
    field(:post_id, :string)
    field(:data, :map)
    field(:like_comment_event, :string)  # Add this field
  end

  def erl_changeset({:comment, id, user_id, post, author, content, content_status, date_created, likes, replies, ipns, data}) do
    %__MODULE__{}
    |> change(%{
      id: id,
      user_id: user_id,
      post_id: post,
      author: author,
      content: content,
      content_status: content_status,
      date_created: handle_datetime(date_created),
      likes: likes,
      replies: replies,
      ipns: ipns,
      data: data,
      like_comment_event: nil
    })
  end

  def erl_changeset(_), do: %{}

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  def changeset(%__MODULE__{} = struct, attrs \\ %{}) do
    struct
    |> cast(attrs, @optional_fields ++ @required_fields)
    |> validate_required(@required_fields)
  end

  def update_changeset(%__MODULE__{} = struct, attrs \\ %{}) do
    struct
    |> cast(attrs, [:id, :content, :like_comment_event])
    |> validate_required([:id, :content])
  end

  def build(map) when map == %{}, do: %{}

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
