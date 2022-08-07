defmodule Mazaryn.Schema.Comment do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Comment
  """

  use Ecto.Schema

  import Ecto.Changeset

  @optional_fields ~w(
    id
  )a

  @required_fields ~w(
    content
    author
    post
    date_created
  )a

  embedded_schema do
    field(:content, :map)
    field(:date_created, :date)

    field(:author, :string)
    field(:post, :string)
  end


  def erl_changeset({:comment, id, post, author, content, date_created}) do
    %__MODULE__{}
    |> changeset(%{
      id: id,
      post: post,
      author: author,
      content: Enum.into(content, %{}),
      date_created: date_created
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
