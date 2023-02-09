defmodule Mazaryn.Schema.Comment do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Comment
  """

  use Ecto.Schema

  import Ecto.Changeset

  @optional_fields ~w(
    id
    date_created
  )a

  @required_fields ~w(
    content
    author
    post_id
  )a

  embedded_schema do
    field(:content, :string)
    field(:date_created, :date)

    field(:author, :string)
    field(:post_id, :string)
  end

  def erl_changeset({:comment, id, post, author, content, date_created}) do
    %__MODULE__{}
    |> change(%{
      id: id,
      post_id: post,
      author: author,
      content: content,
      date_created: handle_datetime(date_created)
    })
  end

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  def changeset(%__MODULE__{} = struct, attrs \\ %{}) do
    struct
    |> cast(attrs, @optional_fields ++ @required_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
