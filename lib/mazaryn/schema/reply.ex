defmodule Mazaryn.Schema.Reply do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Reply
  """

  use Ecto.Schema

  import Ecto.Changeset

  @optional_fields ~w(
    id
    comment
    chat
    date_created
    data
  )a

  @required_fields ~w(
    content
    user_id
  )a


  embedded_schema do
    field(:content, :string)
    field(:date_created, :date)
    field(:comment, :string)
    field(:chat, :string)
    field(:user_id, :string)
    field(:data, :map)
  end

  def erl_changeset({:reply, id, comment, chat, user_id, content, date_created, data}) do
    %__MODULE__{}
    |> change(%{
      id: id,
      comment: comment,
      chat: chat,
      user_id: user_id,
      content: content,
      date_created: handle_datetime(date_created),
      data: data
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
    |> cast(attrs, [:id, :content])
    |> validate_required([:id, :content])
  end

  def build(map) when map == %{}, do: %{}

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
