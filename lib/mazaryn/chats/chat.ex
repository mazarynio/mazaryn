defmodule Mazaryn.Chats.Chat do
  @moduledoc """
  `Mazaryn.Chats.Chat` maps a chat object.
  A chat can include a list of `peer_ids`, ie `Mazaryn.Accounts.User` id.
  A `title` to describe the chat.

  ### Constraints
  * unique peers
  """
  use Ecto.Schema
  import Ecto.Changeset

  @required_fields ~w(peer_ids)a
  @optional_fields ~w(title id)a
  embedded_schema do
    field(:title, :string)
    field(:type, :string)
    field(:peer_ids, {:array, :string})
    timestamps()
  end

  @doc false
  def changeset(chat, attrs) do
    attrs = %{attrs | peer_ids: Enum.map(attrs.peer_ids, &to_string(&1))}
    attrs = (attrs[:id] && %{attrs | id: to_string(attrs.id)}) || attrs

    chat
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_unique_peers()
  end

  @doc false
  def erl_changeset({:chat, _id, _title, _peer_ids, _type, _inserted_at, _updated_at} = record) do
    params = params(record)

    %__MODULE__{}
    |> changeset(params)
    |> apply_action(:insert)
  end

  defp params(record) when is_tuple(record) do
    keys = [:id, :title, :peer_ids, :type, :inserted_at, :updated_at]

    record
    |> Tuple.to_list()
    |> tl()
    |> then(&Enum.zip(keys, &1))
    |> Enum.map(fn
      {k, :undefined} -> {k, nil}
      {k, v} -> {k, v}
    end)
    |> Map.new()
  end

  defp validate_unique_peers(changeset) do
    peer_ids = get_field(changeset, :peer_ids) || []

    peer_ids
    |> Enum.uniq()
    |> case do
      [_] -> add_error(changeset, :peer_ids, "You can't chat with self")
      [_, _] -> put_change(changeset, :type, "p2p")
      [_ | _] -> put_change(changeset, :type, "group")
      _ -> changeset
    end
  end
end
