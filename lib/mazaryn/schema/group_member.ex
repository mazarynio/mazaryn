defmodule Mazaryn.Schema.GroupMember do
  use Ecto.Schema
  import Ecto.Changeset

  @fields ~w(
    id group_id user_id role permissions join_date invited_by muted notifications
    last_read_message_id data
  )a

  embedded_schema do
    field :group_id, :string
    field :user_id, :string
    field :role, :string, default: "member"
    field :permissions, {:array, :string}, default: []
    field :join_date, :utc_datetime
    field :invited_by, :string
    field :muted, :boolean, default: false
    field :notifications, :boolean, default: true
    field :last_read_message_id, :string
    field :data, :map, default: %{}
  end

  def erl_changeset(
        {:group_member, id, group_id, user_id, role, permissions, join_date, invited_by,
         muted, notifications, last_read_message_id, data}
      ) do
    params = %{
      id: id,
      group_id: group_id,
      user_id: user_id,
      role: role,
      permissions: permissions,
      join_date: to_naive(join_date),
      invited_by: invited_by,
      muted: muted,
      notifications: notifications,
      last_read_message_id: last_read_message_id,
      data: data
    }

    %__MODULE__{}
    |> cast(params, @fields)
  end

  def erl_changeset(_), do: {:error, :invalid_group_member_record}

  defp to_naive(:undefined), do: nil
  defp to_naive(datetime), do: Timex.to_naive_datetime(datetime)
end
