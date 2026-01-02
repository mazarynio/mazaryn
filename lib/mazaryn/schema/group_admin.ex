defmodule Mazaryn.Schema.GroupAdmin do
  use Ecto.Schema
  import Ecto.Changeset

  @fields ~w(
    id group_id user_id assigned_by permissions date_assigned data
  )a

  embedded_schema do
    field :group_id, :string
    field :user_id, :string
    field :assigned_by, :string
    field :permissions, {:array, :string}, default: [
      "manage_messages",
      "manage_members",
      "send_announcements",
      "pin_messages",
      "manage_invites"
    ]
    field :date_assigned, :utc_datetime
    field :data, :map, default: %{}
  end

  def erl_changeset(
        {:group_admin, id, group_id, user_id, assigned_by, permissions,
         date_assigned, data}
      ) do
    params = %{
      id: id,
      group_id: group_id,
      user_id: user_id,
      assigned_by: assigned_by,
      permissions: permissions,
      date_assigned: to_naive(date_assigned),
      data: data
    }

    %__MODULE__{}
    |> cast(params, @fields)
  end

  def erl_changeset(_), do: {:error, :invalid_group_admin_record}

  defp to_naive(:undefined), do: nil
  defp to_naive(datetime), do: Timex.to_naive_datetime(datetime)
end
