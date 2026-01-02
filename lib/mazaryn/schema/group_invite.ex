defmodule Mazaryn.Schema.GroupInvite do
  use Ecto.Schema
  import Ecto.Changeset

  @fields ~w(
    id group_id inviter_id invitee_id status message date_created date_responded data
  )a

  embedded_schema do
    field :group_id, :string
    field :inviter_id, :string
    field :invitee_id, :string
    field :status, :string, default: "pending"
    field :message, :string
    field :date_created, :utc_datetime
    field :date_responded, :utc_datetime
    field :data, :map, default: %{}
  end

  def erl_changeset(
        {:group_invite, id, group_id, inviter_id, invitee_id, status, message,
         date_created, date_responded, data}
      ) do
    params = %{
      id: id,
      group_id: group_id,
      inviter_id: inviter_id,
      invitee_id: invitee_id,
      status: status,
      message: message,
      date_created: to_naive(date_created),
      date_responded: to_naive(date_responded),
      data: data
    }

    %__MODULE__{}
    |> cast(params, @fields)
  end

  def erl_changeset(_), do: {:error, :invalid_group_invite_record}

  defp to_naive(:undefined), do: nil
  defp to_naive(datetime), do: Timex.to_naive_datetime(datetime)
end
