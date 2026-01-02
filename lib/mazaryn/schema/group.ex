defmodule Mazaryn.Schema.Group do
  use Ecto.Schema
  import Ecto.Changeset

  @fields ~w(
    id unique_name name description type privacy owner_id avatar_url banner_url
    admins members pending_invites banned_users messages pinned_messages
    settings tags category member_count max_members date_created date_updated
    last_activity data
  )a

  embedded_schema do
    field :unique_name, :string
    field :name, :string
    field :description, :string
    field :type, :string, default: "group"
    field :privacy, :string, default: "public"
    field :owner_id, :string
    field :avatar_url, :string
    field :banner_url, :string
    field :admins, {:array, :string}, default: []
    field :members, {:array, :string}, default: []
    field :pending_invites, {:array, :string}, default: []
    field :banned_users, {:array, :string}, default: []
    field :messages, {:array, :string}, default: []
    field :pinned_messages, {:array, :string}, default: []
    field :settings, :map, default: %{
      allow_member_invite: false,
      message_history_visible: true,
      approval_required: false,
      mute_all: false
    }
    field :tags, {:array, :string}, default: []
    field :category, :string
    field :member_count, :integer, default: 0
    field :max_members, :integer
    field :date_created, :utc_datetime
    field :date_updated, :utc_datetime
    field :last_activity, :utc_datetime
    field :data, :map, default: %{}
  end

  def erl_changeset(
        {:group, id, unique_name, name, description, type, privacy, owner_id, avatar_url,
         banner_url, admins, members, pending_invites, banned_users, messages,
         pinned_messages, settings, tags, category, member_count, max_members,
         date_created, date_updated, last_activity, data}
      ) do
    params = %{
      id: id,
      unique_name: unique_name,
      name: name,
      description: description,
      type: type,
      privacy: privacy,
      owner_id: owner_id,
      avatar_url: avatar_url,
      banner_url: banner_url,
      admins: admins,
      members: members,
      pending_invites: pending_invites,
      banned_users: banned_users,
      messages: messages,
      pinned_messages: pinned_messages,
      settings: settings,
      tags: tags,
      category: category,
      member_count: member_count,
      max_members: max_members,
      date_created: to_naive(date_created),
      date_updated: to_naive(date_updated),
      last_activity: to_naive(last_activity),
      data: data
    }

    %__MODULE__{}
    |> cast(params, @fields)
  end

  def erl_changeset(_), do: {:error, :invalid_group_record}

  defp to_naive(:undefined), do: nil
  defp to_naive(datetime), do: Timex.to_naive_datetime(datetime)
end
