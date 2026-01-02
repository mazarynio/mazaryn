defmodule Mazaryn.Schema.GroupMessage do
  use Ecto.Schema
  import Ecto.Changeset

  @fields ~w(
    id group_id user_id content media message_type reply_to mentions reactions
    reaction_counts edited deleted pinned date_created date_updated data
  )a

  embedded_schema do
    field :group_id, :string
    field :user_id, :string
    field :content, :string
    field :media, {:array, :string}, default: []
    field :message_type, :string, default: "text"
    field :reply_to, :string
    field :mentions, {:array, :string}, default: []
    field :reactions, :map, default: %{
      like: [],
      celebrate: [],
      support: [],
      love: [],
      insightful: [],
      funny: []
    }
    field :reaction_counts, :map, default: %{
      like: 0,
      celebrate: 0,
      support: 0,
      love: 0,
      insightful: 0,
      funny: 0
    }
    field :edited, :boolean, default: false
    field :deleted, :boolean, default: false
    field :pinned, :boolean, default: false
    field :date_created, :utc_datetime
    field :date_updated, :utc_datetime
    field :data, :map, default: %{}
  end

  def erl_changeset(
        {:group_message, id, group_id, user_id, content, media, message_type, reply_to,
         mentions, reactions, reaction_counts, edited, deleted, pinned, date_created,
         date_updated, data}
      ) do
    params = %{
      id: id,
      group_id: group_id,
      user_id: user_id,
      content: content,
      media: media,
      message_type: message_type,
      reply_to: reply_to,
      mentions: mentions,
      reactions: reactions,
      reaction_counts: reaction_counts,
      edited: edited,
      deleted: deleted,
      pinned: pinned,
      date_created: to_naive(date_created),
      date_updated: to_naive(date_updated),
      data: data
    }

    %__MODULE__{}
    |> cast(params, @fields)
  end

  def erl_changeset(_), do: {:error, :invalid_group_message_record}

  defp to_naive(:undefined), do: nil
  defp to_naive(datetime), do: Timex.to_naive_datetime(datetime)
end
