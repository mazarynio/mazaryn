defmodule Mazaryn.Chats.Chat do
  @moduledoc false
  use Ecto.Schema
  import Ecto.Changeset

  @fields ~w(id ai_chat_id user_id recipient_id body media bot date_created date_updated call_id call_type call_status call_link call_start_time
  call_end_time timeout_ref data)a
  @required_fields ~w(user_id recipient_id body)a
  embedded_schema do
    field(:ai_chat_id, :string)
    field(:user_id, :string)
    field(:recipient_id, :string)
    field(:body, :string)
    field(:media, {:array, :string}, default: [])
    field(:bot, :string)
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
    field(:call_id, :string)
    field(:call_type, :string)
    field(:call_status, :string)
    field(:call_link, :string)
    field(:call_start_time, :utc_datetime)
    field(:call_end_time, :utc_datetime)
    field(:timeout_ref, :string)
    field(:data, :map)
  end

  @doc false
  def changeset(chat, attrs) do
    attrs =
      Enum.reduce(
        [:id, :ai_chat_id, :user_id, :recipient_id],
        attrs,
        &((&2[&1] && Map.put(&2, &1, to_string(&2[&1]))) || &2)
      )

    chat
    |> cast(attrs, @fields)
    |> validate_required(@required_fields)
  end

  @doc false
  def erl_changeset(
        {:chat, _id, _ai_chat_id, _user_id, _recipient_id, _body, _media, _bot, _date_created,
         _date_updated, _call_id, _call_type, _call_status, _call_link, _call_start_time, _call_end_time, _timeout_ref, _data} = record
      ) do
    params = params(record)

    %__MODULE__{}
    |> changeset(params)
    |> apply_action(:insert)
  end

  defp params(record) when is_tuple(record) do
    record
    |> Tuple.to_list()
    |> tl()
    |> then(&Enum.zip(@fields, &1))
    |> Enum.map(fn
      {k, :undefined} -> {k, nil}
      {k, v} when k in [:date_created, :date_updated] -> {k, Timex.to_naive_datetime(v)}
      {k, v} -> {k, v}
    end)
    |> Map.new()
  end
end
