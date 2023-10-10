defmodule Mazaryn.Chats do
  @moduledoc false
  alias Mazaryn.Chats.Chat
  alias Account.User
  alias Account.Users
  alias :chat_server, as: ChatDB

  def create_chat(%User{id: actor_id}, %User{id: recipient_id}, params)
      when actor_id != recipient_id do
    %Chat{}
    |> Chat.changeset(params)
    |> Ecto.Changeset.apply_action(:validate)
    |> case do
      {:ok, %{body: body}} ->
        actor_id
        |> ChatDB.send_msg(recipient_id, body)
        |> ChatDB.get_msg()
        |> Chat.erl_changeset()

      any ->
        any
    end
  end

  def create_chat(_actor, _recipient, _params), do: {:error, :invalid_chat_participants}

  def get_by_chat_id(id), do: id |> ChatDB.get_chat_by_id() |> Chat.erl_changeset()

  @spec get_chats(list) :: list
  def get_chats(ids \\ []) do
    ChatDB.list_chats()
    |> Enum.map_reduce(
      [],
      &(&1
        |> ChatDB.get_msg()
        |> Chat.erl_changeset()
        |> case do
          {:ok, chat} -> {{:ok, chat}, [chat | &2]}
          any -> {any, &2}
        end)
    )
    |> elem(1)
    |> Enum.sort_by(& &1.date_created, :desc)
    |> then(fn chats ->
      if Enum.empty?(ids), do: chats, else: Enum.filter(chats, &(to_charlist(&1.id) in ids))
    end)
    |> Enum.sort_by(&(&1.date_created), {:asc, DateTime})
  end

  ## _WTF__

  @spec get_users_with_chats(User.t()) :: list()
  def get_users_with_chats(actor) do
    actor.chat
    |> get_chats()
    |> Enum.map(&to_charlist(&1.user_id))
    |> Enum.uniq()
    |> Enum.map(&(&1 |> Users.one_by_id() |> elem(1)))
  end

  @spec get_latest_recipient(binary | User.t()) :: User.t() | nil
  def get_latest_recipient(id) when is_binary(id),
    do: id |> to_charlist() |> Users.one_by_id() |> elem(1)

  def get_latest_recipient(%User{} = actor) do
    actor
    |> get_users_with_chats()
    |> case do
      [recipient | _] -> recipient
      _ -> nil
    end
  end

  @spec get_chat_messages(User.t(), User.t()) :: list
  def get_chat_messages(%User{} = actor, %User{} = recipient) do
    actor.chat
    |> Kernel.++(recipient.chat)
    |> get_chats()
    |> Enum.filter(&(to_charlist(&1.recipient_id) in [recipient.id, actor.id]))
  end

  def get_chat_messages(_, _), do: []
end
