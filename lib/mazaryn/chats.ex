defmodule Mazaryn.Chats do
  @moduledoc """
  This module provides functions for managing chat interactions between users.
  """
  alias Mazaryn.Chats.Chat
  alias Account.User
  alias Account.Users
  alias :chat_server, as: ChatDB
  import Ecto.Query
  alias Mazaryn.Repo

  def create_chat(%User{id: actor_id}, %User{id: recipient_id}, params) do
    if actor_id == recipient_id do
      {:error, :self_chat_not_allowed}
    else
      %Chat{}
      |> Chat.changeset(params)
      |> Ecto.Changeset.apply_action(:validate)
      |> case do
        {:ok, %{body: body, media: media}} ->
          actor_id
          |> ChatDB.send_msg(recipient_id, body, media)
          |> ChatDB.get_msg()
          |> Chat.erl_changeset()

        any ->
          any
      end
    end
  end

  def create_chat(_actor, _recipient, _params), do: {:error, :invalid_chat_participants}

  def get_by_chat_id(id) do
    case ChatDB.get_chat_by_call_id(id) do
      {:chat, id, ai_chat_id, user_id, recipient_id, body, media, bot, date_created, date_updated, call_id, call_type, call_status, call_link, call_start_time, call_end_time, timeout_ref, data} ->
        Chat.erl_changeset({:chat, id, ai_chat_id, user_id, recipient_id, body, media, bot, date_created, date_updated, call_id, call_type, call_status, call_link, call_start_time, call_end_time, timeout_ref, data})
      :notfound ->
        {:error, :notfound}
      :chat_not_exist ->
        {:error, :notfound}
      _ ->
        {:error, :invalid_chat_record}
    end
  end

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
          {:error, _} -> {{:error, :notfound}, &2}
        end)
    )
    |> elem(1)
    |> Enum.sort_by(& &1.date_created, :desc)
    |> then(fn chats ->
      if Enum.empty?(ids), do: chats, else: Enum.filter(chats, &(to_charlist(&1.id) in ids))
    end)
    |> Enum.sort_by(& &1.date_created, {:asc, DateTime})
  end

  @spec get_users_with_chats(User.t()) :: list()
  def get_users_with_chats(actor) do
    actor.chat
    |> get_chats()
    |> Enum.map(&to_charlist(&1.user_id))
    |> Enum.uniq()
    |> Enum.map(&(&1 |> Users.one_by_id() |> elem(1)))
  end

  def get_users_chatted_to(actor, limit \\ 5) do
    get_chats()
    |> Enum.filter(&(to_charlist(&1.user_id) == actor.id))
    |> Enum.sort_by(& &1.date_created, {:desc, DateTime})
    |> Enum.uniq_by(& &1.recipient_id)
    |> Enum.take(limit)
    |> Enum.map(&(to_charlist(&1.recipient_id) |> Users.one_by_id() |> elem(1)))
  end

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

  def get_chat_messages(%User{} = actor, %User{} = recipient) do
    if actor.id == recipient.id do
      []
    else
      actor.chat
      |> Kernel.++(recipient.chat)
      |> get_chats()
      |> Enum.filter(&(to_charlist(&1.recipient_id) in [recipient.id, actor.id]))
    end
  end

  def get_chat_messages(_, _), do: []

  def get_unread_count(user_id) do
    query =
      from c in Chat,
        where: c.user_id == ^user_id and c.status == "unread",
        select: count(c.id)

    Repo.one(query)
  end

  def start_video_call(%User{id: actor_id}, %User{id: recipient_id}) do
    if actor_id == recipient_id do
      {:error, :self_call_not_allowed}
    else
      try do
        call_id = ChatDB.start_video_call(actor_id, recipient_id)
        {:ok, call_id}
      catch
        {:error, reason} -> {:error, reason}
      end
    end
  end

  @spec accept_call(binary()) :: {:ok, binary()} | {:error, term()}
  def accept_call(call_id) do
    try do
      call_id = ChatDB.accept_call(call_id)
      {:ok, call_id}
    catch
      {:error, reason} -> {:error, reason}
    end
  end

  @spec end_call(binary()) :: {:ok, binary()} | {:error, term()}
  def end_call(call_id) do
    try do
      call_id = ChatDB.end_video_call(call_id)
      {:ok, call_id}
    catch
      {:error, reason} -> {:error, reason}
    end
  end
end
