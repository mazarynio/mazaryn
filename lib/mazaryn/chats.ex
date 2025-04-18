defmodule Mazaryn.Chats do
  @moduledoc """
  This module provides functions for managing chat interactions between users.
  """
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
      {:ok, %{body: body, media: media}} ->
        actor_id
        |> ChatDB.send_msg(recipient_id, body, media)
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
  
  @spec mark_messages_as_read(binary | charlist, binary | charlist) :: {:ok, list} | {:error, any}
  def mark_messages_as_read(recipient_id, sender_id) do
    recipient_id_charlist = if is_binary(recipient_id), do: to_charlist(recipient_id), else: recipient_id
    sender_id_charlist = if is_binary(sender_id), do: to_charlist(sender_id), else: sender_id
    
    try do
      # Call ChatDB to mark messages as read
      case ChatDB.mark_msgs_read(recipient_id_charlist, sender_id_charlist) do
        :ok -> 
          # Get the updated messages to return
          updated_messages = 
            get_chat_messages(
              %User{id: recipient_id_charlist}, 
              %User{id: sender_id_charlist}
            )
          {:ok, updated_messages}
        {:error, reason} -> 
          {:error, reason}
      end
    rescue
      e -> 
        {:error, e}
    end
  end
  @spec has_unread_messages(binary | charlist, binary | charlist) :: {:ok, boolean} | {:error, any}
  def has_unread_messages(recipient_id, sender_id) do
    recipient_id_charlist = if is_binary(recipient_id), do: to_charlist(recipient_id), else: recipient_id
    sender_id_charlist = if is_binary(sender_id), do: to_charlist(sender_id), else: sender_id
    
    try do
      # Get messages between the users
      unread_count = 
        get_chat_messages(
          %User{id: recipient_id_charlist}, 
          %User{id: sender_id_charlist}
        )
        |> Enum.filter(fn msg -> 
          to_charlist(msg.recipient_id) == recipient_id_charlist && 
          to_charlist(msg.user_id) == sender_id_charlist && 
          !msg.read
        end)
        |> Enum.count()
        
      {:ok, unread_count > 0}
    rescue
      e -> 
        {:error, e}
    end
  end
end
