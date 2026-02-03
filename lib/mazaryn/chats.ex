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
  require Logger

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
    Logger.info("📋 ========================================")
    Logger.info("📋 [GET CHAT BY ID] Looking up chat by call_id: #{id}")

    case ChatDB.get_chat_by_call_id(id) do
      {:chat, chat_id, ai_chat_id, user_id, recipient_id, body, media, bot, date_created,
       date_updated, call_id, call_type, call_status, call_link, call_start_time, call_end_time,
       timeout_ref, data} ->
        Logger.info("✅ [GET CHAT BY ID] Chat found: #{chat_id}")
        Logger.info("📋 [GET CHAT BY ID] Call ID: #{call_id}")
        Logger.info("📋 [GET CHAT BY ID] Call link: #{call_link}")
        Logger.info("📋 [GET CHAT BY ID] Call status: #{call_status}")

        chat = %Chat{
          id: to_string(chat_id),
          user_id: to_string(user_id),
          recipient_id: to_string(recipient_id),
          body: to_string(body),
          media: if(media == :undefined or media == "", do: nil, else: to_string(media)),
          bot: to_string(bot),
          date_created: date_created,
          date_updated: date_updated,
          call_id: to_string(call_id),
          call_type: to_string(call_type),
          call_status: to_string(call_status),
          call_link: to_string(call_link),
          call_start_time: call_start_time,
          call_end_time: call_end_time,
          data: data
        }

        Logger.info("✅ [GET CHAT BY ID] Chat struct created successfully")
        Logger.info("📋 ========================================")
        {:ok, chat}

      :notfound ->
        Logger.warning("⚠️ [GET CHAT BY ID] Chat not found")
        Logger.info("📋 ========================================")
        {:error, :notfound}

      :chat_not_exist ->
        Logger.warning("⚠️ [GET CHAT BY ID] Chat does not exist")
        Logger.info("📋 ========================================")
        {:error, :notfound}

      other ->
        Logger.error("❌ [GET CHAT BY ID] Invalid chat record: #{inspect(other)}")
        Logger.info("📋 ========================================")
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
    ten_hours_ago = DateTime.utc_now() |> DateTime.add(-10, :hour)

    get_chats()
    |> Enum.filter(&(to_charlist(&1.user_id) == actor.id))
    |> Enum.filter(&(DateTime.compare(&1.date_created, ten_hours_ago) == :gt))
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
    Logger.info("📞 ========================================")
    Logger.info("📞 [START VIDEO CALL] Initiating video call")
    Logger.info("📞 [START VIDEO CALL] Actor ID: #{inspect(actor_id)}")
    Logger.info("📞 [START VIDEO CALL] Recipient ID: #{inspect(recipient_id)}")

    if actor_id == recipient_id do
      Logger.warning("⚠️ [START VIDEO CALL] Self-call not allowed")
      Logger.info("📞 ========================================")
      {:error, :self_call_not_allowed}
    else
      try do
        Logger.info("📞 [START VIDEO CALL] Calling ChatDB.start_video_call")
        call_id = ChatDB.start_video_call(actor_id, recipient_id)
        Logger.info("✅ [START VIDEO CALL] Call started successfully")
        Logger.info("📞 [START VIDEO CALL] Call ID: #{call_id}")
        Logger.info("📞 ========================================")
        {:ok, call_id}
      catch
        {:error, reason} ->
          Logger.error("❌ [START VIDEO CALL] Error: #{inspect(reason)}")
          Logger.info("📞 ========================================")
          {:error, reason}

        error ->
          Logger.error("❌ [START VIDEO CALL] Unexpected error: #{inspect(error)}")
          Logger.info("📞 ========================================")
          {:error, error}
      end
    end
  end

  @spec accept_call(binary()) :: {:ok, binary()} | {:error, term()}
  def accept_call(call_id) do
    Logger.info("✅ ========================================")
    Logger.info("✅ [ACCEPT CALL] Accepting video call")
    Logger.info("✅ [ACCEPT CALL] Call ID: #{call_id}")

    try do
      Logger.info("✅ [ACCEPT CALL] Calling ChatDB.accept_call")
      result = ChatDB.accept_call(call_id)
      Logger.info("✅ [ACCEPT CALL] Call accepted successfully")
      Logger.info("✅ [ACCEPT CALL] Result: #{inspect(result)}")
      Logger.info("✅ ========================================")
      {:ok, result}
    catch
      {:error, reason} ->
        Logger.error("❌ [ACCEPT CALL] Error: #{inspect(reason)}")
        Logger.info("✅ ========================================")
        {:error, reason}

      error ->
        Logger.error("❌ [ACCEPT CALL] Unexpected error: #{inspect(error)}")
        Logger.info("✅ ========================================")
        {:error, error}
    end
  end

  @spec end_call(binary()) :: {:ok, binary()} | {:error, term()}
  def end_call(call_id) do
    Logger.info("🔴 ========================================")
    Logger.info("🔴 [END CALL] Ending video call")
    Logger.info("🔴 [END CALL] Call ID: #{call_id}")

    try do
      Logger.info("🔴 [END CALL] Calling ChatDB.end_video_call")
      result = ChatDB.end_video_call(call_id)
      Logger.info("✅ [END CALL] Call ended successfully")
      Logger.info("🔴 [END CALL] Result: #{inspect(result)}")
      Logger.info("🔴 ========================================")
      {:ok, result}
    catch
      {:error, reason} ->
        Logger.error("❌ [END CALL] Error: #{inspect(reason)}")
        Logger.info("🔴 ========================================")
        {:error, reason}

      error ->
        Logger.error("❌ [END CALL] Unexpected error: #{inspect(error)}")
        Logger.info("🔴 ========================================")
        {:error, error}
    end
  end
end
