defmodule Mazaryn.Chats do
  @moduledoc false
  alias Mazaryn.Chats.Chat
  alias Account.User
  alias :chat_server, as: ChatDB

  def create_chat(%User{id: actor_id}, %User{id: recipient_id}) when actor_id != recipient_id do
    %Chat{}
    |> Chat.changeset(%{peer_ids: [actor_id, recipient_id]})
    |> Ecto.Changeset.apply_action(:validate)
    |> case do
      {:ok, %{peer_ids: ids, title: title}} ->
        ids
        |> ChatDB.create_chat(title)
        |> Chat.erl_changeset()

      any ->
        any
    end
  end

  def create_chat(_, _), do: {:error, :invalid_chat_participants}

  def get_by_chat_id(id), do: id |> ChatDB.get_chat_by_id() |> Chat.erl_changeset()

  def get_user_chats(user_id) do
    # user_id
    # |> ChatDB.get_user_chats()
    # |> Enum.map(&Chat.erl_changeset/1)

    []
  end
end
