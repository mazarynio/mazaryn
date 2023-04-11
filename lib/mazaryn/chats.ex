defmodule Mazaryn.Chats do
  @moduledoc false
  alias Mazaryn.Chats.Chat
  alias Account.User
  alias Account.Users
  alias :chat_server, as: ChatDB

  def create_chat(%User{id: actor_id}, %User{id: recipient_id}) when actor_id != recipient_id do
    %Chat{}
    |> Chat.changeset(%{peer_ids: [actor_id, recipient_id]})
    |> Ecto.Changeset.apply_action(:validate)
    |> IO.inspect()
    |> case do
      {:ok, %{peer_ids: ids, title: title}} ->
        ids
        |> Enum.map(&to_charlist/1)
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

  # todo: probably do this with a match on mnesia
  def get_users_without_chat(user_id) do
    user_id = '#{user_id}'

    Users.list()
    |> Enum.map(&(&1 |> Users.one_by_id() |> elem(1)))
    |> Enum.reject(&(user_id in &1.chat or user_id == &1.id))
  end
end
