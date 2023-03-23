defmodule Mazaryn.Chats do
  @moduledoc false
  alias Core.ChatClient
  alias Mazaryn.Chats.Chat
  alias Account.User

  def create_chat(%User{id: actor_id}, %User{id: recipient_id}) when actor_id != recipient_id do
    %Chat{}
    |> Chat.changeset(%{peer_ids: [actor_id, recipient_id]})
    |> Ecto.Changeset.apply_action(:validate)
    |> case do
      {:ok, chat} ->
        chat.peer_ids
        |> ChatClient.create_chat(chat.title)
        |> Chat.erl_changeset()

      any ->
        any
    end
  end
end
