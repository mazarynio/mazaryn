defmodule Core.ChatClient do
  @moduledoc """
  This module facilitates communication with Erlang functions using GenServer.
  """

  @doc """
  iex> Core.ChatClient.send_msg(~c"zKegB4mWRXP3PDVuntpnA", ~c"Xvegu4mWR7PdJDLuntpoQ", "hello", "media")
  ~c"svEln4mWR5PdJcPuntpo7"
  """
  def send_msg(user_id, recipient_id, body, media) do
    :chat_server.send_msg(user_id, recipient_id, body, media)
  end

  def get_msg(chat_id) do
    :chat_server.get_msg(chat_id)
  end

  def get_all_msg(recipient_id) do
    :chat_server.get_all_msg(recipient_id)
  end

  def edit_msg(chat_id, new_content) do
    :chat_server.edit_msg(chat_id, new_content)
  end

  def delete_msg(chat_id) do
    :chat_server.delete_msg(chat_id)
  end

  def create_chat(peer_ids, title) do
    :chat_server.create_chat(peer_ids, title)
  end

  def get_chat_by_id(id) do
    :chat_server.get_chat_by_id(id)
  end

  def get_user_chats(id) do
    :chat_server.get_user_chats(id)
  end

  def list_chats() do
    :chat_server.list_chats()
  end
end
