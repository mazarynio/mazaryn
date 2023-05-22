defmodule Core.ChatClient do
  def start, do: :chat_server.start_link()

  def send_msg(user_id, recipient_id, body) do
    :chat_server.send_msg(user_id, recipient_id, body)
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
