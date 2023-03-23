defmodule Core.ChatClient do
  def start, do: :chat_server.start_link()
  def create_chat(peer_ids, title), do: :chat_server.create_chat(peer_ids, title)
end
