defmodule Core.ChatClient do
  def start, do: :chat_server.start_link()
end
