defmodule Core.ChatGPT do
  @moduledoc """
  This module facilitates communication with Erlang functions using GenServer.
  """
  ## This is a private function for getting response from ChatGPT
  defp call_python(prompt) do
    command = "python3 /home/zaryn/mazaryn/src/otpcode/pycode/chat/chatgpt.py '#{prompt}'"
    response = System.cmd("bash", ["-c", command])
    response
  end
  ## Send message to ChatGPT and waiting for response using MyID and Message Content
  def send_msg(user_id, body) do
    x = call_python(body)
    :chatbot.send_msg2(user_id, x)
  end
  ## Get specific message using ChatID
  def get_msg(chat_id) do
    :chatbot.get_msg(chat_id)
  end
  ## Get all messages using UserID
  def get_all_msg(recipient_id) do
    :chatbot.get_all_msg(recipient_id)
  end
end
