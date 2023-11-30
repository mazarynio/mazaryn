defmodule Core.ChatGPT do
  @moduledoc """
  This module facilitates communication with Erlang functions using GenServer.
  """
  defp call_python(prompt) do
    command = "python3 /home/zaryn/mazaryn/src/otpcode/pycode/chat/chatgpt.py '#{prompt}'"
    response = System.cmd("bash", ["-c", command])
    response
  end

  def send_msg(user_id, body) do
    x = call_python(body)
    :chatbot.send_msg2(user_id, x)
  end

  def get_msg(chat_id) do
    :chatbot.get_msg(chat_id)
  end

  def get_all_msg(recipient_id) do
    :chatbot.get_all_msg(recipient_id)
  end
end
