defmodule Core.NotifClient do
  def get_notif(message) do
    :notif_server.start_link()
    :notif_server.notify(message)
  end

  def get_notif(from, to, message) do
    :notif_server.start_link()
    :notif_server.notify(from, to, message)
  end
end
