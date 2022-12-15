defmodule Core.NotifClient do

  def get_notif(from, to, message) do
    :notif_server.notify(from, to, message)
  end
  
end
