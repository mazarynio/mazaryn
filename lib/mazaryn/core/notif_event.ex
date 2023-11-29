defmodule Core.NotifEvent do
  def welcome(user_id) do
    username = :notifdb.get_username_by_id(user_id)
    message = "Welcome to Mazaryn Dear #{username}"
    :notif_event.welcome(user_id, message)
  end

  def follow(user_id) do
    username = :notifdb.get_username_by_id(user_id)
    message = "#{username} followed you"
    :notif_event.follow(user_id, message)
  end
end
