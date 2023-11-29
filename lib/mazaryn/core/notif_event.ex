defmodule Core.NotifEvent do
  # Welcome Notification from Mazaryn
  def welcome(user_id) do
    username = :notifdb.get_username_by_id(user_id)
    message = "Welcome to Mazaryn Dear #{username}"
    :notif_event.welcome(user_id, message)
  end
  # Follow Notification to inform users who followed them
  def follow(user_id) do
    username = :notifdb.get_username_by_id(user_id)
    message = "#{username} followed you"
    :notif_event.follow(user_id, message)
  end

  def message(user_id) do
    username = :notifdb.get_username_by_id(user_id)
    message = "#{username} messaged you"
    :notif_event.notif(user_id, message)
  end

  def mention(user_id) do
    username = :notifdb.get_username_by_id(user_id)
    message = "#{username} mentioned you"
    :notif_event.notif(user_id, message)
  end

  def change_username(user_id) do
    message = "Your Username changed Successfully"
    :notif_event.notif(user_id, message)
  end

  def change_email(user_id) do
    message = "Your Email changed Successfully"
    :notif_event.notif(user_id, message)
  end
end
