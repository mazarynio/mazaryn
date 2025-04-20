defmodule Core.NotifEvent do
  @moduledoc """
  This module facilitates communication with Erlang functions using GenEvent.
  """
  @doc """
    iex> Core.NotifEvent.welcome(~c"zKegB4mWRXP3PDVuntpnA")
    "welcome to mazaryn username"
  """
  def welcome(user_id) do
    username = :notifdb.get_username_by_id(user_id)
    message = "Welcome to Mazaryn Dear #{username}"
    notif_id = :notif_event.welcome(user_id, message)
    get_notif_message(notif_id)
  end
  def welcome_by_info(user_id) do
    username = :notifdb.get_username_by_id(user_id)
    message = "Welcome to Mazaryn Dear #{username}"
    notif_id = :notif_event.welcome(user_id, message)
    get_notif(notif_id)
  end
  # user(the person ID who follow), user_id(my_id)
  def follow(user, user_id) do
    follower = :notifdb.get_username_by_id(user)
    message = "#{follower} followed you"
    notif_id = :notif_event.follow(user, user_id, message)
    get_notif_message(notif_id)
  end
  def follow_by_info(user, user_id) do
    follower = :notifdb.get_username_by_id(user)
    message = "#{follower} followed you"
    notif_id = :notif_event.follow(user, user_id, message)
    get_notif(notif_id)
  end
  def user_info(user_id) do
    Core.UserClient.get_user_by_id(user_id)
  end
  # user(the person who send message), user_id(my_id)
  def message(user, user_id) do
    sender = :notifdb.get_username_by_id(user)
    message = "#{sender} messaged you"
    notif_id = :notif_event.notif(user_id, message)
    get_notif_message(notif_id)
  end
  # user(the person who mention me), user_id(my_id)
  def mention(user, user_id) do
    mentioner = :notifdb.get_username_by_id(user)
    message = "#{mentioner} mentioned you"
    notif_id = :notif_event.mention(user, user_id, message)
    get_notif_message(notif_id)
  end
  def mention_by_info(user, user_id) do
    mentioner = :notifdb.get_username_by_id(user)
    message = "#{mentioner} mentioned you"
    notif_id = :notif_event.mention(user, user_id, message)
    get_notif(notif_id)
  end
  def chat(sender, receiver) do
    senderID = :notifdb.get_username_by_id(sender)
    message = "You have a new message from #{senderID}"
    notif_id = :notif_event.chat(sender, receiver, message)
    get_notif_message(notif_id)
  end
  def chat_by_info(sender, receiver) do
    senderID = :notifdb.get_username_by_id(sender)
    message = "You have a new message from #{senderID}"
    notif_id = :notif_event.chat(sender, receiver, message)
    get_notif(notif_id)
  end
  ## Get notification when changing my username
  def change_username(user_id) do
    message = "Your Username changed Successfully"
    notif_id = :notif_event.notif(user_id, message)
    get_notif_message(notif_id)
  end
  ## Get notification when changing my email
  def change_email(user_id) do
    message = "Your Email changed Successfully"
    notif_id = :notif_event.notif(user_id, message)
    get_notif_message(notif_id)
  end
  ## Get notification when changing my password
  def change_password(user_id) do
    message = "Your Password changed Successfully"
    notif_id = :notif_event.notif(user_id, message)
    get_notif_message(notif_id)
  end
  ## Get all notification info using NotifiactionID
  def get_notif(notif_id) do
    :notif_event.get_notif(notif_id)
  end
  ## Get Notification Time by NotifID
  def get_notif_time(notif_id) do
    :notif_event.get_notif_time(notif_id)
  end
  ## Get The notification message (content) using NotificationID without getting extra info
  def get_notif_message(notif_id) do
    :notif_event.get_notif_message(notif_id)
  end
  ## Get all Generated Notification per user
  def get_all_notifs(user_id) do
    :notif_event.get_all_notifs(user_id)
  end
  def get_five_latest_ids(user_id) do
    :notif_event.get_five_latest_notif_ids(user_id)
  end
  def get_five_latest_messages(user_id) do
    :notif_event.get_five_latest_notif_messages(user_id)
  end
  ## Update all unread notifications for this user to read true
  def mark_all_as_seen(user_id) do
    mark_all_as_read(user_id)
  end
  def mark_all_as_read(user_id) do
    :notifdb.mark_all_as_read(user_id)
  end
  def mark_as_read(notif_id) do
    :notifdb.mark_as_read(notif_id)
  end
  def count_unread(user_id) do
    :notifdb.count_unread(user_id)
  end
  
  ## alias here to maintain compatibility with api
  def unread_count(user_id) do
    count_unread(user_id)
  end
end
