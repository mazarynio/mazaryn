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
    :notif_event.welcome(user_id, message)
  end

  # user(the person ID who follow), user_id(my_id)
  def follow(user, user_id) do
    follower = :notifdb.get_username_by_id(user)
    message = "#{follower} followed you"
    :notif_event.follow(user_id, message)
  end
  # user(the person who send message), user_id(my_id)
  def message(user, user_id) do
    sender = :notifdb.get_username_by_id(user)
    message = "#{sender} messaged you"
    :notif_event.notif(user_id, message)
  end
  # user(the person who mention me), user_id(my_id)
  def mention(user, user_id) do
    mentioner = :notifdb.get_username_by_id(user)
    message = "#{mentioner} mentioned you"
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

  def get_notif(notif_id) do
    :notif_event.get_notif(notif_id)
  end
end
