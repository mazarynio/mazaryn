defmodule Core.UserClient do

  def register(username, password, email) do
    :user_server.create_account(username, password, email)
  end

  def login(username, password) do
    :user_server.login(username, password)
  end

  def set_user_info(username, fields, values) do
    :user_server.set_user_info(username, fields, values)
  end

  def getting_user(username) do
    :user_server.get_user(username)
  end

  def getting_users() do
    :user_server.get_users()
  end

  def get_pass(username) do
    :user_server.get_password(username)
  end

  def get_user_by_mail(email) do
    :user_server.get_user_by_email(email)
  end

  def change_pass(username, current_pass, new_pass) do
    :user_server.change_password(username, current_pass, new_pass)
  end

  def change_mail(username, current_pass, new_email) do
    :user_server.change_email(username, current_pass, new_email)
  end

  def change_username(username, current_pass, new_username) do
    :user_server.change_username(username, current_pass, new_username)
  end

  def delete_user(username) do
    :user_server.delete_user(username)
  end

  def follow(username, following) do
    :user_server.follow(username, following)
  end

  def unfollow(username, following) do
    :user_server.unfollow(username, following)
  end

  def follow_multiple(username, others) do
    :user_server.follow_multiple(username, others)
  end

  def unfollow_multiple(username, others) do
    :user_server.unfollow_multiple(username, others)
  end

  def get_following(username) do
    :user_server.get_following(username)
  end

  def get_follower(username) do
    :user_server.get_follower(username)
  end

  def get_user_info(username) do
    :user_server.get_user_info(username)
  end

  def block(username, blocked) do
    :user_server.block(username, blocked)
  end

  def unblock(username, unblocked) do
    :user_server.unblock(username, unblocked)
  end

  def get_blocked(username) do
    :user_server.get_blocked(username)
  end
end
