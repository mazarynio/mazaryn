defmodule Core.UserClient do
  def start do
    :user_server.start_link()
    :user_server.init([])
  end

  def register(username, password, email) do
    :user_server.create_account(Username, Password, Email)
  end

  def getting_user(username) do
    :user_server.get_user(Username)
  end

  def getting_users() do
    :user_server.get_users()
  end

  def follow_user(username) do
    :user_server.follow_user(Username)
  end

  def get_pass(username) do
    :user_server.get_password(Username)
  end

  def get_user_by_mail(email) do
    :user_server.get_user_by_email(Email)
  end

  def change_pass(username, current_pass, new_pass) do
    :user_server.change_password(Username, CurrentPass, NewPass)
  end

  def change_mail(username, current_pass, new_email) do
    :user_server.change_email(Username, CurrentPass, NewEmail)
  end

  def change_username(username, current_pass, new_username) do
    :user_server.change_username(Username, CurrentPass, NewUsername)
  end

  def delete_user(username) do
    :user_server.delete_user(Username)
  end

  def following(username, following) do
    :user_server.following(Username, Following)
  end

  def unfollowing(username, following) do
    :user_server.unfollowing(Username, Following)
  end
end
