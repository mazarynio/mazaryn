defmodule Core.UserClient do

  def start do
    :otpcode_app.start("_a", "_b")
    :user_server.start_link()
  end


  def register(username, password, email) do
    :user_server.create_account(username, password, email)
  end

  def login(username, password) do
    :user_server.login(username, password)
  end

  def set_user_info(username, fields, values) do
    :user_server.set_user_info(Username, Fields, Values)
  end

  def getting_user(username) do
    :user_server.get_user(Username)
  end

  def getting_users() do
    :user_server.get_users()
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

  def follow(username, following) do
    :user_server.follow(Username, Following)
  end

  def unfollow(username, following) do
    :user_server.unfollow(Username, Following)
  end

  def follow_multiple(username, others) do
    :user_server.follow_multiple(Username, Others)
  end

  def unfollow_multiple(username, others) do
    :user_server.unfollow_multiple(Username, Others)
  end

  def get_following(username) do
    :user_server.get_following(Username)
  end

  def get_follower(username) do
    :user_server.get_follower(Username)
  end

  def get_user_info(username) do
    :user_server.get_user_info(Username)
  end
end
