defmodule Core.UserClient do
  def register(username, password, email) do
    :user_server.start_link()
    :user_server.create_account(username, password, email)
  end

  def login(email, password) do
    :user_server.login(email, password)
  end

  def set_user_info(username, fields, values) do
    :user_server.set_user_info(username, fields, values)
  end

  def get_user(username) do
    :user_server.get_user(username)
  end

  def get_user_in_transaction(username) do
    :user_server.get_user_in_transaction(username)
  end

  def get_all() do
    :user_server.get_users()
  end

  def get_pass(username) do
    :user_server.get_password(username)
  end

  def get_user_by_email(email) do
    :user_server.get_user_by_email(email)
  end

  def get_user_by_id(id) do
    :user_server.get_user_by_id(id)
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

  def save_post(username, postId) do
    :user_server.save_post(username, postId)
  end

  def unsave_post(username, postId) do
    :user_server.unsave_post(username, postId)
  end

  def save_posts(username, postIds) do
    :user_server.save_posts(username, postIds)
  end

  def unsave_posts(username, postIds) do
    :user_server.unsave_posts(username, postIds)
  end

  def get_save_posts(username) do
    :user_server.get_save_posts(username)
  end

  def get_following(id) do
    :user_server.get_following(id)
  end

  def get_follower(id) do
    :user_server.get_follower(id)
  end

  def get_user_info(username, fields) do
    :user_server.get_user_info(username, fields)
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

  def add_media(username, mediaType, url) do
    :user_server.add_media(username, mediaType, url)
  end

  def get_media(username, mediaType) do
    :user_server.get_media(username, mediaType)
  end
end
