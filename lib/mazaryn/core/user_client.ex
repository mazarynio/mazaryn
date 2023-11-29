defmodule Core.UserClient do
  def register(username, password, email) do
    :user_server.start_link()
    :user_server.create_account(username, password, email)
  end

  def insert_notif(userID, message) do
    :user_server.insert_notif(userID, message)
  end

  def validate(token_id) do
    :token_server.start_link()
    :token_server.validate(token_id)
  end

  def insert_avatar(id, avatar_url) do
    :user_server.insert_avatar(id, avatar_url)
  end

  def insert_banner(id, banner_url) do
    :user_server.insert_banner(id, banner_url)
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

  def get_pass(id) do
    :user_server.get_password(id)
  end

  def get_user_by_email(email) do
    :user_server.get_user_by_email(email)
  end

  def get_user_by_id(id) do
    :user_server.get_user_by_id(id)
  end

  def get_token_by_id(token_id) do
    :user_server.get_token_by_id(token_id)
  end

  def get_single_notif(notifID) do
    :user_server.get_single_notif(notifID)
  end

  def get_all_notifs(userID) do
    :user_server.get_all_notifs(userID)
  end

  def change_pass(username, current_pass, new_pass) do
    :user_server.change_password(username, current_pass, new_pass)
  end

  def change_mail(username, password, new_email) do
    :user_server.change_email(username, password, new_email)
  end

  def change_username(username, current_pass, new_username) do
    :user_server.change_username(username, current_pass, new_username)
  end

  def delete_user(username) do
    :user_server.delete_user(username)
  end

  # id = MyID, following = UserID
  @spec follow(Binary.t(), String.t()) :: :ok | any()
  def follow(id, following) do
    :user_server.follow(id, following)
  end

  # id = MyID, following = UserID
  @spec unfollow(Binary.t(), String.t()) :: :ok | any()
  def unfollow(id, following) do
    :user_server.unfollow(id, following)
  end

  def follow_multiple(id, others) do
    :user_server.follow_multiple(id, others)
  end

  def unfollow_multiple(id, others) do
    :user_server.unfollow_multiple(id, others)
  end

  def save_post(id, postId) do
    :user_server.save_post(id, postId)
  end

  def unsave_post(id, postId) do
    :user_server.unsave_post(id, postId)
  end

  def save_posts(id, postIds) do
    :user_server.save_posts(id, postIds)
  end

  def unsave_posts(id, postIds) do
    :user_server.unsave_posts(id, postIds)
  end

  def get_save_posts(id) do
    :user_server.get_save_posts(id)
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

  # id= MyID, blocked = UserID
  def block(id, blocked) do
    :user_server.block(id, blocked)
  end

  def unblock(id, unblocked) do
    :user_server.unblock(id, unblocked)
  end

  def get_blocked(id) do
    :user_server.get_blocked(id)
  end

  def add_media(id, mediaType, url) do
    :user_server.add_media(id, mediaType, url)
  end

  def get_media(id, type) do
    :user_server.get_media(id, type)
  end

  def search_user(username) do
    :user_server.search_user(username)
  end

  def search_user_pattern(pattern) do
    :user_server.search_user_pattern(pattern)
  end

  def report_user(my_id, user_id, type, description) do
    :user_server.report_user(my_id, user_id, type, description)
  end

  def make_private(user_id) do
    :user_server.make_private(user_id)
  end

  def make_public(user_id) do
    :user_server.make_public(user_id)
  end
end
