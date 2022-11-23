defmodule Core.UserClient do

  def register(username, password, email) do
    :user_server.start_link()
    :token_server.start_link()
    :user_server.create_account(username, password, email)
  end

  def validate(token_id) do
    :token_server.start_link()
    :token_server.validate(token_id)
  end

  def insert_avatar(username, avatar_url) do
    :user_server.insert_avatar(username, avatar_url)
  end

  def insert_banner(username, banner_url) do
    :user_server.insert_banner(username, banner_url)
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

  @doc """
  follow a specific profile

    iex> id = 1234
    iex> following = username1234
    iex> follow(id, following)
    :ok
  """
  @spec follow(Binary.t(), String.t()) :: :ok | any()
  def follow(id, following) do
    :user_server.follow(id, following)
  end

  @doc """
  unfollow a specific profile

    iex> id = 1234
    iex> following = username1234
    iex> unfollow(id, following)
    :ok
  """
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

  def search_user_pattern(pattern) do
    :user_server.search_user_pattern(pattern)
  end
end
