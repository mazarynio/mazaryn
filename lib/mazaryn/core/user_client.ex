defmodule Core.UserClient do
  @moduledoc """
  This module facilitates communication with Erlang functions using GenServer.
  It provides functions for user registration, notification insertion and ...
  """

  @doc """
      iex> Core.UserClient.register("my_username", "my_pass", "my_email")
      ~c"zKegB4mWRXP3PDVuntpnA"
  """
  @spec register(String.t(), String.t(), String.t()) :: atom() | charlist()
  def register(username, password, email) do
    :user_server.start_link()
    :user_server.create_account_concurrent(username, password, email)
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

  @spec set_user_info(any(), any(), any()) :: any()
  def set_user_info(username, fields, values) do
    :user_server.set_user_info(username, fields, values)
  end

  ## Get User Informations using Username
  def get_user(username) do
    :user_server.get_user(username)
  end

  def get_user_in_transaction(username) do
    :user_server.get_user_in_transaction(username)
  end

  ## Get all Users on the Network
  def get_all() do
    :user_server.get_users()
  end

  ## Get Password using UserID
  def get_pass(id) do
    :user_server.get_password(id)
  end

  ## Get User information by User Email
  def get_user_by_email(email) do
    :user_server.get_user_by_email(email)
  end

  ## Get User by UserID
  def get_user_by_id(id) do
    case id do
      {:error, _reason} = error -> error
      nil -> {:error, :invalid_id}
      _ ->
        charlist_id = if is_list(id), do: id, else: String.to_charlist(id)

        case :user_server.get_user_by_id(charlist_id) do
          {:error, _reason} = error -> error
          user_tuple -> user_tuple
        end
    end
  end

  def get_token_by_id(token_id) do
    :user_server.get_token_by_id(token_id)
  end

  ## Change Password Using Username, Current Password and New Password
  def change_pass(username, current_pass, new_pass) do
    :user_server.change_password(username, current_pass, new_pass)
  end

  def change_mail(username, password, new_email) do
    :user_server.change_email(username, password, new_email)
  end

  def change_username(username, current_pass, new_username) do
    :user_server.change_username(username, current_pass, new_username)
  end

  ## Remove User Permanently using Username
  def delete_user(username) do
    :user_server.delete_user(username)
  end

  # id = MyID, following = UserID
  def follow(id, following) do
    :user_server.follow(id, following)
  end

  # id = MyID, following = UserID
  def unfollow(id, following) do
    :user_server.unfollow(id, following)
  end

  def follow_multiple(id, others) do
    :user_server.follow_multiple(id, others)
  end

  def unfollow_multiple(id, others) do
    :user_server.unfollow_multiple(id, others)
  end

  ## Save Post using MyID and PostID
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

  ## Block user using MyID and UserID
  def block(id, blocked) do
    :user_server.block(id, blocked)
  end

  ## Unblock user using MyID and UserID
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

  ## resport user using MyID, UserID, Report Type (Spam, Harassment, Violence ..) and Description
  def report_user(my_id, user_id, type, description) do
    :user_server.report_user(my_id, user_id, type, description)
  end

  ## Make user profile private and only visible to current followers (default is public)
  def make_private(user_id) do
    :user_server.make_private(user_id)
  end

  ## Make user profile public and visible to all users (default is public)
  def make_public(user_id) do
    :user_server.make_public(user_id)
  end

  def mark_notif_as_seen(notif_id) do
  :mnesia.transaction(fn ->
    case :mnesia.read({:notif, notif_id}) do
      [_notif = {:notif, ^notif_id, actor, target, message, timestamp, metadata}] ->
        updated_notif = {:notif, notif_id, actor, target, message, timestamp, Map.put(metadata, :seen, true)}
        :mnesia.write(updated_notif)
       [] -> :ok
      end
    end)
  end

end
