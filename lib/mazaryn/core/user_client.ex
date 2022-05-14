defmodule Core.UserClient do
  def start do
    :user_server.start_link()
    :user_server.init([])
  end

  def register(username, password) do
    :user_server.create_account(Username, Password)
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

  def unfollow_user(username) do
    :user_server.unfollow_user(Username)
  end

  def creating_post(content) do
    :user_server.create_post(Content)
  end
end
