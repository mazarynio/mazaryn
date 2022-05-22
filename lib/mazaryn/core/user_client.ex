defmodule Mazaryn.Core.UserClient do
  def start do
    :user_server.start_link()
    :user_server.init([])
  end

  def register(username, password) do
    :user_server.create_account(username, password)
  end

  def getting_user(username) do
    :user_server.get_user(username)
  end

  def getting_users() do
    :user_server.get_users()
  end

  def follow_user(username) do
    :user_server.follow(username)
  end

  def creating_post(content) do
    :user_server.create_post(content)
  end
end
