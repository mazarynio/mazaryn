defmodule Resolvers.UserResolver do
  alias Account.Users

  def all(_args, _info) do
    {:ok, Users.list_users()}
  end

  def create_user(username, password, email) do
    {:ok, Users.create_user()}
  end

  def user_login(email, password) do
    {:ok, Users.login(email, password)}
  end

  def find_user_by_id(%{id: id}, _info) do
    Users.one_by_id(id)
  end

  def find_user_by_username(%{username: username}, _info) do
    Users.one_by_username(username)
  end

  def find_user_by_email(%{email: email}, _info) do
    Users.one_by_email(email)
  end
end
