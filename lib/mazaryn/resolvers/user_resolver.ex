defmodule Resolvers.UserResolver do
  alias Core.UserClient, as: UserClient

  def all(_args, _info) do
    {:ok, UserClient.get_all_users_info_by_ids()}
  end

  def create_user(username, password, email) do
    {:ok, UserClient.register(username, password, email)}
  end

  def user_login(email, password) do
    {:ok, UserClient.login(email, password)}
  end
end
