defmodule Account.UserResolver do
  alias Core.UserClient, as: UserClient

  def all(_args, _info) do
    {:ok, UserClient.getting_users()}
  end
end
