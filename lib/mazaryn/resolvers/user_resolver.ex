defmodule Resolvers.UserResolver do
  alias Core.UserClient, as: UserClient

  def all(_args, _info) do
    {:ok, UserClient.get_wallets()}
  end
end
