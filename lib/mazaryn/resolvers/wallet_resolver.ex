defmodule Resolvers.WalletResolver do
  alias Wallet.Hedera.HedWalletClient, as: HedWalletClient

  def create_wallet(password) do
    {:ok, HedWalletClient.create(password)}
  end

  def all(_args, _info) do
    {:ok, HedWalletClient.get_wallets()}
  end
end
