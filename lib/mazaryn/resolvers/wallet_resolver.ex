defmodule Resolvers.WalletResolver do
  alias Wallet.Hedera.HedWalletClient, as: HedWalletClient

  def all(_args, _info) do
    {:ok, HedWalletClient.get_wallets()}
  end
end
