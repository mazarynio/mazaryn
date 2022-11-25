defmodule Wallet.Hedera.HedWalletClient do

  def start() do
    :hedera_wallet_server.start_link()
  end

  def create(password) do
    :hedera_wallet_server.create_account(password)
  end
end
