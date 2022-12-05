defmodule Wallet.Hedera.HedWalletClient do

  def start() do
    :hedera_wallet_server.start_link()
  end

  def create(password) do
    :hedera_wallet_server.create_account(password)
  end

  def get_wallets() do
    :hedera_wallet_server.get_wallets()
  end
end
