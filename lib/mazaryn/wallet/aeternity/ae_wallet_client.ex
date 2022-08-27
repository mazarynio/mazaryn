defmodule Wallet.Aeternity.AE_wallet_client do

  def create(name, password, size, token) do
    :ae_wallet_server.create(nme, password, size, token)
  end

  def get_wallet(name) do
    :ae_wallet_server.get_wallet(name)
  end

  def get_wallets() do
    :ae_wallet_server.get_wallets()
  end

  def deposit(name, amount) do
    :ae_wallet_server.deposit(name, amount)
  end

  def withdraw(name, amount) do
    :ae_wallet_server.withdraw(name, amount)
  end
end
