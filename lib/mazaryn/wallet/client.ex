defmodule Wallet.Client do

  def start do
    :wallet_server.start_link()
  end

  def create(name, password) do
    :wallet_server.create(name, password)
  end

  def get_wallet(address) do
    :wallet_server.get_wallet(address)
  end

  def get_wallets() do
    :wallet_server.get_wallets()
  end

  def deposit(amount) do
    :wallet_server.deposit(amount)
  end
end
