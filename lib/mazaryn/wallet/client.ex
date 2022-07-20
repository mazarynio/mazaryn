defmodule Wallet.Client do
  def start do
    :wallet_server.start_link()
  end

  def create(name, password) do
    :wallet_server.create(name, password)
  end

  def get_wallet(name) do
    :wallet_server.get_wallet(name)
  end

  def get_wallets() do
    :wallet_server.get_wallets()
  end

  def get_address(name) do
    :wallet_server.get_address(name)
  end

  def deposit(name, amount) do
    :wallet_server.deposit(name, amount)
  end

  def withdraw(name, amount) do
    :wallet_server.withdraw(name, amount)
  end

  def get_balance(name) do
    :wallet_server.get_balance(name)
  end
end
