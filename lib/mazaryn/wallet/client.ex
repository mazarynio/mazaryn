defmodule Wallet.Client do

  def start do
    :wallet_server.start_link()
  end

  def create(name, password) do
    :wallet_server.create(name, password)
  end

  def get_wallet(pub_key) do
    :wallet_server.get_wallet(pub_key)
  end

  def get_wallets() do
    :wallet_server.get_wallets()
  end

  def get_address(pub_key) do
    :wallet_server.get_address(pub_key)
  end

  def deposit(pub_key, amount) do
    :wallet_server.deposit(pub_key, amount)
  end

  def withdraw(pub_key, amount) do
    :wallet_server.withdraw(pub_key, amount)
  end

  def deposit(amount) do
    :wallet_server.deposit(amount)
  end

  def get_balance(pub_key) do
    :wallet_server.get_balance(pub_key)
  end

end
