defmodule Wallet.Client do

  def create(name, password) do
    :wallet_server.create(name, password)
  end

  def deposit(amount) do
    :wallet_server.deposit(amount)
  end
end
