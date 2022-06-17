defmodule Mazaryn.Wallet.Client do

  def deposit(amount) do
    :wallet_server.deposit(amount)
  end
end
