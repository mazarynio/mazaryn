defmodule Mazaryn.Wallet.Utils do
  def generate_id do
    :crypto.strong_rand_bytes(30)
  end
end
