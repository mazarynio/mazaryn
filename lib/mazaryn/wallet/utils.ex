defmodule Mazaryn.Wallet.Utils do

  def generate_id do
    :crypto.strong_rand_bytes(30)
  end

  def generate_pair() do
    :crypto.generate_key(:ecdh, :secp256k1)
  end
end
