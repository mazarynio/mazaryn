defmodule Mazaryn.Wallet.Utils do

  def generate_id do
    :crypto.strong_rand_bytes(30)
  end

  def generate_pair() do
    :crypto.generate_key(:ecdh, :secp256k1)
  end

  def hash(data) do
    :crypto.hash(:sha256, data)
  end

  def format_address(hash) do
    "1" |> String.slice(hash, 1..31)
  end

  def generate_address_from_public_key(pk) do
    pk |> hash() |> Base.encode16() |> format_address()
  end
end
