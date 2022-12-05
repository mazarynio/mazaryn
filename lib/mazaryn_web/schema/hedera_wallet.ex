defmodule MazarynWeb.Schema.HederaWallet do
  use Absinthe.Schema.Notation

  object :hedera_wallet do
    field(:password, :string)
  end
end
