defmodule Wallet.API.Cardano.Address do
  @type address :: %{
          id: String.t(),
          state: String.t(),
          derivation_path: list(String.t())
        }
end
