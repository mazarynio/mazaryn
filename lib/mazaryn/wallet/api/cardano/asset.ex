defmodule Wallet.API.Cardano.Asset do
  @type asset :: %{
          policy_id: String.t(),
          asset_name: String.t(),
          fingerprint: String.t(),
          metadata: metadata(),
          metadata_error: String.t()
        }

  @type metadata :: %{
          name: String.t(),
          description: String.t(),
          ticker: String.t(),
          decimals: non_neg_integer(),
          url: String.t(),
          logo: String.t()
        }
end
