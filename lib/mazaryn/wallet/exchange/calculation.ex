defmodule Wallet.Exchange.Calculation do
  @enforce_keys [
    :from_amount,
    :from_token,
    :to_amount,
    :to_token,
    :actual_rate,
    :pair,
    :calculated_at
  ]

  defstruct [
    :from_amount,
    :from_token,
    :to_amount,
    :to_token,
    :actual_rate,
    :pair,
    :calculated_at
  ]
end
