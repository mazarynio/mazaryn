defmodule Mazaryn.Wallet do
  defstruct [
    name: nil,
    password: nil,
    address: nil,
    balance: nil,
    pub_key: nil,
    priv_key: nil,
    tokens: nil
  ]

  def new(name, password) do
    %__MODULE__{
      name: name,
      password: password
    }
  end
end
