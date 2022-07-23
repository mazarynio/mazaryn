defmodule Mazaryn.Wallet do

  @doc false
  defstruct [
    :name,
    :password
  ]

  def new(name, password) do
    %__MODULE__{
      name: name,
      password: password
    }
  end
end
