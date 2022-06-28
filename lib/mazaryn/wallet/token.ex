defmodule Wallet.Token do

  use Ecto.Schema
  import Ecto.Changeset

  schema "token" do
    field(:symbol, :string)
    field(:name, :string)
    field(:description, :string)
  end

  @required_attrs [
    :symbol,
    :name
  ]

  def changeset(token, params \\ %{}) do
    token
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
  end
end
