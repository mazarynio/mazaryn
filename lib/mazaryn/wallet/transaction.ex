defmodule Mazaryn.Wallet.Transaction do

  use Ecto.Schema

  import Ecto.Changeset
  #defstruct [
  #  :id,
  #  :status,
  #  :amount
  # ]

  @pending "pending"
  @confirmed "confirmed"
  @failed "failed"
  def pending, do: @pending
  def confirmed, do: @confirmed
  def failed, do: @failed

  @internal "internal"
  @external "external"
  def internal, do: @internal
  def external, do: @external

  schema "transactions" do
    field :type, :string, default: @internal
    field :status, :string, default: @pending
    field :amount, :decimal
    belongs_to(:wallet, Mazaryn.Wallet)
    belongs_to(:user, Account.User)
  end

  @required_attrs [
    :type,
    :type
  ]

  def changeset(transaction, params \\ %{}) do
    transaction
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
  end

end
