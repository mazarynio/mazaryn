defmodule Dashboard.Wallet do
  use Ecto.Schema

  import Ecto.Changeset

  alias Mazaryn.Wallet, as: Wallet
  alias Wallet.Client

  schema "wallets" do
    field(:name, :string)
    field(:password)
    has_many(:transactions, Mazaryn.Wallet.Transaction)
    belongs_to(:user, Account.User)

    # timestamps()
  end

  @required_attrs [
    :name,
    :password
  ]

  def changeset(wallet, params \\ %{}) do
    wallet
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
  end

  def create_wallet(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def create_wallet(%Ecto.Changeset{} = changeset) do
    name = changeset |> Ecto.Changeset.get_field(:name)
    password = changeset |> Ecto.Changeset.get_field(:password)

    case Wallet.Client.create(name, password) do
      :ok ->
        %Wallet{name: name, password: password}

      :wallet_existed ->
        :wallet_existed

      other ->
        other
    end
  end
end
