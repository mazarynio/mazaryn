defmodule Account.Invitation do

  use Ecto.Schema
  import Ecto.Changeset

  schema "invitations" do
    field :email, :string
    field :role, :string
    field :token, :string
    field :pending, :boolean, default: true
    field :inviter_id, :string
    belongs_to :user, Account.User

    # timestamsp()
  end

  @required_attrs [
    :email
  ]

  def changeset(invitation, params \\ %{}) do
    invitation
    |> cast(params, @required_attrs)
    |> validate_required(@required_attrs)
  end

end
