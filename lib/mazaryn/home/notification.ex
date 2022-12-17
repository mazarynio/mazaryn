defmodule Home.Notification do
  use Ecto.Schema
  import Ecto.Changeset

  schema "notification" do
    field(:action, :string)
    field(:read, :boolean, default: false)
    belongs_to(:user, Account.User)
    belongs_to(:comment, Home.Comment)
    belongs_to(:post, Home.Post)

    # timestamps()
  end
end
