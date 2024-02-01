defmodule Home.Notification do
  use Ecto.Schema

  schema "notification" do
    field(:action, :string)
    field(:read, :boolean, default: false)
    field(:data, :map)
    belongs_to(:user, Account.User)
    belongs_to(:comment, Home.Comment)
    belongs_to(:post, Home.Post)

    # timestamps()
  end
end
