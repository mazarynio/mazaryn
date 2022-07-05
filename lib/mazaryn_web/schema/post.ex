defmodule MazarynWeb.Schema.Post do
  use Absinthe.Schema.Notation

  object :post do
    field :id, :id
    field :comment, :string
    field :date_created, :string
  end
end
