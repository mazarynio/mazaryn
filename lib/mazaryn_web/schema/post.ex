defmodule MazarynWeb.Schema.Post do
  use Absinthe.Schema.Notation

  object :post do
    field(:id, :id)
    field(:comments, :string)
    field(:date_created, :string)
    field(:content, :string)
    field(:author, :string)
  end

  object :find_post_by_id do
    field(:id, :id)
    field(:comments, :string)
    field(:date_created, :string)
    field(:content, :string)
    field(:author, :string)
  end

  object :find_post_by_author do
    field(:id, :id)
    field(:comments, :string)
    field(:date_created, :string)
    field(:content, :string)
    field(:author, :string)
  end

  object :find_post_by_hashtag do
    field(:id, :id)
    field(:comments, :string)
    field(:date_created, :string)
    field(:content, :string)
    field(:author, :string)
  end

  object :create_post do
    field(:id, :id)
    field(:comments, :string)
    field(:date_created, :string)
    field(:content, :string)
    field(:author, :string)
  end
end
