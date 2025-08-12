defmodule MazarynWeb.Schema.Post do
  use Absinthe.Schema.Notation

  object :post do
    field(:id, :string)
    field(:content, :string)
    field(:ai_post_id, :string)
    field(:user_id, :string)
    field(:business_id, :string)
    field(:media, list_of(:string))
    field(:hashtag, :string)
    field(:mention, :string)
    field(:ipns, :string)
    field(:emoji, :string)
    field(:link_url, :string)
    field(:author, :string)
    field(:other, list_of(:string))
    field(:comments, list_of(:comment))
    field(:likes, list_of(:integer))
    field(:date_created, :string)
    field(:date_updated, :string)
    field(:report, list_of(:string))
    field(:device_info, :string)
    field(:pin_info, :string)
    field(:data, :string)
  end

  object :comment do
    field(:id, :string)
    field(:user_id, :string)
    field(:content, :string)
    field(:content_status, :string)
    field(:date_created, :string)
    field(:likes, :string)
    field(:replies, list_of(:reply))
    field(:ipns, :string)
    field(:author, :string)
    field(:post_id, :string)
    field(:data, :string)
    field(:like_comment_event, :string)
  end

  object :reply do
    field(:id, :string)
    field(:content, :string)
    field(:date_created, :string)
    field(:comment, :string)
    field(:chat, :string)
    field(:user_id, :string)
    field(:data, :string)
  end

  input_object :post_input do
    field(:author, non_null(:string))
    field(:content, non_null(:string))
    field(:media, list_of(:string), default_value: [])
    field(:hashtag, :string, default_value: "#mazaryn")
    field(:mention, :string)
    field(:link_url, :string)
    field(:emoji, :string)
  end

  input_object :comment_input do
    field(:author, non_null(:string))
    field(:post_id, non_null(:string))
    field(:content, non_null(:string))
  end

  input_object :reply_input do
    field(:user_id, non_null(:string))
    field(:comment_id, non_null(:string))
    field(:content, non_null(:string))
  end

  input_object :update_comment_input do
    field(:comment_id, non_null(:string))
    field(:content, non_null(:string))
  end

  input_object :update_post_input do
    field(:post_id, non_null(:string))
    field(:author, non_null(:string))
    field(:content, :string)
    field(:emoji, :string)
    field(:media, list_of(:string))
    field(:hashtag, :string)
    field(:mention, :string)
    field(:link_url, :string)
  end
end
