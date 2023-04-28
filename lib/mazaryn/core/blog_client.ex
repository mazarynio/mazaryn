defmodule Core.BlogClient do

  def insert(author, content, _media) do
    :blog_server.insert(author, content, _media)
  end

  def delete_post(post_id) do
    :blog_server.delete_post(post_id)
  end

  def get_post(post_id) do
    :blog_server.get_post(post_id)
  end
end
