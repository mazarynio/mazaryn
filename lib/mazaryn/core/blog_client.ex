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

  def add_comment(author, post_id, content) do
    :blog_server.add_comment(author, post_id, content)
  end

  def update_comment(comment_id, new_content) do
    :blog_server.update_comment(comment_id, new_content)
  end

  def get_single_comment(comment_id) do
    :blog_server.get_single_comment(comment_id)
  end

  def get_all_comments(post_id) do
    :blog_server.get_all_comments(post_id)
  end

  def delete_comment(comment_id, post_id) do
    :blog_server.delete_comment(comment_id, post_id)
  end
end
