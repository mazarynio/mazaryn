defmodule Core.PostClient do
  def start do
    :post_server.start_link()
  end

  def create_post(author, content, media) do
    :post_server.insert(author, content, media)
  end

  def modify_post(author, newContent, newMedia) do
    :post_server.modify_post(author, newContent, newMedia)
  end

  def get_post_by_id(id) do
    :post_server.get_post_by_id(id)
  end

  def get_posts_by_author(author) do
    :post_server.get_posts_by_author(author)
  end

  def get_latest_posts(author) do
    :post_server.get_latest_posts(author)
  end

  def delete_post(id) do
    :post_server.delete_post(id)
  end

  def add_comment(author, postID, content) do
    :post_server.add_comment(author, postID, content)
  end

  def update_comment(commentID, newContent) do
    :post_server.update_comment(commentID, newContent)
  end

  def get_single_comment(commentId) do
    :post_server.get_single_comment(commentId)
  end

  def get_all_comments(postId) do
    :post_server.get_all_comments(postId)
  end

  def get_posts() do
    :post_server.get_posts()
  end

  def get_all_posts_from_date(year, month, date, author) do
    :post_server.get_all_posts_from_date(year, month, date, author)
  end

  def get_all_posts_from_month(year, month, author) do
    :post_server.get_all_posts_from_month(year, month, author)
  end

  def get_comments(id) do
    :post_server.get_comments(id)
  end

  def save_post(username, post_id) do
    :post_server.save_post(username, post_id)
  end

  def unsave_post(username, post_id) do
    :post_server.unsave_post(username, post_id)
  end

  def save_posts(username, post_ids) do
    :post_server.save_posts(username, post_ids)
  end

  def unsave_posts(username, post_ids) do
    :post_server.unsave_posts(username, post_ids)
  end

  def get_save_posts(username) do
    :post_server.get_save_posts(username)
  end
end
