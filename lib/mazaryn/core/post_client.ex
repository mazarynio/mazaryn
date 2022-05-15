defmodule Core.PostClient do

  def start do
    :post_server.start_link()
    :post_server.init([])
  end

  def create_post(author, content) do
    :post_server.insert(Author, Content)
  end

  def get_post_by_id(id) do
    :post_server.get_post_by_id(Id)
  end

  def get_posts_by_author(author) do
    :post_server.get_posts_by_author(Author)
  end

  def delete_post(id) do
    :post_server.delete_post(Id)
  end

  def add_comment(id, username, comment) do
    :post_server.add_comment(Id, Username, Comment)
  end

  def get_all_posts_from_date(year, month, date, author) do
    :post_server.get_all_posts_from_date(Year, Month, Date, Author)
  end

  def get_all_posts_from_month(year, month, author) do
    :post_server.get_all_posts_from_month(Year, Month, Author)
  end

  def get_comments(id) do
    :post_server.get_comments(Id)
  end

  def save_post(username, post_id) do
    :post_server.save_post(Username, PostId)
  end

  def unsave_post(username, post_id) do
    :post_server.unsave_post(Username, PostId)
  end

  def save_posts(username, post_ids) do
    :post_server.save_posts(Username, PostIds)
  end

  def unsave_posts(username, post_ids) do
    :post_server.unsave_posts(Username, PostIds)
  end

  def get_save_posts(username) do
    :post_server.get_save_posts(Username)
  end

end
