defmodule Core.PostClient do
  @moduledoc """
  This module facilitates communication with Erlang functions using GenServer.
  """
  @doc """
    iex> Core.PostClient.create("username", "Hello World", "#world", "@friend", "https://mazaryn.io")
    ~c"zKegB4mWRXP3PDVuntpnA"
  """
  def create(author, content, emoji, media \\ [], hashtag, mention, link_url) do
    :post_server.insert(author, content, emoji, media, hashtag, mention, link_url)
  end

  def modify_post(author, newContent, newEmoji, newMedia, newHashtag, newMention, newLink_url) do
    :post_server.modify_post(
      author,
      newContent,
      newEmoji,
      newMedia,
      newHashtag,
      newMention,
      newLink_url
    )
  end

  ## Get post information using PostID
  def get_by_id(id) do
    :post_server.get_post_by_id(id)
  end

  ## Get post's Content using PostID
  def get_post_content_by_id(id) do
    :post_server.get_post_content_by_id(id)
  end

  ## Get all posts of specific user using username
  def get_posts_by_author(author) do
    :post_server.get_posts_by_author(author)
  end

  def get_posts_content_by_author(author) do
    :post_server.get_posts_content_by_author(author)
  end

  ## Get all posts related to Specific Hashtag
  def get_posts_by_hashtag(hashtag) do
    :post_server.get_posts_by_hashtag(hashtag)
  end

  ## Get 5 latest posts of specific user using username
  def get_latest_posts(author) do
    :post_server.get_latest_posts(author)
  end

  ## Update post using PostID and NewContent
  def update_post(postId, newContent) do
    :post_server.update_post(postId, newContent)
  end

  ## Remove post permanently
  def delete_post(id) do
    :post_server.delete_post(id)
  end

  ## Get all available posts in the network
  def get_posts() do
    :post_server.get_posts()
  end

  ## Like post using MyID and PostID
  def like_post(userID, postId) do
    :post_server.like_post(userID, postId)
  end

  ## Unlike post using MyID and PostID
  def unlike_post(likeId, postId) do
    :post_server.unlike_post(likeId, postId)
  end

  ## Add comment to post using My Username, PostID and Comment's Content
  def add_comment(author, postID, content) do
    :post_server.add_comment(author, postID, content)
  end

  ## Update comment using CommentID and New Content
  def update_comment(commentID, newContent) do
    :post_server.update_comment(commentID, newContent)
  end

  ## Get specific comment using CommentID
  def get_single_comment(commentId) do
    :post_server.get_single_comment(commentId)
  end

  ## Get all comments related to specific post using PostID
  def get_all_comments(postId) do
    :post_server.get_all_comments(postId)
  end

  ## Remove comment Permanently using CommentID and related PostID
  def delete_comment(commentID, postId) do
    :post_server.delete_comment(commentID, postId)
  end

  def delete_comment_from_mnesia(commentID) do
    :post_server.delete_comment_from_mnesia(commentID)
  end

  ## Get all likes for specific post using PostID
  def get_likes(postID) do
    :post_server.get_likes(postID)
  end

  ## Get posts for an specific time
  def get_all_posts_from_date(year, month, date, author) do
    :post_server.get_all_posts_from_date(year, month, date, author)
  end

  def get_all_posts_from_month(year, month, author) do
    :post_server.get_all_posts_from_month(year, month, author)
  end

  ## Get all comments for specific post using PostID
  def get_comments(postID) do
    :post_server.get_all_comments(postID)
  end

  def save_post(username, post_id) do
    :user_server.save_post(username, post_id)
  end

  def unsave_post(username, post_id) do
    :user_server.unsave_post(username, post_id)
  end

  def save_posts(username, post_ids) do
    :user_server.save_posts(username, post_ids)
  end

  def unsave_posts(username, post_ids) do
    :user_server.unsave_posts(username, post_ids)
  end

  def get_save_posts(username) do
    :user_server.get_save_posts(username)
  end

  ## Report specific post using MyID, PostID, Type(Spam, Harassment, Violence ..) and Content
  def report_post(my_id, post_id, type, description) do
    :post_server.report_post(my_id, post_id, type, description)
  end
end
