defmodule Core.PostClient do
  @moduledoc """
  This module facilitates communication with Erlang functions using GenServer.
  """
  @doc """
    iex> Core.PostClient.create("username", "Hello World", "#world", "@friend", "https://mazaryn.io")
    ~c"zKegB4mWRXP3PDVuntpnA"
  """
  def create(author, content, media \\ [], hashtag \\ "#mazaryn", link_url, emoji, mention) do
    content_erlang = String.to_charlist(content)
    :post_server.insert(author, content_erlang, media, hashtag, link_url, emoji, mention)
  end

  def modify_post(post_id, author, newContent, newEmoji, newMedia, newHashtag, newMention, newLink_url) do
    content_erlang = String.to_charlist(newContent)
    :post_server.modify_post(
      post_id, author, content_erlang, newEmoji, newMedia, newHashtag, newMention, newLink_url
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

  def get_user_id_by_post_id(post_id) do
    :postdb.get_user_id_by_post_id(post_id)
  end

  def get_file_from_ipfs(user_id, cid) do
    :go_libp2p.get_file_from_ipfs(user_id, cid)
  end

  ## Get all posts of specific user using username
  def get_posts_by_author(author) do
    :post_server.get_posts_by_author(author)
  end

  def get_posts_by_user_id(user_id) do
    :post_server.get_posts_by_user_id(user_id)
  end

  def get_posts_content_by_author(author) do
    :post_server.get_posts_content_by_author(author)
  end

  def get_posts_content_by_user_id(user_id) do
    :post_server.get_posts_content_by_user_id(user_id)
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

  def upload_media(filepath) do
    :ipfs_media.upload_media([filepath])
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
  postID_charlist =
    case postID do
      id when is_binary(id) -> String.to_charlist(id)
      id when is_list(id) -> id
    end

  case :post_server.add_comment(author, postID_charlist, content) do
    comment_id when is_list(comment_id) -> List.to_string(comment_id)
    error -> error
  end
end

  def update_comment(commentID, newContent) do
    :post_server.update_comment(commentID, newContent)
  end

  def get_comment_content(comment_id) do
  comment_id_charlist =
    case comment_id do
      id when is_binary(id) -> String.to_charlist(id)
      id when is_list(id) -> id
    end

  case :post_server.get_comment_content(comment_id_charlist) do
    content when is_list(content) -> List.to_string(content)
    error -> error
  end
end

  def like_comment(userID, commentID) do
    :post_server.like_comment(userID, commentID)
  end

  def get_comment_likes(commentID) do
    :post_server.get_comment_likes(commentID)
  end

  def reply_comment(userID, commentID, content) do
    :post_server.reply_comment(userID, commentID, content)
  end

  def get_reply(replyID) do
    :post_server.get_reply(replyID)
  end

  def get_reply_content(reply_id) when is_binary(reply_id) do
    reply_id
    |> String.to_charlist()
    |> :post_server.get_reply_content()
    |> handle_reply_content()
  end

  def get_reply_content(reply_id) when is_list(reply_id) do
    :post_server.get_reply_content(reply_id)
    |> handle_reply_content()
  end

  defp handle_reply_content(result) do
    case result do
      {:error, reason} -> {:error, reason}
      charlist when is_list(charlist) -> List.to_string(charlist)
      string when is_binary(string) -> string
      other -> other
    end
  end

  def delete_reply(replyID) do
    :post_server.delete_reply(replyID)
  end

  def get_all_replies(commentID) do
    :post_server.get_all_replies(commentID)
  end

  ## Get specific comment using CommentID
  def get_single_comment(commentId) do
    :post_server.get_single_comment(commentId)
  end

  def get_user_by_single_comment(commentID) do
    :post_server.get_user_by_single_comment(commentID)
  end

  ## Get all comments related to specific post using PostID
  def get_all_comments(postId) do
    :post_server.get_all_comments(postId)
  end

  def get_all_comments_by_user_id(post_id, user_id) do
    :post_Server.get_all_comments_by_user_id(post_id, user_id)
  end

  ## Remove comment Permanently using CommentID and related PostID
  def delete_comment(commentID, postId) do
    :post_server.delete_comment(commentID, postId)
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

  def get_media(post_id) do
    :post_server.get_media(post_id)
  end

  def display_media(media_binary) do
    :post_server.display_media(media_binary)
  end

  ## Report specific post using MyID, PostID, Type(Spam, Harassment, Violence ..) and Content
  def report_post(my_id, post_id, type, description) do
    :post_server.report_post(my_id, post_id, type, description)
  end

  def translate_post(postID, target) do
    text =  :postdb.get_post_content_by_id(postID)
    Translator.translate_text(text, "en", target)
  end

  def get_media_cid(post_id) do
    cid = :postdb.get_media_cid(post_id)

    cid = if is_tuple(cid) do
      elem(cid, 1)
    else
      cid
    end

    cid
    |> List.to_string()
  end

  def display_real_media(media_binary) do
    base64_image = Base.encode64(media_binary)

    content_type = detect_content_type(media_binary)

    "data:#{content_type};base64,#{base64_image}"
  end

  defp detect_content_type(<<0xFF, 0xD8, 0xFF, _::binary>>), do: "image/jpeg"
  defp detect_content_type(<<0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, _::binary>>), do: "image/png"
  defp detect_content_type(<<0x47, 0x49, 0x46, 0x38, _::binary>>), do: "image/gif"
  defp detect_content_type(<<0x42, 0x4D, _::binary>>), do: "image/bmp"
  defp detect_content_type(<<0x52, 0x49, 0x46, 0x46, _::binary>>), do: "image/webp"
  defp detect_content_type(_), do: "image/jpeg"

  def get_ipns_from_post(post_id) when is_list(post_id) do
    post_id
    |> :post_server.get_ipns_from_post()
    |> List.to_string()
  end

  def get_ipns_from_post(post_id) when is_binary(post_id) do
    post_id
    |> String.to_charlist()
    |> :post_server.get_ipns_from_post()
    |> List.to_string()
  end

end
