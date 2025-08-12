defmodule Resolvers.PostResolver do
  alias Mazaryn.Posts
  alias Mazaryn.Comments
  alias Mazaryn.Schema.{Post, Comment, Reply}
  alias Core.PostClient

  require Logger

  defp format_date_field(item, field) when is_map(item) do
    case Map.get(item, field) do
      %DateTime{} = dt ->
        Map.put(item, field, DateTime.to_iso8601(dt))

      %NaiveDateTime{} = dt ->
        Map.put(item, field, NaiveDateTime.to_iso8601(dt))

      {{year, month, day}, {hour, minute, second}} ->
        formatted_date = "#{year}-#{pad_zero(month)}-#{pad_zero(day)} #{pad_zero(hour)}:#{pad_zero(minute)}:#{pad_zero(second)}"
        Map.put(item, field, formatted_date)

      nil ->
        Map.put(item, field, nil)

      other when is_binary(other) ->
        item

      other ->
        Map.put(item, field, to_string(other))
    end
  end

  defp format_date_field(item, _field), do: item

  defp pad_zero(num) when num < 10, do: "0#{num}"
  defp pad_zero(num), do: to_string(num)

  defp format_post(post) when is_map(post) do
    post
    |> format_date_field(:date_created)
    |> format_date_field(:date_updated)
  end

  defp format_post(post), do: post

  defp format_comment(comment) when is_map(comment) do
    comment
    |> format_date_field(:date_created)
  end

  defp format_comment(comment), do: comment

  defp format_reply(reply) when is_map(reply) do
    reply
    |> format_date_field(:date_created)
  end

  defp format_reply(reply), do: reply

  # Post operations
  def create(_parent, %{input: input}, _resolution) do
    changeset = Post.changeset(%Post{}, input)

    case Posts.create_post(changeset) do
      {:ok, post} ->
        {:ok, format_post(post)}

      {:error, changeset} when is_map(changeset) ->
        formatted_errors = format_changeset_errors(changeset)
        error_message = format_error_message(formatted_errors)
        {:error, error_message}

      %Ecto.Changeset{valid?: false} = invalid_changeset ->
        formatted_errors = format_changeset_errors(invalid_changeset)
        error_message = format_error_message(formatted_errors)
        {:error, error_message}

      {:error, reason} when is_binary(reason) ->
        {:error, reason}

      {:error, reason} ->
        {:error, "Failed to create post: #{inspect(reason)}"}
    end
  end

  def all(_parent, _args, _resolution) do
    posts = Posts.get_posts()
    formatted_posts = Enum.map(posts, &format_post/1)
    {:ok, formatted_posts}
  end

  def get_post_by_id(_parent, %{id: id}, _resolution) do
    case Posts.one_by_id(id) do
      {:ok, post} -> {:ok, format_post(post)}
      {:error, _reason} -> {:error, "Post not found"}
      nil -> {:error, "Post not found"}
    end
  end

  def get_posts_by_author(_parent, %{author: author}, _resolution) do
    posts = Posts.get_posts_by_author(author)
    formatted_posts = Enum.map(posts, &format_post/1)
    {:ok, formatted_posts}
  end

  def get_posts_by_user_id(_parent, %{user_id: user_id}, _resolution) do
    posts = Posts.get_posts_by_user_id(user_id)
    formatted_posts = Enum.map(posts, &format_post/1)
    {:ok, formatted_posts}
  end

  def get_posts_by_hashtag(_parent, %{hashtag: hashtag}, _resolution) do
    posts = Posts.get_posts_by_hashtag(hashtag)
    formatted_posts = Enum.map(posts, &format_post/1)
    {:ok, formatted_posts}
  end

  def get_home_posts(_parent, _args, _resolution) do
    posts = Posts.get_home_posts()
    formatted_posts = Enum.map(posts, &format_post/1)
    {:ok, formatted_posts}
  end

  def update_post(_parent, %{input: input}, _resolution) do
    %{post_id: post_id, author: author} = input

    new_content = Map.get(input, :content)
    new_emoji = Map.get(input, :emoji)
    new_media = Map.get(input, :media, [])
    new_hashtag = Map.get(input, :hashtag)
    new_mention = Map.get(input, :mention)
    new_link_url = Map.get(input, :link_url)

    case PostClient.modify_post(post_id, author, new_content, new_emoji, new_media, new_hashtag, new_mention, new_link_url) do
      :ok ->
        case Posts.one_by_id(post_id) do
          {:ok, post} -> {:ok, format_post(post)}
          error -> error
        end

      {:error, reason} ->
        {:error, "Failed to update post: #{inspect(reason)}"}
    end
  end

  def delete_post(_parent, %{id: id}, _resolution) do
    case PostClient.delete_post(id) do
      :ok -> {:ok, %{success: true, message: "Post deleted successfully"}}
      {:error, reason} -> {:error, "Failed to delete post: #{inspect(reason)}"}
      _ -> {:error, "Failed to delete post"}
    end
  end

  # Comment operations
  def create_comment(_parent, %{input: input}, _resolution) do
    changeset = Comment.changeset(%Comment{}, input)

    case Posts.create_comment(changeset) do
      %Ecto.Changeset{} = result_changeset ->
        case Comment.build(result_changeset) do
          {:ok, comment} -> {:ok, format_comment(comment)}
          {:error, reason} -> {:error, "Failed to build comment: #{inspect(reason)}"}
        end

      {:error, changeset} when is_map(changeset) ->
        formatted_errors = format_changeset_errors(changeset)
        error_message = format_error_message(formatted_errors)
        {:error, error_message}

      %Ecto.Changeset{valid?: false} = invalid_changeset ->
        formatted_errors = format_changeset_errors(invalid_changeset)
        error_message = format_error_message(formatted_errors)
        {:error, error_message}

      {:error, reason} ->
        {:error, "Failed to create comment: #{inspect(reason)}"}
    end
  end

  def get_comments_by_post(_parent, %{post_id: post_id}, _resolution) do
    comments = Posts.get_comment_by_post_id(post_id)
    formatted_comments = Enum.map(comments, &format_comment/1)
    {:ok, formatted_comments}
  end

  def get_comment_by_id(_parent, %{id: id}, _resolution) do
    case Comments.one_by_id(id) do
      {:ok, comment} -> {:ok, format_comment(comment)}
      {:error, _reason} -> {:error, "Comment not found"}
      nil -> {:error, "Comment not found"}
    end
  end

  def update_comment(_parent, %{input: input}, _resolution) do
    changeset = Comment.update_changeset(%Comment{}, input)

    case Posts.update_comment(changeset) do
      %Ecto.Changeset{} = result_changeset ->
        case Comment.build(result_changeset) do
          {:ok, comment} -> {:ok, format_comment(comment)}
          {:error, reason} -> {:error, "Failed to build comment: #{inspect(reason)}"}
        end

      {:error, changeset} when is_map(changeset) ->
        formatted_errors = format_changeset_errors(changeset)
        error_message = format_error_message(formatted_errors)
        {:error, error_message}

      %Ecto.Changeset{valid?: false} = invalid_changeset ->
        formatted_errors = format_changeset_errors(invalid_changeset)
        error_message = format_error_message(formatted_errors)
        {:error, error_message}

      {:error, reason} ->
        {:error, "Failed to update comment: #{inspect(reason)}"}
    end
  end

  def delete_comment(_parent, %{comment_id: comment_id, post_id: post_id}, _resolution) do
    case PostClient.delete_comment(comment_id, post_id) do
      :ok -> {:ok, %{success: true, message: "Comment deleted successfully"}}
      {:error, reason} -> {:error, "Failed to delete comment: #{inspect(reason)}"}
      _ -> {:error, "Failed to delete comment"}
    end
  end

  # Reply operations
  def create_reply(_parent, %{input: input}, _resolution) do
    %{user_id: user_id, comment_id: comment_id, content: content} = input

    case PostClient.reply_comment(user_id, comment_id, content) do
      reply_id when is_binary(reply_id) ->
        case get_reply_by_id(reply_id) do
          {:ok, reply} -> {:ok, format_reply(reply)}
          error -> error
        end

      {:error, reason} ->
        {:error, "Failed to create reply: #{inspect(reason)}"}
    end
  end

  def get_replies_by_comment(_parent, %{comment_id: comment_id}, _resolution) do
    case PostClient.get_all_replies(comment_id) do
      replies when is_list(replies) ->
        formatted_replies =
          replies
          |> Enum.map(&get_reply_by_id/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            _ -> false
          end)
          |> Enum.map(fn {:ok, reply} -> format_reply(reply) end)

        {:ok, formatted_replies}

      {:error, reason} ->
        {:error, "Failed to get replies: #{inspect(reason)}"}
    end
  end

  def get_reply_by_id(_parent, %{id: id}, _resolution) do
    get_reply_by_id(id)
  end

  defp get_reply_by_id(id) do
    case PostClient.get_reply(id) do
      erl_reply ->
        erl_reply
        |> Reply.erl_changeset()
        |> Reply.build()
    end
  end

  def delete_reply(_parent, %{id: id}, _resolution) do
    case PostClient.delete_reply(id) do
      :ok -> {:ok, %{success: true, message: "Reply deleted successfully"}}
      {:error, reason} -> {:error, "Failed to delete reply: #{inspect(reason)}"}
      _ -> {:error, "Failed to delete reply"}
    end
  end

  # Like operations
  def like_post(_parent, %{user_id: user_id, post_id: post_id}, _resolution) do
    case PostClient.like_post(user_id, post_id) do
      :ok -> {:ok, %{success: true, message: "Post liked successfully"}}
      {:error, reason} -> {:error, "Failed to like post: #{inspect(reason)}"}
      _ -> {:error, "Failed to like post"}
    end
  end

  def unlike_post(_parent, %{like_id: like_id, post_id: post_id}, _resolution) do
    case PostClient.unlike_post(like_id, post_id) do
      :ok -> {:ok, %{success: true, message: "Post unliked successfully"}}
      {:error, reason} -> {:error, "Failed to unlike post: #{inspect(reason)}"}
      _ -> {:error, "Failed to unlike post"}
    end
  end

  def get_post_likes(_parent, %{post_id: post_id}, _resolution) do
    likes = Posts.get_likes_by_post_id(post_id)
    {:ok, likes}
  end

  def like_comment(_parent, %{user_id: user_id, comment_id: comment_id}, _resolution) do
    case PostClient.like_comment(user_id, comment_id) do
      :ok -> {:ok, %{success: true, message: "Comment liked successfully"}}
      {:error, reason} -> {:error, "Failed to like comment: #{inspect(reason)}"}
      _ -> {:error, "Failed to like comment"}
    end
  end

  def get_comment_likes(_parent, %{comment_id: comment_id}, _resolution) do
    case PostClient.get_comment_likes(comment_id) do
      likes when is_list(likes) -> {:ok, likes}
      {:error, reason} -> {:error, "Failed to get comment likes: #{inspect(reason)}"}
      _ -> {:ok, []}
    end
  end

  defp format_changeset_errors(changeset) do
    changeset
    |> Ecto.Changeset.traverse_errors(fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end

  defp format_error_message(errors) when is_map(errors) do
    errors
    |> Enum.map(fn {field, messages} ->
      field_name = field |> to_string() |> String.capitalize()
      message_list = if is_list(messages), do: messages, else: [messages]
      "#{field_name}: #{Enum.join(message_list, ", ")}"
    end)
    |> Enum.join("; ")
  end

  defp format_error_message(error), do: to_string(error)
end
