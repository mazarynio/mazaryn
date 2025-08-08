defmodule MazarynWeb.HomeLive.CommentHandler do
  @moduledoc """
  Handles comment operations for the home live view.

  This module provides functionality for:
  - Creating, updating, and deleting comments
  - Managing comment replies
  - Handling comment validation and error states
  - Managing comment editing states
  - Like/unlike operations for comments
  """

  alias Core.PostClient
  alias Mazaryn.Schema.{Comment, Post, Reply}
  alias Mazaryn.Posts
  alias Account.Users
  alias Home.Like
  alias MazarynWeb.HomeLive.CommentUtilities

  require Logger

  @doc """
  Handles saving a new comment with comprehensive validation and error handling.
  """
  def handle_save_comment(%{"comment" => comment_params}) do
    start_time = System.monotonic_time(:millisecond)
    Logger.info("Starting comment save operation")

    with {:ok, validated_params} <- validate_comment_params(comment_params),
         {:ok, changeset} <- create_comment_changeset(validated_params),
         {:ok, result} <- save_comment_with_fallback(validated_params, changeset) do

      log_operation_time("Comment save", start_time)
      Logger.info("Comment saved successfully")
      {:ok, result}
    else
      {:error, reason} ->
        log_operation_time("Comment save (failed)", start_time)
        Logger.error("Comment save failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def handle_save_comment(invalid_params) do
    Logger.warning("Invalid comment parameters received: #{inspect(invalid_params)}")
    {:error, :invalid_params}
  end

  @doc """
  Handles updating an existing comment.
  """
  def handle_update_comment(%{"comment" => comment_params}, post_id) do
    Logger.info("Starting comment update operation")

    with {:ok, comment_id, new_content} <- extract_update_params(comment_params),
         {:ok, changeset} <- create_update_changeset(comment_params),
         {:ok, result} <- update_comment_with_fallback(comment_id, new_content, changeset, post_id) do

      Logger.info("Comment updated successfully")
      {:ok, result}
    else
      {:error, reason} ->
        Logger.error("Comment update failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Handles comment validation for real-time feedback.
  """
  def handle_validate_comment(%{"comment" => comment_params}) do
    changeset = Comment.changeset(%Comment{}, comment_params)
                |> Map.put(:action, :validate)

    {:ok, %{changeset: changeset}}
  end

  def handle_validate_update_comment(%{"comment" => comment_params}) do
    changeset = Comment.update_changeset(%Comment{}, comment_params)
                |> Map.put(:action, :validate)

    {:ok, %{update_comment_changeset: changeset}}
  end

  @doc """
  Handles entering edit mode for a comment.
  """
  def handle_edit_comment(%{"comment-id" => comment_id}) do
    Logger.info("Entering edit mode for comment: #{comment_id}")

    {:ok, %{
      editing_comment: true,
      editing_comment_id: comment_id
    }}
  end

  @doc """
  Handles cancelling comment edit mode.
  """
  def handle_cancel_comment_edit(_params) do
    Logger.info("Cancelling comment edit mode")

    {:ok, %{
      editing_comment: false,
      editing_comment_id: nil
    }}
  end

  @doc """
  Handles deleting a comment with optimistic updates.
  """
  def handle_delete_comment(%{"comment-id" => comment_id, "post-id" => post_id}, comments) do
    Logger.info("Deleting comment #{comment_id} with optimistic update")

    comment_to_delete = find_comment_by_id(comments, comment_id)
    updated_comments = remove_comment_from_list(comments, comment_id)

    spawn_comment_deletion_task(comment_id, comment_to_delete)

    {:ok, %{comments: updated_comments}}
  end

  @doc """
  Handles liking a comment.
  """
  def handle_like_comment(%{"comment-id" => comment_id}, post_id, user_id) do
    comment_id_charlist = to_charlist(comment_id)

    PostClient.like_comment(user_id, comment_id_charlist)

    post = CommentUtilities.rebuild_post(post_id)
    comments = get_comments_with_content(post_id)

    {:ok, %{
      post: post,
      comments: comments
    }}
  end

  @doc """
  Handles unliking a comment.
  """
  def handle_unlike_comment(%{"comment-id" => comment_id}, post_id, user_id, comments) do
    comment_id_charlist = to_charlist(comment_id)
    user_id_charlist = to_charlist(user_id)

    comment = find_comment_by_id(comments, comment_id_charlist)

    like = comment_id_charlist
           |> PostClient.get_comment_likes()
           |> Enum.map(&CommentUtilities.build_like/1)
           |> Enum.find(&(&1.user_id == user_id_charlist))

    if like do
      updated_likes = Enum.reject(comment.likes, &(&1 == like.id))
      :postdb.update_comment_likes(comment_id_charlist, updated_likes)
    end

    post = CommentUtilities.rebuild_post(post_id)
    fresh_comments = get_comments_with_content(post_id)

    {:ok, %{
      post: post,
      comments: fresh_comments
    }}
  end

  # Reply Management

  @doc """
  Handles entering reply mode for a comment.
  """
  def handle_reply_comment(%{"comment-id" => comment_id}) do
    Logger.info("Setting reply state for comment: #{comment_id}")

    {:ok, %{
      reply_comment: true,
      replying_to_comment_id: to_charlist(comment_id)
    }}
  end

  @doc """
  Handles cancelling reply mode.
  """
  def handle_cancel_comment_reply(_params) do
    Logger.info("Cancelling comment reply")

    {:ok, %{
      reply_comment: false,
      replying_to_comment_id: nil
    }}
  end

  @doc """
  Handles creating a reply to a comment.
  """
  def handle_reply_comment_content(%{"comment" => comment_params}, user_id, comments) do
    case create_reply(comment_params, user_id, comments) do
      {:ok, result} ->
        {:ok, result}
      {:error, _reason} ->
        {:error, %{flash: {:error, "Failed to create reply"}}}
    end
  end

  @doc """
  Handles deleting a reply with optimistic updates.
  """
  def handle_delete_reply(%{"reply-id" => reply_id, "comment-id" => comment_id}, comments) do
    Logger.info("Deleting reply #{reply_id} from comment #{comment_id}")

    reply_to_delete = find_reply_in_comments(comments, comment_id, reply_id)
    updated_comments = remove_reply_from_comments(comments, comment_id, reply_id)

    spawn_reply_deletion_task(reply_id, comment_id, reply_to_delete)

    {:ok, %{comments: updated_comments}}
  end

  # Comment Retrieval

  @doc """
  Gets comments with content for a specific post, with caching and optimization.
  """
  def get_comments_with_content(post_id) do
    CommentUtilities.get_comments_with_content_optimized(post_id)
  end

  @doc """
  Gets comments with content for a specific post - alias for compatibility.
  """
  def get_comments_with_content_optimized(post_id) do
    CommentUtilities.get_comments_with_content_optimized(post_id)
  end

  def handle_show_comments(%{"id" => post_id}) do
    comments = get_comments_with_content(post_id)
    {:ok, %{comments: comments}}
  end

  # Private Functions

  defp validate_comment_params(params) do
    post_id = String.trim(params["post_id"] || "")
    author = String.trim(params["author"] || "")
    content = String.trim(params["content"] || "")

    case {post_id, author, content} do
      {"", _, _} ->
        {:error, :missing_post_id}

      {_, "", _} ->
        {:error, :missing_author}

      {_, _, ""} ->
        changeset = Comment.changeset(%Comment{}, params)
                    |> Map.put(:action, :validate)
                    |> Ecto.Changeset.add_error(:content, "can't be blank")
        {:error, {:validation, changeset}}

      {valid_post_id, valid_author, valid_content} ->
        {:ok, %{
          post_id: valid_post_id,
          author: valid_author,
          content: valid_content
        }}
    end
  end

  defp create_comment_changeset(params) do
    changeset = Comment.changeset(%Comment{}, params)

    if changeset.valid? do
      {:ok, changeset}
    else
      {:error, {:changeset, changeset}}
    end
  end

  defp save_comment_with_fallback(params, changeset) do
    current_comments = CommentUtilities.get_comments_with_content_optimized(params.post_id)

    case Posts.create_comment(changeset) do
      {:ok, comment} ->
        handle_successful_comment_save(params, comment, current_comments)

      {:error, changeset} ->
        {:error, {:changeset, changeset}}

      %{} ->
        Logger.warning("Posts.create_comment returned empty map, trying alternative")
        handle_alternative_save(params, current_comments)

      other ->
        Logger.warning("Unexpected result from Posts.create_comment: #{inspect(other)}")
        handle_alternative_save(params, current_comments)
    end
  end

  defp handle_successful_comment_save(params, comment, current_comments) do
    CommentUtilities.cache_content(:comment, comment.id, params.content)

    new_comment = build_comment_map(comment, params)
    updated_comments = current_comments ++ [new_comment]

    CommentUtilities.clear_comments_cache(params.post_id)
    post = CommentUtilities.rebuild_post(params.post_id)

    spawn_comment_sync_task(params.post_id, comment.id)

    {:ok, %{
      post: post,
      comments: updated_comments,
      changeset: Comment.changeset(%Comment{}),
      flash: {:info, "Comment saved!"}
    }}
  end

  defp handle_alternative_save(params, current_comments) do
    temp_comment = create_optimistic_comment(params.post_id, params.author, params.content)
    updated_comments = current_comments ++ [temp_comment]

    CommentUtilities.cache_content(:comment, temp_comment.id, params.content)
    spawn_async_comment_save(temp_comment, params)

    {:ok, %{
      post: CommentUtilities.rebuild_post(params.post_id),
      comments: updated_comments,
      changeset: Comment.changeset(%Comment{}),
      flash: {:info, "Comment saved!"}
    }}
  end

  defp create_optimistic_comment(post_id, author, content) do
    temp_id = "temp_" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))

    %{
      id: temp_id,
      post_id: post_id,
      author: author,
      content: content,
      inserted_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now(),
      likes: [],
      replies: [],
      like_comment_event: "like-comment",
      is_temp: true
    }
  end

  defp build_comment_map(comment, params) do
    %{
      id: comment.id,
      post_id: params.post_id,
      author: comment.author || params.author,
      content: params.content,
      inserted_at: comment.inserted_at || DateTime.utc_now(),
      updated_at: comment.updated_at || DateTime.utc_now(),
      likes: comment.likes || [],
      replies: [],
      like_comment_event: "like-comment",
      is_temp: false
    }
  end

  defp extract_update_params(params) do
    comment_id = params["id"]
    new_content = String.trim(params["content"] || "")

    cond do
      is_nil(comment_id) ->
        {:error, :missing_comment_id}

      new_content == "" ->
        {:error, :empty_content}

      true ->
        {:ok, comment_id, new_content}
    end
  end

  defp create_update_changeset(params) do
    changeset = Comment.update_changeset(%Comment{}, params)

    if changeset.valid? do
      {:ok, changeset}
    else
      {:error, changeset}
    end
  end

  defp update_comment_with_fallback(comment_id, new_content, changeset, post_id) do
    case Posts.update_comment(changeset) do
      {:ok, updated_comment} ->
        handle_successful_update(comment_id, new_content, updated_comment)

      {:error, changeset} ->
        {:error, %{
          update_comment_changeset: changeset,
          flash: {:error, "Failed to update comment"}
        }}

      %{} ->
        Logger.warning("Posts.update_comment returned empty map, trying direct method")
        update_comment_direct(comment_id, new_content, post_id)

      _other ->
        Logger.warning("Unexpected return from Posts.update_comment, trying direct method")
        update_comment_direct(comment_id, new_content, post_id)
    end
  end

  defp handle_successful_update(comment_id, new_content, updated_comment) do
    CommentUtilities.cache_content(:comment, comment_id, new_content)
    post = CommentUtilities.rebuild_post(updated_comment.post_id)

    {:ok, %{
      post: post,
      comment_id: comment_id,
      new_content: new_content,
      update_comment_changeset: Comment.changeset(%Comment{}),
      editing_comment: false,
      editing_comment_id: nil,
      flash: {:info, "Comment updated successfully"}
    }}
  end

  defp update_comment_direct(comment_id, new_content, _post_id) do
    CommentUtilities.cache_content(:comment, comment_id, new_content)

    comment_id_charlist = if is_binary(comment_id), do: to_charlist(comment_id), else: comment_id

    spawn_update_task(comment_id_charlist, new_content)

    {:ok, %{
      comment_id: comment_id,
      new_content: new_content,
      update_comment_changeset: Comment.changeset(%Comment{}),
      editing_comment: false,
      editing_comment_id: nil,
      flash: {:info, "Comment updated successfully"}
    }}
  end

  defp create_reply(params, user_id, comments) do
    comment_id = params["comment_id"]
    content = params["content"]

    Logger.info("Creating reply for comment #{comment_id}")

    temp_reply = CommentUtilities.create_temp_reply(user_id, comment_id, content)
    updated_comments = add_reply_to_comment(comments, comment_id, temp_reply)

    spawn_reply_save_task(temp_reply, user_id, comment_id, content)

    {:ok, %{
      comments: updated_comments,
      reply_comment: false,
      replying_to_comment_id: nil
    }}
  end

  # Background Tasks

  defp spawn_comment_sync_task(post_id, comment_id) do
    parent_pid = self()

    Task.start(fn ->
      Process.sleep(1000)
      Logger.info("Starting background sync for comment #{comment_id}")

      fresh_comments = CommentUtilities.fetch_and_process_comments_improved(
        post_id,
        {:comments_processed, post_id}
      )

      if length(fresh_comments) > 0 do
        send(parent_pid, {:comments_synced, post_id, fresh_comments})
        Logger.info("Background sync completed for post #{post_id}")
      end
    end)
  end

  defp spawn_async_comment_save(temp_comment, params) do
    parent_pid = self()

    Task.start(fn ->
      Logger.info("Starting background save for temp comment #{temp_comment.id}")

      case CommentUtilities.save_comment_directly(
        temp_comment.id, params.post_id, params.author, params.content
      ) do
        {:ok, real_comment} ->
          CommentUtilities.cache_content(:comment, real_comment.id, params.content)
          send(parent_pid, {:temp_comment_saved, temp_comment.id, real_comment})
          Logger.info("Background save succeeded: #{temp_comment.id} -> #{real_comment.id}")

        {:error, reason} ->
          send(parent_pid, {:temp_comment_save_failed, temp_comment.id, reason})
          Logger.error("Background save failed for #{temp_comment.id}: #{inspect(reason)}")
      end
    end)
  end

  defp spawn_comment_deletion_task(comment_id, comment_to_delete) do
    component_pid = self()

    Task.start(fn ->
      try do
        comment_id_charlist = to_charlist(comment_id)
        :postdb.delete_comment_from_mnesia(comment_id_charlist)

        CommentUtilities.clear_content_cache(:comment, comment_id_charlist)
        CommentUtilities.clear_content_cache(:comment_status, comment_id_charlist)

        Logger.info("Comment #{comment_id} deleted successfully from backend")
        send(component_pid, {:comment_deleted_success, comment_id})
      rescue
        e ->
          Logger.error("Failed to delete comment #{comment_id}: #{inspect(e)}")
          send(component_pid, {:comment_deletion_failed, comment_id, comment_to_delete})
      end
    end)
  end

  defp spawn_reply_deletion_task(reply_id, comment_id, reply_to_delete) do
    component_pid = self()

    Task.start(fn ->
      try do
        reply_id_charlist = to_charlist(reply_id)
        :postdb.delete_reply_from_mnesia(reply_id_charlist)

        CommentUtilities.clear_content_cache(:reply, reply_id_charlist)

        Logger.info("Reply #{reply_id} deleted successfully from backend")
        send(component_pid, {:reply_deleted_success, reply_id, comment_id})
      rescue
        e ->
          Logger.error("Failed to delete reply #{reply_id}: #{inspect(e)}")
          send(component_pid, {:reply_deletion_failed, reply_id, comment_id, reply_to_delete})
      end
    end)
  end

  defp spawn_reply_save_task(temp_reply, user_id, comment_id, content) do
    parent_pid = self()

    Task.start(fn ->
      case PostClient.reply_comment(user_id, to_charlist(comment_id), content) do
        {:ok, reply} ->
          Logger.info("Reply saved successfully: #{inspect(reply.id)}")

          if reply && reply.id do
            CommentUtilities.cache_content(:reply, reply.id, content)
          end

          send(parent_pid, {:reply_saved, comment_id, reply, temp_reply.id})

        error ->
          Logger.error("Error saving reply: #{inspect(error)}")
          send(parent_pid, {:reply_failed, comment_id, temp_reply.id})
      end
    end)
  end

  defp spawn_update_task(comment_id_charlist, new_content) do
    Task.start(fn ->
      try do
        case :postdb.update_comment_content(comment_id_charlist, new_content) do
          :ok ->
            Logger.info("Comment updated successfully in backend")
          error ->
            Logger.warning("Backend update failed: #{inspect(error)}")
        end
      rescue
        e ->
          Logger.error("Exception during backend update: #{inspect(e)}")
      end
    end)
  end

  # Helper Functions

  defp add_reply_to_comment(comments, comment_id, new_reply) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        existing_replies = comment.replies || []
        updated_replies = existing_replies ++ [new_reply]

        Logger.info("Added reply to comment #{comment_id}. Total replies: #{length(updated_replies)}")
        Map.put(comment, :replies, updated_replies)
      else
        comment
      end
    end)
  end

  defp remove_reply_from_comments(comments, comment_id, reply_id) do
    Logger.info("Optimistically removing reply #{reply_id} from comment #{comment_id}")

    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        updated_replies = Enum.reject(comment.replies || [], fn reply ->
          to_string(reply.id) == to_string(reply_id)
        end)

        %{comment | replies: updated_replies}
      else
        comment
      end
    end)
  end

  defp remove_comment_from_list(comments, comment_id) do
    Logger.info("Optimistically removing comment #{comment_id}")

    Enum.reject(comments, fn comment ->
      to_string(comment.id) == to_string(comment_id)
    end)
  end

  defp find_comment_by_id(comments, comment_id) do
    Enum.find(comments, fn comment ->
      to_string(comment.id) == to_string(comment_id)
    end)
  end

  defp find_reply_in_comments(comments, comment_id, reply_id) do
    comments
    |> find_comment_by_id(comment_id)
    |> case do
      nil -> nil
      comment ->
        Enum.find(comment.replies || [], fn reply ->
          to_string(reply.id) == to_string(reply_id)
        end)
    end
  end

  defp log_operation_time(operation, start_time) do
    duration = System.monotonic_time(:millisecond) - start_time
    Logger.info("#{operation} completed in #{duration}ms")
  end
end
