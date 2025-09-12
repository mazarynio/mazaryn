defmodule MazarynWeb.HomeLive.CommentHandler do
  @moduledoc """
  Handles comment operations for the home live view with improved reliability.
  """
  alias Core.PostClient
  alias Mazaryn.Schema.{Comment, Post, Reply}
  alias Mazaryn.Posts
  alias Account.Users
  alias Home.Like
  alias MazarynWeb.HomeLive.CommentUtilities

  require Logger

  @comment_save_timeout 2000
  @content_fetch_timeout 800
  @reply_deletion_timeout 3000

  def handle_save_comment(%{"comment" => comment_params}) do
    start_time = System.monotonic_time(:millisecond)
    Logger.info("Starting optimized comment save operation")

    with {:ok, validated_params} <- validate_comment_params(comment_params),
         {:ok, changeset} <- create_comment_changeset(validated_params) do

      result = save_comment_with_immediate_content(validated_params, changeset)

      log_operation_time("Comment save", start_time)
      Logger.info("Comment saved successfully with immediate content")
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

  def handle_update_comment(%{"comment" => comment_params}, post_id) do
    Logger.info("Starting optimized comment update operation")

    with {:ok, comment_id, new_content} <- extract_update_params(comment_params),
         {:ok, changeset} <- create_update_changeset(comment_params) do

      result = update_comment_with_immediate_content(comment_id, new_content, changeset, post_id)

      Logger.info("Comment updated successfully with immediate content")
      {:ok, result}
    else
      {:error, reason} ->
        Logger.error("Comment update failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_comments_with_content(post_id) do
    CommentUtilities.get_comments_with_content_fast(post_id)
  end

  def get_comments_with_content_optimized(post_id) do
    CommentUtilities.get_comments_with_content_fast(post_id)
  end

  def handle_show_comments(%{"id" => post_id}) do
    comments = get_comments_with_content(post_id)
    {:ok, %{comments: comments}}
  end

  def handle_delete_comment(%{"comment-id" => comment_id, "post-id" => post_id}, comments) do
    Logger.info("Deleting comment #{comment_id} with optimistic update")

    comment_to_delete = find_comment_by_id(comments, comment_id)
    updated_comments = remove_comment_from_list(comments, comment_id)

    spawn_comment_deletion_task_fast(comment_id, comment_to_delete)

    {:ok, %{comments: updated_comments}}
  end

  def handle_delete_reply(%{"reply-id" => reply_id, "comment-id" => comment_id}, comments) do
    Logger.info("Starting enhanced reply deletion for reply #{reply_id} from comment #{comment_id}")

    reply_to_delete = find_reply_in_comments(comments, comment_id, reply_id)
    updated_comments = remove_reply_from_comments(comments, comment_id, reply_id)

    spawn_enhanced_reply_deletion_task(reply_id, comment_id, reply_to_delete)

    {:ok, %{comments: updated_comments}}
  end

  def handle_like_comment(%{"comment-id" => comment_id}, post_id, user_id) do
    comment_id_charlist = to_charlist(comment_id)

    Task.start(fn ->
      PostClient.like_comment(user_id, comment_id_charlist)
    end)

    case CommentUtilities.rebuild_post_safe(post_id) do
      {:ok, post} ->
        comments = get_comments_with_content(post_id)
        {:ok, %{post: post, comments: comments}}
      {:error, _reason} ->
        comments = get_comments_with_content(post_id)
        {:ok, %{
          post: %{id: post_id},
          comments: comments
        }}
    end
  end

  def handle_unlike_comment(%{"comment-id" => comment_id}, post_id, user_id, comments) do
    comment_id_charlist = to_charlist(comment_id)
    user_id_charlist = to_charlist(user_id)

    comment = find_comment_by_id(comments, comment_id_charlist)

    Task.start(fn ->
      try do
        like = comment_id_charlist
               |> PostClient.get_comment_likes()
               |> Enum.map(&CommentUtilities.build_like/1)
               |> Enum.find(&(&1.user_id == user_id_charlist))

        if like do
          updated_likes = Enum.reject(comment.likes, &(&1 == like.id))
          :postdb.update_comment_likes(comment_id_charlist, updated_likes)
        end
      rescue
        e ->
          Logger.error("Error in background unlike: #{inspect(e)}")
      end
    end)

    case CommentUtilities.rebuild_post_safe(post_id) do
      {:ok, post} ->
        fresh_comments = get_comments_with_content(post_id)
        {:ok, %{post: post, comments: fresh_comments}}
      {:error, _reason} ->
        fresh_comments = get_comments_with_content(post_id)
        {:ok, %{
          post: %{id: post_id},
          comments: fresh_comments
        }}
    end
  end

  def handle_reply_comment(%{"comment-id" => comment_id}) do
    Logger.info("Setting reply state for comment: #{comment_id}")

    {:ok, %{
      reply_comment: true,
      replying_to_comment_id: to_charlist(comment_id)
    }}
  end

  def handle_cancel_comment_reply(_params) do
    Logger.info("Cancelling comment reply")

    {:ok, %{
      reply_comment: false,
      replying_to_comment_id: nil
    }}
  end

  def handle_reply_comment_content(%{"comment" => comment_params}, user_id, comments) do
    case create_reply_with_immediate_content(comment_params, user_id, comments) do
      {:ok, result} ->
        {:ok, result}
      {:error, _reason} ->
        {:error, %{flash: {:error, "Failed to create reply"}}}
    end
  end

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

  def handle_edit_comment(%{"comment-id" => comment_id}) do
    Logger.info("Entering edit mode for comment: #{comment_id}")

    {:ok, %{
      editing_comment: true,
      editing_comment_id: comment_id
    }}
  end

  def handle_cancel_comment_edit(_params) do
    Logger.info("Cancelling comment edit mode")

    {:ok, %{
      editing_comment: false,
      editing_comment_id: nil
    }}
  end

  defp spawn_enhanced_reply_deletion_task(reply_id, comment_id, reply_to_delete) do
    parent_pid = self()

    Task.start(fn ->
      try do
        Logger.info("Starting enhanced reply deletion for #{reply_id}")

        deletion_attempts = [
          fn -> attempt_mnesia_reply_deletion(reply_id) end,
          fn -> attempt_postdb_reply_deletion(reply_id) end,
          fn -> attempt_comment_reply_cleanup(comment_id, reply_id) end
        ]

        results = Enum.map(deletion_attempts, fn attempt ->
          try do
            case attempt.() do
              :ok -> {:ok, :deleted}
              {:ok, _} -> {:ok, :deleted}
              {:error, reason} -> {:error, reason}
              other -> {:error, {:unexpected_result, other}}
            end
          rescue
            error -> {:error, {:exception, error}}
          end
        end)

        success_count = Enum.count(results, &match?({:ok, _}, &1))

        if success_count > 0 do
          Logger.info("Enhanced reply deletion succeeded for #{reply_id} (#{success_count} methods)")
          CommentUtilities.clear_content_cache_fast(:reply, reply_id)
          send(parent_pid, {:reply_deleted_success, reply_id, comment_id})
        else
          Logger.error("All enhanced reply deletion methods failed for #{reply_id}: #{inspect(results)}")
          send(parent_pid, {:reply_deletion_failed, reply_id, comment_id, reply_to_delete})
        end

        Process.sleep(1000)
        attempt_final_cleanup(reply_id, comment_id)

      rescue
        e ->
          Logger.error("Exception in enhanced reply deletion task: #{inspect(e)}")
          send(parent_pid, {:reply_deletion_failed, reply_id, comment_id, reply_to_delete})
      end
    end)
  end

  defp attempt_mnesia_reply_deletion(reply_id) do
    reply_id_charlist = to_charlist(reply_id)
    :postdb.delete_reply_from_mnesia(reply_id_charlist)
  end

  defp attempt_postdb_reply_deletion(reply_id) do
    reply_id_charlist = to_charlist(reply_id)

    case :postdb.delete_reply(reply_id_charlist) do
      :ok -> :ok
      error ->
        :postdb.remove_reply(reply_id_charlist)
    end
  end

  defp attempt_comment_reply_cleanup(comment_id, reply_id) do
    comment_id_charlist = to_charlist(comment_id)
    reply_id_charlist = to_charlist(reply_id)

    case :postdb.remove_reply_from_comment(comment_id_charlist, reply_id_charlist) do
      :ok -> :ok
      error ->
        current_replies = :postdb.get_comment_replies(comment_id_charlist)
        filtered_replies = Enum.reject(current_replies, fn reply ->
          case reply do
            {_, reply_id_from_db, _, _, _, _} ->
              to_string(reply_id_from_db) == to_string(reply_id)
            _ -> false
          end
        end)

        :postdb.update_comment_replies(comment_id_charlist, filtered_replies)
    end
  end

  defp attempt_final_cleanup(reply_id, comment_id) do
    try do
      reply_id_charlist = to_charlist(reply_id)
      comment_id_charlist = to_charlist(comment_id)

      case :postdb.get_reply_by_id(reply_id_charlist) do
        :reply_not_exist ->
          Logger.info("Final cleanup: Reply #{reply_id} successfully deleted")
          :ok
        _ ->
          Logger.warning("Final cleanup: Reply #{reply_id} still exists, attempting force delete")
          :postdb.force_delete_reply(reply_id_charlist)
      end
    rescue
      e ->
        Logger.warning("Final cleanup failed for reply #{reply_id}: #{inspect(e)}")
    end
  end

  defp create_reply_with_immediate_content(params, user_id, comments) do
    comment_id = params["comment_id"]
    content = params["content"]

    Logger.info("Creating reply for comment #{comment_id}")

    temp_reply = create_temp_reply_with_real_content(user_id, comment_id, content)
    updated_comments = add_reply_to_comment(comments, comment_id, temp_reply)

    spawn_reply_save_task_fast(temp_reply, user_id, comment_id, content)

    {:ok, %{
      comments: updated_comments,
      reply_comment: false,
      replying_to_comment_id: nil
    }}
  end

  defp create_temp_reply_with_real_content(user_id, comment_id, content) do
    temp_id = "temp_" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))

    CommentUtilities.cache_content_fast(:reply, temp_id, content)

    %{
      id: temp_id,
      content: content,
      user_id: user_id,
      comment_id: comment_id,
      inserted_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now(),
      is_temp: true
    }
  end

  defp spawn_comment_deletion_task_fast(comment_id, comment_to_delete) do
    Task.start(fn ->
      try do
        comment_id_charlist = to_charlist(comment_id)
        :postdb.delete_comment_from_mnesia(comment_id_charlist)
        CommentUtilities.clear_content_cache_fast(:comment, comment_id_charlist)
        Logger.info("Comment #{comment_id} deleted successfully from backend")
      rescue
        e ->
          Logger.error("Failed to delete comment #{comment_id}: #{inspect(e)}")
      end
    end)
  end

  defp spawn_reply_save_task_fast(temp_reply, user_id, comment_id, content) do
    Task.start(fn ->
      case PostClient.reply_comment(user_id, to_charlist(comment_id), content) do
        {:ok, reply} ->
          Logger.info("Reply saved successfully: #{inspect(reply.id)}")
          if reply && reply.id do
            CommentUtilities.cache_content_fast(:reply, reply.id, content)
          end
        error ->
          Logger.error("Error saving reply: #{inspect(error)}")
      end
    end)
  end

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

  defp save_comment_with_immediate_content(params, changeset) do
    current_comments = CommentUtilities.get_comments_with_content_fast(params.post_id)

    optimistic_comment = create_comment_with_real_content(params.post_id, params.author, params.content)
    updated_comments = current_comments ++ [optimistic_comment]

    CommentUtilities.cache_content_fast(:comment, optimistic_comment.id, params.content)

    spawn_background_database_save(optimistic_comment, params, changeset)

    CommentUtilities.clear_comments_cache(params.post_id)

    post = case CommentUtilities.rebuild_post_safe(params.post_id) do
      {:ok, post} -> post
      {:error, _reason} ->
        %{
          id: params.post_id,
          author: params.author,
          content: "Post content unavailable",
          inserted_at: DateTime.utc_now(),
          updated_at: DateTime.utc_now()
        }
    end

    %{
      post: post,
      comments: updated_comments,
      changeset: Comment.changeset(%Comment{}),
      flash: {:info, "Comment saved!"}
    }
  end

  defp update_comment_with_immediate_content(comment_id, new_content, changeset, post_id) do
    CommentUtilities.cache_content_fast(:comment, comment_id, new_content)

    spawn_background_update_attempt(comment_id, new_content)

    %{
      comment_id: comment_id,
      new_content: new_content,
      update_comment_changeset: Comment.changeset(%Comment{}),
      editing_comment: false,
      editing_comment_id: nil,
      flash: {:info, "Comment updated successfully"}
    }
  end

  defp create_comment_with_real_content(post_id, author, content) do
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

  defp spawn_background_database_save(optimistic_comment, params, changeset) do
    parent_pid = self()

    Task.start(fn ->
      try do
        Logger.info("Starting background database save for comment #{optimistic_comment.id}")

        result = attempt_database_save_with_timeout(changeset, @comment_save_timeout)

        case result do
          {:ok, real_comment} when is_map(real_comment) ->
            if Map.has_key?(real_comment, :id) and real_comment.id do
              CommentUtilities.cache_content_fast(:comment, real_comment.id, params.content)

              send(parent_pid, {:temp_comment_saved, optimistic_comment.id, real_comment})

              Logger.info("Background save succeeded: #{optimistic_comment.id} -> #{real_comment.id}")
            else
              Logger.warning("Database save returned comment without ID: #{inspect(real_comment)}")
            end

          {:error, reason} ->
            Logger.warning("Background database save failed: #{inspect(reason)}")

          other ->
            Logger.warning("Background database save returned unexpected result: #{inspect(other)}")
        end
      rescue
        e ->
          Logger.error("Exception in background database save: #{inspect(e)}")
      end
    end)
  end

  defp attempt_database_save_with_timeout(changeset, timeout) do
    task = Task.async(fn ->
      try do
        case Posts.create_comment(changeset) do
          {:ok, comment} when is_map(comment) ->
            {:ok, comment}
          %{} = comment when map_size(comment) > 0 ->
            {:ok, comment}
          %{} ->
            {:error, :empty_result}
          {:error, reason} ->
            {:error, reason}
          other ->
            {:error, {:unexpected_result, other}}
        end
      rescue
        e ->
          {:error, {:exception, e}}
      end
    end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  defp spawn_background_update_attempt(comment_id, new_content) do
    Task.start(fn ->
      try do
        comment_id_charlist = if is_binary(comment_id), do: to_charlist(comment_id), else: comment_id

        case :postdb.update_comment_content(comment_id_charlist, new_content) do
          :ok ->
            Logger.info("Background comment update succeeded")
          error ->
            Logger.warning("Background comment update failed: #{inspect(error)}")
        end
      rescue
        e ->
          Logger.error("Exception in background comment update: #{inspect(e)}")
      end
    end)
  end

  defp log_operation_time(operation, start_time) do
    duration = System.monotonic_time(:millisecond) - start_time
    Logger.info("#{operation} completed in #{duration}ms")
  end

  def replace_temp_comment_with_real(comments, temp_id, real_comment) do
    Enum.map(comments, fn comment ->
      if comment.id == temp_id do
        %{
          id: real_comment.id || temp_id,
          post_id: real_comment.post_id || comment.post_id,
          author: real_comment.author || comment.author,
          content: comment.content,
          inserted_at: real_comment.inserted_at || comment.inserted_at,
          updated_at: real_comment.updated_at || comment.updated_at,
          likes: real_comment.likes || [],
          replies: [],
          like_comment_event: "like-comment",
          is_temp: false
        }
      else
        comment
      end
    end)
  end

  def update_comment_content_in_list(comments, comment_id, new_content) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        Map.put(comment, :content, new_content)
      else
        comment
      end
    end)
  end

  def update_reply_content_in_comments(comments, reply_id, new_content) do
    Enum.map(comments, fn comment ->
      updated_replies = Enum.map(comment.replies || [], fn reply ->
        if to_string(reply.id) == to_string(reply_id) do
          Map.put(reply, :content, new_content)
        else
          reply
        end
      end)

      Map.put(comment, :replies, updated_replies)
    end)
  end

  def replace_temp_reply_with_real(comments, comment_id, temp_id, real_reply) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        updated_replies = Enum.map(comment.replies || [], fn reply ->
          if reply.id == temp_id do
            %{
              id: real_reply.id || temp_id,
              content: reply.content,
              user_id: real_reply.user_id || reply.user_id,
              comment_id: reply.comment_id,
              inserted_at: real_reply.inserted_at || reply.inserted_at,
              updated_at: real_reply.updated_at || reply.updated_at,
              is_temp: false
            }
          else
            reply
          end
        end)

        Map.put(comment, :replies, updated_replies)
      else
        comment
      end
    end)
  end

  def remove_temp_reply(comments, comment_id, temp_id) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        updated_replies = Enum.reject(comment.replies || [], fn reply ->
          reply.id == temp_id and Map.get(reply, :is_temp, false)
        end)

        Map.put(comment, :replies, updated_replies)
      else
        comment
      end
    end)
  end
end
