defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.UserClient
  alias Core.PostClient
  alias Mazaryn.Schema.Comment
  alias Mazaryn.Posts
  alias Mazaryn.Schema.Post
  alias Phoenix.LiveView.JS

  @impl Phoenix.LiveComponent
  def update_many(list_of_assigns) do
    IO.puts("=== UPDATE_MANY called ===")
    IO.inspect(length(list_of_assigns), label: "Number of assigns")
    changeset = Comment.changeset(%Comment{})
    update_comment_changeset = Comment.changeset(%Comment{})
    update_post_changeset = Post.changeset(%Post{})

    Enum.map(list_of_assigns, fn {assigns, socket} ->
      IO.puts("--- Processing post ---")
      IO.inspect(assigns.post, label: "Post object")
      IO.inspect(assigns.post.id, label: "Post ID")
      comments = Posts.get_comment_by_post_id(assigns.post.id)

      likes_count = get_likes_count(assigns.post.id)

      comments_with_ipfs_content =
        Enum.map(comments, fn comment ->
          actual_content = fetch_comment_content_from_ipfs(comment.id)
          Map.put(comment, :content, actual_content)
        end)

      comments_with_like_events =
        Enum.map(comments_with_ipfs_content, fn comment ->
          Map.put(
            comment,
            :like_comment_event,
            like_comment_event(assigns.current_user.id, comment.id)
          )
        end)

      comments_with_replies =
        comments_with_like_events
        |> Enum.map(fn comment ->
          replies = :postdb.get_comment_replies(comment.id |> to_charlist)

          list_replies =
            replies
            |> Enum.map(fn reply ->
              {:ok, built_reply} = reply
              |> Mazaryn.Schema.Reply.erl_changeset()
              |> Mazaryn.Schema.Reply.build()

              actual_content = fetch_reply_content_from_ipfs(built_reply.id)

              Map.put(built_reply, :content, actual_content)
            end)

          Map.put(comment, :replies, list_replies)
        end)
         IO.puts("About to call get_post_ipns...")
        ipns_id = get_post_ipns(assigns.post.id)
        IO.inspect(ipns_id, label: "IPNS ID result")

      ipns_id = get_post_ipns(assigns.post.id)

      assigns =
        assigns
        |> Map.put(:follow_event, follow_event(assigns.current_user.id, assigns.post.author))
        |> Map.put(:follow_text, follow_text(assigns.current_user.id, assigns.post.author))
        |> Map.put(:like_icon, like_icon(assigns.current_user.id, assigns.post.id))
        |> Map.put(:like_event, like_event(assigns.current_user.id, assigns.post.id))
        |> Map.put(:changeset, changeset)
        |> Map.put(:update_comment_changeset, update_comment_changeset)
        |> Map.put(:comments, comments_with_replies)
        |> Map.put(:report_action, false)
        |> Map.put(:like_action, false)
        |> Map.put(:is_liked, false)
        |> Map.put(:update_post_changeset, update_post_changeset)
        |> Map.put(:ipns_id, ipns_id)
        |> Map.put(:likes_count, likes_count)

      assign(socket, assigns)
    end)
  end

  defp get_likes_count(post_id) do
    post_id
    |> to_charlist
    |> PostClient.get_likes()
    |> Enum.count()
  end

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> assign(:editing_post, false)
     |> assign(:reply_comment, false)
     |> assign(:replying_to_comment_id, nil)
     |> assign(:ipns_id, nil)
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  def get_ipns_from_post(post_id) when is_list(post_id) do
    IO.puts("=== get_ipns_from_post called with LIST ===")
    IO.inspect(post_id, label: "post_id (list)", limit: :infinity)
    IO.inspect(length(post_id), label: "List length")

  try do
    result = Core.PostClient.get_ipns_from_post(post_id)
    IO.inspect(result, label: "post_server result")

    case result do
      ipns when is_list(ipns) ->
        IO.puts("Converting list to string")
        List.to_string(ipns)
      {:error, reason} ->
        IO.puts("Got error tuple")
        throw({:ipns_error, reason})
      other ->
        IO.inspect(other, label: "Unexpected result type")
        throw({:ipns_error, {:unexpected_result, other}})
    end
  catch
    error_type, reason ->
      IO.puts("Exception in get_ipns_from_post (list)")
      IO.inspect({error_type, reason}, label: "Exception details")
      reraise "IPNS Error", __STACKTRACE__
  end
end

def get_ipns_from_post(post_id) when is_binary(post_id) do
  IO.puts("=== get_ipns_from_post called with BINARY ===")
  IO.inspect(post_id, label: "post_id (binary)")
  IO.inspect(String.length(post_id), label: "String length")

  charlist = String.to_charlist(post_id)
  IO.inspect(charlist, label: "Converted to charlist", limit: :infinity)

  try do
    result = Core.PostClient.get_ipns_from_post(charlist)
    IO.inspect(result, label: "post_server result")

    case result do
      ipns when is_list(ipns) ->
        IO.puts("Converting list to string")
        List.to_string(ipns)
      {:error, reason} ->
        IO.puts("Got error tuple")
        throw({:ipns_error, reason})
      other ->
        IO.inspect(other, label: "Unexpected result type")
        throw({:ipns_error, {:unexpected_result, other}})
    end
  catch
    error_type, reason ->
      IO.puts("Exception in get_ipns_from_post (binary)")
      IO.inspect({error_type, reason}, label: "Exception details")
      reraise "IPNS Error", __STACKTRACE__
  end
end

def get_ipns_from_post(invalid_post_id) do
  IO.puts("=== get_ipns_from_post called with INVALID TYPE ===")
  IO.inspect(invalid_post_id, label: "Invalid post_id")
  throw({:ipns_error, {:invalid_post_id_type, invalid_post_id}})
end


defp get_post_ipns(post_id) do
  IO.puts("=== get_post_ipns called ===")
  IO.inspect(post_id, label: "post_id input")

  # Add nil/empty checks
  case post_id do
    nil ->
      IO.puts("post_id is nil")
      nil
    "" ->
      IO.puts("post_id is empty string")
      nil
    [] ->
      IO.puts("post_id is empty list")
      nil
    _ ->
      post_id_charlist = if is_binary(post_id), do: to_charlist(post_id), else: post_id
      IO.inspect(post_id_charlist, label: "Converted to charlist")

      try do
        IO.puts("About to call Core.PostClient.get_ipns_from_post")
        result = Core.PostClient.get_ipns_from_post(post_id_charlist)
        IO.inspect(result, label: "Final IPNS result")
        result
      catch
        :throw, :post_not_found ->
          IO.puts("CATCH: post_not_found")
          nil
        :throw, {:transaction_failed, reason} ->
          IO.puts("CATCH: transaction_failed")
          IO.inspect(reason, label: "Transaction failed reason")
          nil
        :throw, :ipns_timeout ->
          IO.puts("CATCH: ipns_timeout")
          nil
        :throw, {:ipns_error, reason} ->
          IO.puts("CATCH: ipns_error")
          IO.inspect(reason, label: "IPNS error reason")
          nil
        :throw, reason ->
          IO.puts("CATCH: generic throw")
          IO.inspect(reason, label: "Generic throw reason")
          nil
        type, reason ->
          IO.puts("CATCH: unexpected error in get_post_ipns")
          IO.inspect({type, reason}, label: "Unexpected error details")
          IO.puts("Stack trace:")
          IO.inspect(__STACKTRACE__, label: "Stack trace", limit: :infinity)
          nil
      rescue
        error ->
          IO.puts("RESCUE: Exception in get_post_ipns")
          IO.inspect(error, label: "Exception")
          IO.puts("Stack trace:")
          IO.inspect(__STACKTRACE__, label: "Stack trace", limit: :infinity)
          nil
      end
  end
end

  defp fetch_comment_content_from_ipfs(comment_id) do
    try do
      case Core.PostClient.get_comment_content(comment_id) do
        content when is_binary(content) and content != "" -> content
        content when is_list(content) ->
          case List.to_string(content) do
            "" -> "Content unavailable"
            str -> str
          end
        _ -> "Content unavailable"
      end
    catch
      type, reason ->
        IO.puts("Error fetching comment content for #{inspect(comment_id)}: #{inspect({type, reason})}")
        "Content unavailable"
    end
  end

  defp fetch_reply_content_from_ipfs(reply_id) do
    try do
      case Core.PostClient.get_reply_content(reply_id) do
        content when is_binary(content) and content != "" -> content
        content when is_list(content) ->
          case List.to_string(content) do
            "" -> "Content unavailable"
            str -> str
          end
        _ -> "Content unavailable"
      end
    catch
      type, reason ->
        IO.puts("Error fetching reply content for #{inspect(reply_id)}: #{inspect({type, reason})}")
        "Content unavailable"
    end
  end

  @impl true
  def handle_event(
        "delete-post",
        %{"post-id" => post_id} = _params,
        socket
      ) do
    post_id = post_id |> to_charlist
    PostClient.delete_post(post_id)
    send(self(), :reload_posts)

    {:noreply, socket}
  end

  def handle_event(
        "delete-comment",
        %{"comment-id" => comment_id, "post-id" => post_id} = _params,
        socket
      ) do
    post_id = post_id |> to_charlist
    comment_id = comment_id |> to_charlist
    IO.inspect(comment_id, label: "comment id")
    IO.inspect(post_id, label: "post id")

    :postdb.delete_comment_from_mnesia(comment_id)

    post =
      post_id
      |> rebuild_post()

    comments = Posts.get_comment_by_post_id(post.id)

    comments_with_ipfs_content =
      Enum.map(comments, fn comment ->
        actual_content = fetch_comment_content_from_ipfs(comment.id)
        Map.put(comment, :content, actual_content)
      end)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments_with_ipfs_content)}
  end

  def handle_event("validate-update-comment", %{"comment" => comment_params} = _params, socket) do
    changeset =
      %Comment{}
      |> Comment.update_changeset(comment_params)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :update_comment_changeset, changeset)}
  end

  def handle_event("edit_post", %{"post-id" => post_id}, socket) do
    post_id = post_id |> to_charlist
    post = PostClient.get_post_content_by_id(post_id)
    {:noreply, socket |> assign(:editing_post, post) |> assign(:edit_post_id, post_id)}
  end

  def handle_event("update-post", %{"post" => post_params}, socket) do
    post_id = socket.assigns.edit_post_id

    post_id = if is_binary(post_id), do: :erlang.binary_to_list(post_id), else: post_id

    changeset =
      %Post{}
      |> Post.changeset(post_params)

    case changeset.valid? do
      true ->
        new_content = post_params["content"]

        IO.puts("Attempting to update post")
        IO.inspect(post_id, label: "Post ID")
        IO.inspect(new_content, label: "New Content")

        case PostClient.update_post(post_id, new_content) do
          :ok ->
            try do
              retrieved_post = rebuild_post(post_id)
              IO.inspect(retrieved_post, label: "Retrieved Post After Update")

              ipns_id = get_post_ipns(post_id)

              send(self(), :reload_posts)

              {:noreply,
               socket
               |> assign(:editing_post, false)
               |> assign(:edit_post_id, nil)
               |> assign(:ipns_id, ipns_id)}
            catch
              _, e ->
                IO.puts("Error retrieving post after update")
                IO.inspect(e, label: "Retrieval Error")

                {:noreply,
                 socket
                 |> assign(:editing_post, false)
                 |> assign(:edit_post_id, nil)}
            end

          error ->
            IO.puts("Error in updating post")
            IO.inspect(error, label: "Update Error")
            {:noreply,
             socket
             |> put_flash(:error, "Failed to update post: #{inspect(error)}")}
        end

      false ->
        {:noreply, assign(socket, :update_post_changeset, %{changeset | action: :validate})}
    end
  end

  def handle_event("cancel-edit", _params, socket) do
    {:noreply, assign(socket, :editing_post, false)}
  end

  def handle_event("update-comment", %{"comment" => comment_params} = _params, socket) do
  IO.puts("📝 UPDATE-COMMENT EVENT TRIGGERED")
  IO.puts("=" <> String.duplicate("=", 60))
  IO.inspect(comment_params, label: "📋 Comment params for update")

  # Log comment update
  IO.puts("🔄 Updating comment...")
  comment =
    %Comment{}
    |> Comment.update_changeset(comment_params)
    |> Posts.update_comment()

  IO.inspect(comment, label: "📋 Updated comment result")

  # Log post rebuilding
  IO.puts("🔄 Rebuilding post after comment update...")
  post = comment.changes.post_id |> rebuild_post()
  IO.inspect(post.id, label: "📄 Rebuilt post ID")

  # Log comments retrieval
  IO.puts("📥 Retrieving comments after update...")
  comments = Posts.get_comment_by_post_id(post.id)
  IO.inspect(length(comments), label: "📊 Comments count after update")

  # Log IPFS content fetching
  IO.puts("🌐 Fetching IPFS content after update...")
  comments_with_ipfs_content =
    Enum.map(comments, fn comment ->
      IO.puts("  Processing comment ID: #{inspect(comment.id)}")
      actual_content = fetch_comment_content_from_ipfs(comment.id)
      IO.puts("  Updated content: #{String.slice(actual_content, 0, 50)}...")
      Map.put(comment, :content, actual_content)
    end)

  IO.puts("✅ UPDATE-COMMENT EVENT COMPLETED")
  IO.puts("=" <> String.duplicate("=", 60))

  {:noreply,
   socket
   |> assign(:post, post)
   |> assign(:comments, comments_with_ipfs_content)
   |> assign(:update_comment_changeset, Comment.changeset(%Comment{}))}
end

  def handle_event("validate-comment", %{"comment" => comment_params} = _params, socket) do
    IO.puts("🔍 VALIDATE-COMMENT EVENT TRIGGERED")
    IO.puts("=" <> String.duplicate("=", 60))
    IO.inspect(comment_params, label: "📋 Comment params received")
    IO.inspect(socket.assigns.current_user.id, label: "👤 Current user ID")
    IO.inspect(socket.assigns.post.id, label: "📝 Post ID")

    changeset =
      %Comment{}
      |> Comment.changeset(comment_params)
      |> Map.put(:action, :validate)

    IO.inspect(changeset.valid?, label: "✅ Changeset valid?")
    IO.inspect(changeset.errors, label: "❌ Changeset errors")
    IO.puts("=" <> String.duplicate("=", 60))

    {:noreply, assign(socket, :changeset, changeset)}
  end

  def handle_event("save-comment", %{"comment" => comment_params} = _params, socket) do
  IO.puts("💾 SAVE-COMMENT EVENT TRIGGERED")
  IO.puts("=" <> String.duplicate("=", 60))

  # Log initial state
  IO.inspect(comment_params, label: "📥 INCOMING COMMENT PARAMS")
  IO.inspect(socket.assigns.current_user.id, label: "👤 Current user ID")
  IO.inspect(socket.assigns.post.id, label: "📝 Current post ID")
  IO.inspect(socket.assigns.post.author, label: "👨‍💼 Post author")

  # Log changeset creation
  IO.puts("🔨 Creating changeset...")
  changeset = %Comment{} |> Comment.changeset(comment_params)
  IO.inspect(changeset.valid?, label: "✅ Changeset valid?")
  IO.inspect(changeset.errors, label: "❌ Changeset errors")
  IO.inspect(changeset.changes, label: "🔄 Changeset changes")

  # Log comment creation
  IO.puts("💬 Attempting to create comment...")
  comment_result = Posts.create_comment(changeset)
  IO.inspect(comment_result, label: "📋 Comment creation result")

  # Log post rebuilding
  IO.puts("🔄 Rebuilding post...")
  post_id_charlist = comment_params["post_id"] |> to_charlist
  IO.inspect(post_id_charlist, label: "📝 Post ID as charlist")

  post = rebuild_post(post_id_charlist)
  IO.inspect(post.id, label: "📄 Rebuilt post ID")

  # Log comments retrieval
  IO.puts("📥 Retrieving comments...")
  comments = Posts.get_comment_by_post_id(post.id)
  IO.inspect(length(comments), label: "📊 Number of comments retrieved")

  # Log IPFS content fetching
  IO.puts("🌐 Fetching IPFS content for comments...")
  comments_with_ipfs_content =
    Enum.map(comments, fn comment ->
      IO.puts("  Processing comment ID: #{inspect(comment.id)}")
      actual_content = fetch_comment_content_from_ipfs(comment.id)
      IO.puts("  Content fetched: #{String.slice(actual_content, 0, 50)}...")
      Map.put(comment, :content, actual_content)
    end)

  # Log like events processing
  IO.puts("👍 Processing like events...")
  comments_with_like_events =
    Enum.map(comments_with_ipfs_content, fn comment ->
      like_event = like_comment_event(socket.assigns.current_user.id, comment.id)
      IO.puts("  Comment #{inspect(comment.id)} like event: #{like_event}")
      Map.put(comment, :like_comment_event, like_event)
    end)

  # Log replies processing
  IO.puts("💬 Processing replies...")
  comments_with_replies =
    comments_with_like_events
    |> Enum.map(fn comment ->
      IO.puts("  Getting replies for comment: #{inspect(comment.id)}")
      replies = :postdb.get_comment_replies(comment.id |> to_charlist)
      IO.inspect(length(replies), label: "  Number of replies")

      list_replies =
        replies
        |> Enum.map(fn reply ->
          IO.puts("    Processing reply...")
          {:ok, built_reply} = reply
          |> Mazaryn.Schema.Reply.erl_changeset()
          |> Mazaryn.Schema.Reply.build()

          actual_content = fetch_reply_content_from_ipfs(built_reply.id)
          IO.puts("    Reply content fetched: #{String.slice(actual_content, 0, 30)}...")
          Map.put(built_reply, :content, actual_content)
        end)

      Map.put(comment, :replies, list_replies)
    end)

  IO.inspect(length(comments_with_replies), label: "📊 Final comments count")
  IO.puts("✅ SAVE-COMMENT EVENT COMPLETED")
  IO.puts("=" <> String.duplicate("=", 60))

  {:noreply,
   socket
   |> assign(:post, post)
   |> assign(:comments, comments_with_replies)
   |> assign(:changeset, Comment.changeset(%Comment{}))}
end

  def handle_event("reply_comment_content", %{"comment" => comment_params} = _params, socket) do
    %{
      "comment_id" => comment_id,
      "content" => content
    } = comment_params

    user_id = socket.assigns.current_user.id

    PostClient.reply_comment(user_id, to_charlist(comment_id), content)

    post = rebuild_post(socket.assigns.post.id)
    comments = Posts.get_comment_by_post_id(post.id)

    replies = :postdb.get_comment_replies(comment_id |> to_charlist)

    list_replies =
      replies
      |> Enum.map(fn reply ->
        {:ok, built_reply} = reply
        |> Mazaryn.Schema.Reply.erl_changeset()
        |> Mazaryn.Schema.Reply.build()

        actual_content = fetch_reply_content_from_ipfs(built_reply.id)

        Map.put(built_reply, :content, actual_content)
      end)

    comments_with_ipfs_content =
      Enum.map(comments, fn comment ->
        actual_content = fetch_comment_content_from_ipfs(comment.id)
        updated_comment = Map.put(comment, :content, actual_content)

        if comment.id == comment_id |> to_charlist do
          Map.put(updated_comment, :replies, list_replies)
        else
          updated_comment
        end
      end)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments_with_ipfs_content)}
  end

  def handle_event(
        "delete-reply",
        %{"reply-id" => reply_id, "comment-id" => comment_id},
        socket
      ) do
    reply_id = reply_id |> to_charlist
    comment_id = comment_id |> to_charlist

    :postdb.delete_reply_from_mnesia(reply_id)

    post = rebuild_post(socket.assigns.post.id)

    comments = Posts.get_comment_by_post_id(post.id)

    comments_with_ipfs_content =
      Enum.map(comments, fn comment ->
        actual_content = fetch_comment_content_from_ipfs(comment.id)
        Map.put(comment, :content, actual_content)
      end)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments_with_ipfs_content)}
  end

  def handle_event("reply_comment", %{"comment-id" => comment_id}, socket) do
    {:noreply, socket |> assign(:reply_comment, true) |> assign(:replying_to_comment_id, comment_id |> to_charlist)}
  end

  def handle_event("cancel-comment-reply", _, socket) do
    {:noreply, socket |> assign(:reply_comment, false) |> assign(:replying_to_comment_id, nil)}
  end

  def handle_event("show-comments", %{"id" => post_id}, socket) do
    IO.puts("👁️ SHOW COMMENTS EVENT")
    IO.inspect(post_id, label: "Post ID")

    Phoenix.LiveView.JS.toggle(to: "test")

    comments =
      Posts.get_comment_by_post_id(post_id)
      |> IO.inspect()

    comments_with_ipfs_content =
      Enum.map(comments, fn comment ->
        actual_content = fetch_comment_content_from_ipfs(comment.id)
        Map.put(comment, :content, actual_content)
      end)

    {:noreply,
     socket
     |> assign(:comments, comments_with_ipfs_content)}
  end

  def handle_event("like-comment", %{"comment-id" => comment_id}, socket) do
  comment_id = comment_id |> to_charlist
  user_id = socket.assigns.current_user.id

  PostClient.like_comment(user_id, comment_id)

  post_id = socket.assigns.post.id |> to_charlist

  updated_comments = Posts.get_comment_by_post_id(List.to_string(post_id))

  comments_with_ipfs_and_likes =
    Enum.map(updated_comments, fn comment ->
      actual_content = fetch_comment_content_from_ipfs(comment.id)
      comment
      |> Map.put(:content, actual_content)
      |> Map.put(:like_comment_event, like_comment_event(user_id, comment.id))
    end)

  post = rebuild_post(post_id)

  {:noreply,
   socket
   |> assign(:post, post)
   |> assign(:comments, comments_with_ipfs_and_likes)}
end

defp one_of_comment_likes?(user_id, comment_id) do
  try do
    comment_id
    |> PostClient.get_comment_likes()
    |> Enum.map(fn like ->
      case like |> Home.Like.erl_changeset() |> Home.Like.build() do
        {:ok, built_like} -> built_like
        {:error, _} -> nil
      end
    end)
    |> Enum.filter(&(&1 != nil))
    |> Enum.any?(&(&1.user_id == user_id))
  catch
    _, _ -> false
  end
end

defp like_comment_event(user_id, comment_id) do
  try do
    if one_of_comment_likes?(user_id, comment_id),
      do: "unlike-comment",
      else: "like-comment"
  catch
    _, _ -> "like-comment"
  end
end

defp comment_like_color(user_id, comment_id) do
  try do
    if one_of_comment_likes?(user_id, comment_id),
      do: "text-blue-500",
      else: "text-gray-500"
  catch
    _, _ -> "text-gray-500"
  end
end

  def handle_event("unlike-comment", %{"comment-id" => comment_id}, socket) do
    comment_id = comment_id |> to_charlist
    user_id = socket.assigns.current_user.id |> to_charlist
    comments = socket.assigns.comments

    comment =
      Enum.find(comments, fn comment -> comment.id == comment_id |> to_charlist end)

    like =
      comment_id
      |> PostClient.get_comment_likes()
      |> Enum.map(&(&1 |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)))
      |> Enum.filter(&(&1.user_id == user_id))
      |> hd()

    like_id = like.id

    updated_likes =
      Enum.filter(comment.likes, fn like ->
        like != like_id
      end)

    new_updated_likes = %{comment | likes: updated_likes}

    :postdb.update_comment_likes(comment_id, updated_likes)

    post_id = socket.assigns.post.id |> to_charlist
    post = rebuild_post(post_id)

    updated_comments = Posts.get_comment_by_post_id(post_id)

    comments_with_ipfs_and_likes =
      Enum.map(updated_comments, fn comment ->
        actual_content = fetch_comment_content_from_ipfs(comment.id)
        updated_comment = Map.put(comment, :content, actual_content)

        if comment.id == comment_id |> to_charlist do
          Map.put(updated_comment, :like_comment_event, "like-comment")
        else
          updated_comment
        end
      end)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:comments, comments_with_ipfs_and_likes)}
  end

  def handle_event("follow_user", %{"username" => username}, socket) do
    user_id = socket.assigns.current_user.id
    UserClient.follow(user_id, username)
    {:noreply, handle_assigns(socket, user_id, username)}
  end

  def handle_event("unfollow_user", %{"username" => username}, socket) do
    user_id = socket.assigns.current_user.id
    UserClient.unfollow(user_id, username)
    {:noreply, handle_assigns(socket, user_id, username)}
  end

  def handle_event("like_post", %{"post-id" => post_id}, socket) do
    post_id = post_id |> to_charlist
    user_id = socket.assigns.current_user.id
    PostClient.like_post(user_id, post_id)

    post = rebuild_post(post_id)
    likes_count = get_likes_count(post_id)

    Posts.get_likes_by_post_id(post_id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:like_icon, like_icon(user_id, post_id))
     |> assign(:like_event, like_event(user_id, post_id))
     |> assign(:likes_count, likes_count)
     |> assign(:is_liked, true)}
  end

  def handle_event("unlike_post", %{"post-id" => post_id}, socket) do
    post_id = post_id |> to_charlist
    user_id = socket.assigns.current_user.id

    like =
      post_id
      |> PostClient.get_likes()
      |> Enum.map(&(&1 |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)))
      |> Enum.filter(&(&1.user_id == user_id))
      |> hd()

    PostClient.unlike_post(like.id, post_id)

    post = rebuild_post(post_id)
    likes_count = get_likes_count(post_id)

    {:noreply,
     socket
     |> assign(:post, post)
     |> assign(:like_icon, like_icon(user_id, post_id))
     |> assign(:like_event, like_event(user_id, post_id))
     |> assign(:likes_count, likes_count)}
  end

  def handle_event("show_likes", %{"post-id" => post_id}, socket) do
    post_id = post_id |> to_charlist

    users =
      post_id
      |> PostClient.get_likes()
      |> Enum.map(&(&1 |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)))
      |> Enum.map(fn like -> like.user_id |> Core.UserClient.get_user_by_id() end)
      |> Enum.map(&(&1 |> elem(2) |> Users.one_by_username()))

    {:noreply, assign(socket, users: users)}
  end

  def handle_event("open_modal", %{"action" => "report-post"}, socket) do
    {:noreply,
     socket
     |> assign(
       like_action: false,
       report_action: true,
       edit_action: false,
       follower_action: false,
       follows_action: false
     )}
  end

  def handle_event("open_modal", %{"action" => "like-post"}, socket) do
    {:noreply,
     socket
     |> assign(
       like_action: true,
       report_action: false,
       edit_action: false,
       follower_action: false,
       follows_action: false
     )}
  end

  def get_user_avatar(author) do
    case Account.Users.one_by_username(author) do
      {:ok, user} ->
        avatar_cid = user.avatar_url

        if avatar_cid do
          Mazaryn.config([:media, :ipfs_gateway]) <> avatar_cid
        else
          ~p"/images/default-user.svg"
        end

      {:error, _changeset} -> ""
    end
  end

  defp rebuild_post(post_id) do
    {:ok, post} =
      PostClient.get_by_id(post_id)
      |> Mazaryn.Schema.Post.erl_changeset()
      |> Mazaryn.Schema.Post.build()

    post
  end

  defp activate_content_characters(post, socket) do
    try do
      content_str = cond do
        ipfs_content = Core.PostClient.get_post_content_by_id(post.id) ->
          cond do
            is_binary(ipfs_content) -> ipfs_content
            is_list(ipfs_content) -> List.to_string(ipfs_content)
            true ->
              case post.content do
                content when is_binary(content) -> content
                content when is_list(content) -> List.to_string(content)
                _ ->
                  IO.puts("Failed to retrieve post content")
                  "No content available"
              end
          end

        true ->
          case post.content do
            content when is_binary(content) -> content
            content when is_list(content) -> List.to_string(content)
            _ ->
              IO.puts("Failed to retrieve post content")
              "No content available"
          end
      end

      if content_str == "" do
        "No content available"
      else
        link_regex = ~r/([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#\.]?[\w-]+)*\/?/

        content_str
        |> String.split()
        |> Enum.map(fn con ->
          case {check_regex(con, ~r/@\S[a-zA-Z]*/), check_regex(con, ~r/#\S[a-zA-Z]*/),
                check_regex(con, link_regex)} do
            {[[mention]], [], []} ->
              activate_mention_only(mention, socket)

            {[], [[hashtag]], []} ->
              activate_hashtag_only(hashtag, socket)

            {[], [], [[url | _rest]]} ->
              activate_url_only(url)

            {[[mention]], [[hashtag]], [[url | _rest]]} ->
              activate_mention_only(mention, socket)
              activate_hashtag_only(hashtag, socket)
              activate_url_only(url)

            _ ->
              escape_char(con)
          end
        end)
        |> Enum.join(" ")
        |> Earmark.as_html!(compact_output: true)
        |> apply_styles()
      end
    catch
      type, reason ->
        IO.puts("Unexpected error in content processing")
        IO.inspect({type, reason}, label: "Error Details")

        "Error processing content"
        |> Earmark.as_html!(compact_output: true)
        |> apply_styles()
    end
  end

  defp apply_styles(html) do
    html
    |> String.replace("<a", "<a class=\"text-blue-500\"")
    |> String.replace("</a>", "</a>")
  end

  defp activate_hashtag_only(hashtag, socket) do
    locale = Gettext.get_locale(MazarynWeb.Gettext)
    path = Routes.live_path(socket, MazarynWeb.HashtagLive.Index, locale, hashtag)
    markdown = "[\ #{hashtag}](#{path})"

    String.replace(hashtag, hashtag, markdown)
  end

  defp activate_mention_only(mention, socket) do
    path =
      mention
      |> String.replace("@", "")
      |> create_user_path(socket)

    markdown = "[\ #{mention}](#{path})"

    String.replace(mention, mention, markdown)
  end

  defp activate_url_only("http" <> _rest = url) do
    url
  end

  defp activate_url_only(url) do
    path = "https" <> ":" <> "//" <> "#{url}"
    "[\ #{url}](#{path})"
  end

  defp escape_char(con) do
    case con do
      "#" -> "\\#"
      _ -> con
    end
  end

  defp check_regex(con, regex) do
    cond do
      con == "#" ->
        "#"

      con == "@" ->
        "@"

      true ->
        regex |> Regex.scan(con)
    end
  end

  defp create_user_path(username, socket) do
    case Users.one_by_username(username) do
      :ok ->
        "#"

      {:ok, _user} ->
        locale = Gettext.get_locale(MazarynWeb.Gettext)
        Routes.live_path(socket, MazarynWeb.UserLive.Profile, locale, username)
    end
  end

  defp handle_assigns(socket, user_id, username) do
    socket
    |> assign(:follow_event, follow_event(user_id, username))
    |> assign(:follow_text, follow_text(user_id, username))
  end

  defp one_of_following?(id, username) do
    id
    |> UserClient.get_following()
    |> Enum.any?(&(&1 == username))
  end

  defp follow_text(id, username) do
    if one_of_following?(id, username),
      do: "Unfollow",
      else: "Follow"
  end

  defp follow_event(id, username) do
    if one_of_following?(id, username),
      do: "unfollow_user",
      else: "follow_user"
  end

  defp one_of_likes?(user_id, post_id) do
    post_id
    |> PostClient.get_likes()
    |> Enum.map(fn like ->
      like
      |> Home.Like.erl_changeset()
      |> Home.Like.build()
      |> elem(1)
    end)
    |> Enum.any?(&(&1.user_id == user_id))
  end

  defp like_icon(user_id, post_id) do
    if one_of_likes?(user_id, post_id),
      do: "hand-thumb-down",
      else: "hand-thumb-up"
  end

  defp like_event(user_id, post_id) do
    if one_of_likes?(user_id, post_id),
      do: "unlike_post",
      else: "like_post"
  end

  defp verified?(author) do
    case Users.one_by_username(author) do
      {:ok, user} -> user.verified
      _ -> false
    end
  end

  defp like_comment_event(user_id, comment_id) do
    if one_of_comment_likes?(user_id, comment_id),
      do: "unlike-comment",
      else: "like-comment"
  end

  defp one_of_comment_likes?(user_id, comment_id) do
    comment_id
    |> PostClient.get_comment_likes()
    |> Enum.map(fn like ->
      like
      |> Home.Like.erl_changeset()
      |> Home.Like.build()
      |> elem(1)
    end)
    |> Enum.any?(&(&1.user_id == user_id))
  end

  defp comment_like_color(user_id, comment_id) do
    if one_of_comment_likes?(user_id, comment_id),
      # Blue color when liked
      do: "text-blue-500",
      # Gray color when unliked
      else: "text-gray-500"
  end

  def get_image_url(post_id) do
     cid = PostClient.get_media_cid(post_id)
     Mazaryn.config([:media, :ipfs_gateway]) <> cid
  end

end
