defmodule MazarynWeb.HomeLive.CommentHandler do

  alias Core.PostClient
  alias Mazaryn.Schema.Comment
  alias Mazaryn.Posts
  alias Account.Users
  alias Home.Like

  @content_cache :post_content_cache

  def handle_save_comment(%{"comment" => comment_params} = _params) do
    save_start = :erlang.system_time(:millisecond)
    IO.puts("🚀 === COMMENT SAVE DEBUG START ===")
    IO.puts("💾 Starting handle_save_comment")
    IO.puts("📋 Full params received")
    IO.puts("📝 Comment params: #{inspect(comment_params)}")

    post_id = comment_params["post_id"] || ""
    author = comment_params["author"] || ""
    content = comment_params["content"] || ""

    IO.puts("🔍 Extracted values:")
    IO.puts("   📌 post_id: '#{post_id}'")
    IO.puts("   👤 author: '#{author}'")
    IO.puts("   📄 content: '#{content}'")

    trimmed_post_id = String.trim(post_id)
    trimmed_author = String.trim(author)
    trimmed_content = String.trim(content)

    result = case {trimmed_post_id, trimmed_author, trimmed_content} do
      {"", _, _} ->
        IO.puts("❌ VALIDATION FAILED: Missing post_id")
        {:error, :missing_post_id}

      {_, "", _} ->
        IO.puts("❌ VALIDATION FAILED: Missing author")
        {:error, :missing_author}

      {_, _, ""} ->
        IO.puts("❌ VALIDATION FAILED: Empty content")
        changeset = Comment.changeset(%Comment{}, comment_params)
        |> Map.put(:action, :validate)
        |> Ecto.Changeset.add_error(:content, "can't be blank")
        {:error, {:validation, changeset}}

      {valid_post_id, valid_author, valid_content} ->
        IO.puts("✅ VALIDATION PASSED - Creating comment...")

        temp_comment = create_optimistic_comment(valid_post_id, valid_author, valid_content)

        changeset_data = %{
          post_id: valid_post_id,
          author: valid_author,
          content: valid_content
        }

        changeset = %Comment{}
        |> Comment.changeset(changeset_data)

        if not changeset.valid? do
          IO.puts("❌ CHANGESET INVALID - Cannot proceed with save")
          {:error, {:changeset, changeset}}
        else
          IO.puts("💾 Attempting to save comment via Posts.create_comment...")

          current_comments = get_comments_with_content_optimized(valid_post_id)

          try do
            save_result = Posts.create_comment(changeset)
            IO.puts("📤 Posts.create_comment returned: #{inspect(save_result)}")

            case save_result do
              {:ok, comment} ->
                IO.puts("✅ Comment saved successfully!")
                handle_successful_comment_save_with_immediate_update(valid_post_id, valid_content, comment, current_comments)

              {:error, changeset} ->
                IO.puts("❌ Error from Posts.create_comment")
                {:error, {:changeset, changeset}}

              %{} ->
                IO.puts("⚠️ Posts.create_comment returned empty map, trying alternative approach...")
                handle_alternative_save_with_immediate_update(valid_post_id, valid_author, valid_content, current_comments, temp_comment)

              other ->
                IO.puts("⚠️ Unexpected result from Posts.create_comment: #{inspect(other)}")
                handle_alternative_save_with_immediate_update(valid_post_id, valid_author, valid_content, current_comments, temp_comment)
            end

          rescue
            exception ->
              IO.puts("💥 EXCEPTION in Posts.create_comment: #{inspect(exception)}")
              handle_alternative_save_with_immediate_update(valid_post_id, valid_author, valid_content, current_comments, temp_comment)
          end
        end
    end

    save_end = :erlang.system_time(:millisecond)
    total_time = save_end - save_start
    IO.puts("⏱️ handle_save_comment completed in #{total_time}ms")
    IO.puts("📊 Final result: #{inspect(result)}")
    IO.puts("🏁 === COMMENT SAVE DEBUG END ===")

    result
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

  defp handle_successful_comment_save_with_immediate_update(post_id, content, comment, current_comments) do
    IO.puts("🔄 Handling successful save with immediate update...")

    if comment && comment.id do
      cache_content(:comment, comment.id, content)
      IO.puts("💾 Cached content for new comment #{comment.id}")
    end

    new_comment = %{
      id: comment.id,
      post_id: post_id,
      author: comment.author || comment[:author],
      content: content,
      inserted_at: comment.inserted_at || DateTime.utc_now(),
      updated_at: comment.updated_at || DateTime.utc_now(),
      likes: comment.likes || [],
      replies: [],
      like_comment_event: "like-comment",
      is_temp: false
    }

    updated_comments = current_comments ++ [new_comment]

    :ets.delete(@content_cache, {:comments_processed, post_id})

    post_rebuild_start = :erlang.system_time(:millisecond)
    post = rebuild_post(post_id)
    post_rebuild_end = :erlang.system_time(:millisecond)
    IO.puts("🏗️ Post rebuild took #{post_rebuild_end - post_rebuild_start}ms")

    spawn_comment_sync_task(post_id, comment.id)

    success_result = {:ok, %{
      post: post,
      comments: updated_comments,
      changeset: Comment.changeset(%Comment{}),
      flash: {:info, "Comment saved!"}
    }}

    IO.puts("✅ SUCCESS - Returning result with immediate update")
    success_result
  end

  defp handle_alternative_save_with_immediate_update(post_id, author, content, current_comments, temp_comment) do
    IO.puts("🔄 Handling alternative save with immediate update...")

    updated_comments = current_comments ++ [temp_comment]

    cache_content(:comment, temp_comment.id, content)

    spawn_async_comment_save_with_sync(temp_comment.id, post_id, author, content)

    {:ok, %{
      post: rebuild_post(post_id),
      comments: updated_comments,
      changeset: Comment.changeset(%Comment{}),
      flash: {:info, "Comment saved!"}
    }}
  end

  defp spawn_comment_sync_task(post_id, comment_id) do
    parent_pid = self()

    Task.start(fn ->
      Process.sleep(1000)

      IO.puts("🔄 Background sync started for comment #{comment_id}")

      fresh_comments = try do
        fetch_and_process_comments_improved(post_id, {:comments_processed, post_id})
      rescue
        _ -> []
      end

      if length(fresh_comments) > 0 do
        send(parent_pid, {:comments_synced, post_id, fresh_comments})
        IO.puts("✅ Background sync completed for post #{post_id}")
      end
    end)
  end

  defp spawn_async_comment_save_with_sync(temp_id, post_id, author, content) do
    parent_pid = self()

    Task.start(fn ->
      IO.puts("🔄 Background comment save started for temp #{temp_id}")

      case save_comment_directly(temp_id, post_id, author, content) do
        {:ok, real_comment} ->
          IO.puts("✅ Background comment save succeeded for #{temp_id} -> #{real_comment.id}")

          cache_content(:comment, real_comment.id, content)

          send(parent_pid, {:temp_comment_saved, temp_id, real_comment})

        {:error, reason} ->
          IO.puts("❌ Background comment save failed for #{temp_id}: #{inspect(reason)}")
          send(parent_pid, {:temp_comment_save_failed, temp_id, reason})
      end
    end)
  end

  def save_comment(%{post_id: post_id, author: author, content: content} = params) do
    IO.puts("🚀 === SAVE_COMMENT DEBUG START ===")
    IO.puts("📋 Input params: #{inspect(params)}")

    current_comments = get_comments_with_content_optimized(post_id)

    changeset_data = %{
      post_id: post_id,
      author: author,
      content: content
    }

    changeset = %Comment{}
      |> Comment.changeset(changeset_data)

    case Posts.create_comment(changeset) do
      {:ok, comment} ->
        IO.puts("✅ Comment created successfully: #{inspect(comment)}")

        cache_content(:comment, comment.id, content)

        new_comment = %{
          id: comment.id,
          post_id: post_id,
          author: author,
          content: content,
          inserted_at: comment.inserted_at || DateTime.utc_now(),
          updated_at: comment.updated_at || DateTime.utc_now(),
          likes: [],
          replies: [],
          like_comment_event: "like-comment"
        }

        updated_comments = current_comments ++ [new_comment]

        :ets.delete(@content_cache, {:comments_processed, post_id})

        post = rebuild_post(post_id)

        success_result = {:ok, %{
          post: post,
          comments: updated_comments,
          changeset: Comment.changeset(%Comment{})
        }}

        IO.puts("✅ save_comment SUCCESS with immediate update")
        IO.puts("🏁 === SAVE_COMMENT DEBUG END ===")
        success_result

      {:error, changeset} ->
        IO.puts("❌ Error creating comment: #{inspect(changeset.errors)}")
        IO.puts("❌ save_comment FAILED")
        IO.puts("🏁 === SAVE_COMMENT DEBUG END ===")
        {:error, changeset}
    end
  end

  defp replace_temp_comment_with_real(comments, temp_id, real_comment) do
    Enum.map(comments, fn comment ->
      if comment.id == temp_id do
        %{
          id: real_comment.id,
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


defp get_type(value) do
  cond do
    is_binary(value) -> :binary
    is_list(value) -> :list
    is_map(value) && Map.has_key?(value, :__struct__) -> value.__struct__
    is_map(value) -> :map
    is_atom(value) -> :atom
    is_integer(value) -> :integer
    is_float(value) -> :float
    true -> :unknown
  end
end

defp handle_successful_comment_save(post_id, content, comment) do
  IO.puts("🔄 Rebuilding post and fetching comments...")

  if comment && comment.id do
    cache_content(:comment, comment.id, content)
    IO.puts("💾 Cached content for new comment #{comment.id}")
  end

  post_rebuild_start = :erlang.system_time(:millisecond)
  post = rebuild_post(post_id)
  post_rebuild_end = :erlang.system_time(:millisecond)
  IO.puts("🏗️ Post rebuild took #{post_rebuild_end - post_rebuild_start}ms")

  comments_fetch_start = :erlang.system_time(:millisecond)
  comments = case function_exported?(__MODULE__, :get_comments_with_content, 1) do
    true ->
      IO.puts("📚 Using get_comments_with_content function")
      get_comments_with_content(post_id)
    false ->
      IO.puts("📚 Using Posts.get_comment_by_post_id fallback")
      Posts.get_comment_by_post_id(post_id) || []
  end
  comments_fetch_end = :erlang.system_time(:millisecond)
  IO.puts("📚 Comments fetch took #{comments_fetch_end - comments_fetch_start}ms")
  IO.puts("📊 Fetched #{length(comments)} comments")

  success_result = {:ok, %{
    post: post,
    comments: comments,
    changeset: Comment.changeset(%Comment{}),
    flash: {:info, "Comment saved!"}
  }}

  IO.puts("✅ SUCCESS - Returning result")
  success_result
end


defp try_alternative_comment_save(post_id, author, content) do
  IO.puts("🔄 Trying alternative comment save approach...")

  try do

    comment_id = generate_comment_id()
    IO.puts("🆔 Generated comment ID: #{comment_id}")


    case save_comment_directly(comment_id, post_id, author, content) do
      {:ok, comment} ->
        IO.puts("✅ Alternative save successful!")
        handle_successful_comment_save(post_id, content, comment)

      {:error, reason} ->
        IO.puts("❌ Alternative save failed: #{inspect(reason)}")


        mock_comment = create_mock_comment(comment_id, post_id, author, content)


        cache_content(:comment, comment_id, content)


        spawn_async_comment_save(comment_id, post_id, author, content)


        {:ok, %{
          post: rebuild_post(post_id),
          comments: get_comments_with_content_optimized(post_id),
          changeset: Comment.changeset(%Comment{}),
          flash: {:info, "Comment saved!"}
        }}
    end

  rescue
    exception ->
      IO.puts("💥 Exception in alternative save: #{inspect(exception)}")


      changeset = Comment.changeset(%Comment{}, %{
        post_id: post_id,
        author: author,
        content: content
      })
      |> Map.put(:action, :insert)
      |> Ecto.Changeset.add_error(:base, "Failed to save comment. Please try again.")

      {:error, {:changeset, changeset}}
  end
end


defp generate_comment_id do
  :crypto.strong_rand_bytes(16) |> Base.encode64() |> String.replace(["+", "/", "="], "")
end


defp save_comment_directly(comment_id, post_id, author, content) do
  try do

    case PostClient.create_comment(author, post_id, content) do
      {:ok, result} ->
        IO.puts("✅ PostClient.create_comment succeeded")
        {:ok, result}
      {:error, reason} ->
        IO.puts("❌ PostClient.create_comment failed: #{inspect(reason)}")
        try_postdb_save(comment_id, post_id, author, content)
      other ->
        IO.puts("⚠️ PostClient.create_comment returned: #{inspect(other)}")
        try_postdb_save(comment_id, post_id, author, content)
    end
  rescue
    _ ->
      IO.puts("❌ PostClient.create_comment not available, trying postdb...")
      try_postdb_save(comment_id, post_id, author, content)
  end
end


defp try_postdb_save(comment_id, post_id, author, content) do
  try do

    comment_data = {
      to_charlist(comment_id),
      to_charlist(post_id),
      to_charlist(author),
      to_charlist(content),
      :erlang.system_time(:second),
      []
    }

    case :postdb.save_comment(comment_data) do
      :ok ->
        IO.puts("✅ Direct postdb save succeeded")


        comment = %{
          id: comment_id,
          post_id: post_id,
          author: author,
          content: content,
          inserted_at: DateTime.utc_now(),
          likes: []
        }

        {:ok, comment}

      error ->
        IO.puts("❌ Direct postdb save failed: #{inspect(error)}")
        {:error, error}
    end
  rescue
    exception ->
      IO.puts("💥 Exception in postdb save: #{inspect(exception)}")
      {:error, exception}
  end
end


defp create_mock_comment(comment_id, post_id, author, content) do
  %{
    id: comment_id,
    post_id: post_id,
    author: author,
    content: content,
    inserted_at: DateTime.utc_now(),
    likes: []
  }
end


defp spawn_async_comment_save(comment_id, post_id, author, content) do
  Task.start(fn ->
    IO.puts("🔄 Background comment save started for #{comment_id}")


    Process.sleep(1000)

    case save_comment_directly(comment_id, post_id, author, content) do
      {:ok, _} ->
        IO.puts("✅ Background comment save succeeded for #{comment_id}")
      {:error, reason} ->
        IO.puts("❌ Background comment save failed for #{comment_id}: #{inspect(reason)}")
    end
  end)
end


defp get_type(value) do
  cond do
    is_binary(value) -> :binary
    is_list(value) -> :list
    is_map(value) && Map.has_key?(value, :__struct__) -> value.__struct__
    is_map(value) -> :map
    is_atom(value) -> :atom
    is_integer(value) -> :integer
    is_float(value) -> :float
    true -> :unknown
  end
end

def handle_save_comment(params) do
  IO.puts("🚀 === INVALID PARAMS DEBUG START ===")
  IO.puts("❌ Invalid save-comment parameters")
  IO.puts("📋 Invalid params: #{inspect(params)}")
  IO.puts("📊 Params type: #{inspect(params.__struct__ || :unknown)}")

  case params do
    %{} = map_params ->
      IO.puts("📝 Map keys: #{inspect(Map.keys(map_params))}")
      IO.puts("🔍 Expected structure: %{\"comment\" => %{...}}")

    _ ->
      IO.puts("⚠️ Params is not a map")
  end

  IO.puts("❌ handle_save_comment FAILED - invalid params")
  IO.puts("🏁 === INVALID PARAMS DEBUG END ===")
  {:error, :invalid_params}
end

def save_comment(%{post_id: post_id, author: author, content: content} = params) do
  IO.puts("🚀 === SAVE_COMMENT DEBUG START ===")
  IO.puts("📋 Input params: #{inspect(params)}")
  IO.puts("📝 Extracted values:")
  IO.puts("   📌 post_id: #{inspect(post_id)}")
  IO.puts("   👤 author: #{inspect(author)}")
  IO.puts("   📄 content: #{inspect(content)}")


  changeset_data = %{
    post_id: post_id,
    author: author,
    content: content
  }

  IO.puts("🔧 Creating changeset with: #{inspect(changeset_data)}")

  changeset = %Comment{}
    |> Comment.changeset(changeset_data)

  IO.puts("📋 Changeset created:")
  IO.puts("   ✅ Valid: #{changeset.valid?}")
  IO.puts("   ❌ Errors: #{inspect(changeset.errors)}")
  IO.puts("   📊 Changes: #{inspect(changeset.changes)}")

  case Posts.create_comment(changeset) do
    {:ok, comment} ->
      IO.puts("✅ Comment created successfully: #{inspect(comment)}")

      # Cache the content
      IO.puts("💾 Caching comment content...")
      cache_content(:comment, comment.id, content)

      # Clear processed comments cache
      IO.puts("🗑️ Clearing processed comments cache for post #{post_id}")
      :ets.delete(@content_cache, {:comments_processed, post_id})

      # Get fresh comments and post
      IO.puts("🔄 Fetching fresh comments and rebuilding post...")
      comments = get_comments_with_content_optimized(post_id)
      post = rebuild_post(post_id)

      IO.puts("📊 Final state:")
      IO.puts("   📝 Comments count: #{length(comments)}")
      IO.puts("   🏗️ Post rebuilt: #{not is_nil(post)}")

      success_result = {:ok, %{
        post: post,
        comments: comments,
        changeset: Comment.changeset(%Comment{})
      }}

      IO.puts("✅ save_comment SUCCESS")
      IO.puts("🏁 === SAVE_COMMENT DEBUG END ===")
      success_result

    {:error, changeset} ->
      IO.puts("❌ Error creating comment: #{inspect(changeset.errors)}")
      IO.puts("📋 Failed changeset: #{inspect(changeset)}")
      IO.puts("❌ save_comment FAILED")
      IO.puts("🏁 === SAVE_COMMENT DEBUG END ===")
      {:error, changeset}
  end
end

def save_comment(invalid_params) do
  IO.puts("🚀 === SAVE_COMMENT DEBUG START (INVALID PARAMS) ===")
  IO.puts("❌ Missing required comment parameters")
  IO.puts("📋 Received params: #{inspect(invalid_params)}")
  IO.puts("📊 Params type: #{inspect(invalid_params.__struct__ || :unknown)}")

  case invalid_params do
    %{} = map_params ->
      IO.puts("📝 Map keys: #{inspect(Map.keys(map_params))}")
      IO.puts("🔍 Looking for required keys: [:post_id, :author, :content]")

      missing_keys = [:post_id, :author, :content] -- Map.keys(map_params)
      IO.puts("❌ Missing keys: #{inspect(missing_keys)}")

    _ ->
      IO.puts("⚠️ Params is not a map")
  end

  IO.puts("❌ save_comment FAILED - missing params")
  IO.puts("🏁 === SAVE_COMMENT DEBUG END ===")
  {:error, :missing_params}
end

defp rebuild_post(post_id) do
  IO.puts("🏗️ === REBUILD_POST DEBUG START ===")
  IO.puts("📌 Rebuilding post: #{post_id}")
  IO.puts("📊 Post ID type: #{inspect(post_id.__struct__ || :binary)}")

  try do
    # Get post data
    IO.puts("📡 Fetching post data via PostClient.get_by_id...")
    get_start = :erlang.system_time(:millisecond)

    post_data = PostClient.get_by_id(post_id)

    get_end = :erlang.system_time(:millisecond)
    IO.puts("📡 PostClient.get_by_id took #{get_end - get_start}ms")
    IO.puts("📊 Post data received: #{inspect(post_data)}")

    # Convert to changeset
    IO.puts("🔄 Converting to changeset...")
    changeset_start = :erlang.system_time(:millisecond)

    changeset = Mazaryn.Schema.Post.erl_changeset(post_data)

    changeset_end = :erlang.system_time(:millisecond)
    IO.puts("🔄 Changeset creation took #{changeset_end - changeset_start}ms")
    IO.puts("📋 Changeset: #{inspect(changeset)}")

    # Build post
    IO.puts("🏗️ Building post...")
    build_start = :erlang.system_time(:millisecond)

    {:ok, post} = Mazaryn.Schema.Post.build(changeset)

    build_end = :erlang.system_time(:millisecond)
    IO.puts("🏗️ Post build took #{build_end - build_start}ms")
    IO.puts("✅ Post built successfully: #{inspect(post)}")
    IO.puts("🏁 === REBUILD_POST DEBUG END ===")

    post

  rescue
    exception ->
      IO.puts("💥 Exception in rebuild_post: #{inspect(exception)}")
      IO.puts("📍 Stacktrace: #{inspect(__STACKTRACE__)}")
      IO.puts("❌ REBUILD_POST FAILED")
      IO.puts("🏁 === REBUILD_POST DEBUG END ===")
      reraise exception, __STACKTRACE__
  end
end

  def handle_save_comment(%{"comment" => comment_params} = _params) do
    save_start = :erlang.system_time(:millisecond)
    IO.puts("💾 Starting handle_save_comment with params: #{inspect(comment_params)}")

    post_id = comment_params["post_id"] || ""
    author = comment_params["author"] || ""
    content = comment_params["content"] || ""

    result = case {String.trim(post_id), String.trim(author), String.trim(content)} do
      {"", _, _} ->
        IO.puts("❌ Missing post_id in handle_save_comment")
        {:error, :missing_post_id}

      {_, "", _} ->
        IO.puts("❌ Missing author in handle_save_comment")
        {:error, :missing_author}

      {_, _, ""} ->
        IO.puts("❌ Empty content in handle_save_comment")
        changeset = Comment.changeset(%Comment{}, comment_params)
        |> Map.put(:action, :validate)
        |> Ecto.Changeset.add_error(:content, "can't be blank")
        {:error, {:validation, changeset}}

      {valid_post_id, valid_author, valid_content} ->
        changeset = %Comment{}
        |> Comment.changeset(%{
          post_id: valid_post_id,
          author: valid_author,
          content: valid_content
        })

        case Posts.create_comment(changeset) do
          {:ok, _comment} ->
            IO.puts("✅ Comment saved successfully for post #{valid_post_id}")
            post = rebuild_post(valid_post_id)
            comments = case function_exported?(__MODULE__, :get_comments_with_content, 1) do
              true -> get_comments_with_content(valid_post_id)
              false -> Posts.get_comment_by_post_id(valid_post_id) || []
            end
            {:ok, %{
              post: post,
              comments: comments,
              changeset: Comment.changeset(%Comment{}),
              flash: {:info, "Comment saved!"}
            }}

          {:error, changeset} ->
            IO.puts("❌ Error saving comment for post #{valid_post_id}: #{inspect(changeset.errors)}")
            {:error, {:changeset, changeset}}
        end
    end

    save_end = :erlang.system_time(:millisecond)
    IO.puts("💾 handle_save_comment completed in #{save_end - save_start}ms")
    result
  end

  def get_comments_with_content_optimized(post_id) do
    fetch_start = :erlang.system_time(:millisecond)
    IO.puts("🔍 Starting get_comments_with_content_optimized for post #{post_id}")

    result = try do
      cache_key = {:comments_processed, post_id}
      case :ets.lookup(@content_cache, cache_key) do
        [{^cache_key, comments, timestamp}] ->
          age = :erlang.system_time(:second) - timestamp
          if age < 180 do
            IO.puts("📦 Comments Cache HIT for post #{post_id} (age: #{age}s)")
            verify_and_fix_comment_content(comments, post_id)
          else
            IO.puts("⏰ Comments Cache EXPIRED for post #{post_id} (age: #{age}s)")
            fetch_and_process_comments_improved(post_id, cache_key)
          end
        [] ->
          IO.puts("❌ Comments Cache MISS for post #{post_id}")
          fetch_and_process_comments_improved(post_id, cache_key)
      end
    rescue
      e ->
        IO.puts("❌ Error getting comments for post #{post_id}: #{inspect(e)}")
        []
    end

    fetch_end = :erlang.system_time(:millisecond)
    IO.puts("🔍 get_comments_with_content_optimized for post #{post_id} completed in #{fetch_end - fetch_start}ms")
    result
  end

  def fetch_and_process_comments_improved(post_id, cache_key) do
    comments_start = :erlang.system_time(:millisecond)
    IO.puts("🔄 Starting fetch_and_process_comments_improved for post #{post_id}")

    task = Task.async(fn ->
      fetch_start = :erlang.system_time(:millisecond)
      result = Posts.get_comment_by_post_id(post_id)
      fetch_end = :erlang.system_time(:millisecond)
      IO.puts("📚 Posts.get_comment_by_post_id for post #{post_id} took #{fetch_end - fetch_start}ms")
      result
    end)

    comments = case Task.yield(task, 2000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} ->
        IO.puts("✅ Fetched #{length(result || [])} comments for post #{post_id}")
        result || []
      nil ->
        IO.puts("⏰ Timeout fetching comments for post #{post_id}")
        []
    end

    IO.inspect(length(comments), label: "Number of comments found")

    processed_comments = comments
    |> Enum.take(5)
    |> Enum.map(fn comment ->
      comment_start = :erlang.system_time(:millisecond)
      result = process_single_comment_with_content(comment)
      comment_end = :erlang.system_time(:millisecond)
      IO.puts("📝 Processed comment #{comment.id} in #{comment_end - comment_start}ms")
      result
    end)
    |> Enum.filter(&(&1 != nil))

    :ets.insert(@content_cache, {cache_key, processed_comments, :erlang.system_time(:second)})
    IO.puts("💾 Cached processed comments for post #{post_id}")

    comments_end = :erlang.system_time(:millisecond)
    IO.puts("✅ fetch_and_process_comments_improved for post #{post_id} completed in #{comments_end - comments_start}ms")
    processed_comments
  end

  def process_single_comment_with_content(comment) do
    comment_start = :erlang.system_time(:millisecond)
    IO.puts("📝 Starting process_single_comment_with_content for comment #{comment.id}")

    result = try do
      content = get_comment_content_with_fallbacks(comment.id)

      comment
      |> Map.put(:content, content)
      |> Map.put(:like_comment_event, like_comment_event_cached(comment.id))
      |> Map.put(:replies, get_comment_replies_optimized(comment.id))
    rescue
      e ->
        IO.puts("❌ Error processing comment #{comment.id}: #{inspect(e)}")
        nil
    end

    comment_end = :erlang.system_time(:millisecond)
    IO.puts("📝 process_single_comment_with_content for comment #{comment.id} completed in #{comment_end - comment_start}ms")
    result
  end

  def get_comment_content_with_fallbacks(comment_id) do
    fetch_start = :erlang.system_time(:millisecond)
    IO.puts("📄 Starting get_comment_content_with_fallbacks for comment #{comment_id}")

    result = case get_cached_content(:comment, comment_id) do
      content when is_binary(content) and content != "" and
                content not in ["Loading...", "Loading content...", "Content loading...", "Content temporarily unavailable"] ->
        IO.puts("📦 Using cached content for comment #{comment_id}")
        content

      _ ->
        IO.puts("❌ Cache miss or invalid content for comment #{comment_id}, attempting synchronous fetch")
        case attempt_synchronous_fetch(comment_id) do
          {:ok, content} when is_binary(content) and content != "" ->
            IO.puts("✅ Synchronous fetch succeeded for comment #{comment_id}")
            cache_content(:comment, comment_id, content)
            content
          _ ->
            IO.puts("⚠️ Synchronous fetch failed, attempting final fetch for comment #{comment_id}")
            case fetch_comment_content_final_attempt(comment_id) do
              {:ok, content} ->
                IO.puts("✅ Final fetch succeeded for comment #{comment_id}")
                cache_content(:comment, comment_id, content)
                content
              _ ->
                IO.puts("⚠️ Final fetch failed, spawning background fetch for comment #{comment_id}")
                spawn_comment_content_fetch_improved(comment_id)
                "Loading content..."
            end
        end
    end

    fetch_end = :erlang.system_time(:millisecond)
    IO.puts("📄 get_comment_content_with_fallbacks for comment #{comment_id} completed in #{fetch_end - fetch_start}ms")
    result
  end

  def handle_save_comment(%{"comment" => comment_params} = _params) do
    IO.puts("💾 SAVE-COMMENT EVENT TRIGGERED")
    IO.inspect(comment_params, label: "Comment params received")

    post_id = comment_params["post_id"] || ""
    author = comment_params["author"] || ""
    content = comment_params["content"] || ""

    case {String.trim(post_id), String.trim(author), String.trim(content)} do
      {"", _, _} ->
        {:error, :missing_post_id}

      {_, "", _} ->
        {:error, :missing_author}

      {_, _, ""} ->
        changeset = Comment.changeset(%Comment{}, comment_params)
        |> Map.put(:action, :validate)
        |> Ecto.Changeset.add_error(:content, "can't be blank")
        {:error, {:validation, changeset}}

      {valid_post_id, valid_author, valid_content} ->
        changeset = %Comment{}
        |> Comment.changeset(%{
          post_id: valid_post_id,
          author: valid_author,
          content: valid_content
        })

        case Posts.create_comment(changeset) do
          {:ok, _comment} ->
            IO.puts("✅ Comment saved successfully")

            post = rebuild_post(valid_post_id)

            comments = case function_exported?(__MODULE__, :get_comments_with_content, 1) do
              true -> get_comments_with_content(valid_post_id)
              false -> Posts.get_comment_by_post_id(valid_post_id) || []
            end

            {:ok, %{
              post: post,
              comments: comments,
              changeset: Comment.changeset(%Comment{}),
              flash: {:info, "Comment saved!"}
            }}

          {:error, changeset} ->
            IO.puts("❌ Error saving comment")
            IO.inspect(changeset.errors, label: "Errors")
            {:error, {:changeset, changeset}}
        end
    end
  end

  def handle_save_comment(params) do
    IO.puts("❌ Invalid save-comment parameters")
    IO.inspect(params, label: "Invalid params")
    {:error, :invalid_params}
  end

  def handle_update_comment(%{"comment" => comment_params} = _params, post_id) do
    IO.puts("📝 UPDATE-COMMENT handler called")
    IO.inspect(comment_params, label: "Received comment params")

    try do
      comment_id = Map.get(comment_params, "id")
      new_content = Map.get(comment_params, "content", "")

      if comment_id && String.trim(new_content) != "" do
        case update_comment(comment_params) do
          {:ok, _} ->
            IO.puts("✅ Comment update successful - refreshing comments")

            fresh_comments = get_comments_with_content_optimized(post_id)

            IO.puts("🔄 Refreshed #{length(fresh_comments)} comments after update")

            {:ok, %{
              comments: fresh_comments,
              editing_comment: false,
              editing_comment_id: nil,
              update_comment_changeset: Comment.changeset(%Comment{}),
              flash: {:info, "Comment updated successfully"}
            }}

          {:error, changeset} ->
            IO.puts("❌ Comment update failed")
            IO.inspect(changeset.errors, label: "Changeset errors")

            {:error, %{
              update_comment_changeset: changeset,
              flash: {:error, "Failed to update comment. Please try again."}
            }}

          error ->
            IO.puts("❌ Unexpected error updating comment: #{inspect(error)}")

            {:error, %{
              flash: {:error, "Failed to update comment. Please try again."}
            }}
        end
      else
        IO.puts("❌ Invalid comment data - missing ID or content")

        changeset = Comment.changeset(%Comment{}, comment_params)
        |> Map.put(:action, :validate)
        |> Ecto.Changeset.add_error(:content, "can't be blank")

        {:error, %{
          update_comment_changeset: changeset,
          flash: {:error, "Comment content cannot be empty"}
        }}
      end

    rescue
      e ->
        IO.puts("❌ Exception in update-comment handler: #{inspect(e)}")

        {:error, %{
          flash: {:error, "An error occurred while updating the comment. Please try again."},
          editing_comment: false,
          editing_comment_id: nil
        }}
    end
  end

  def handle_validate_comment(%{"comment" => comment_params} = _params) do
    changeset =
      %Comment{}
      |> Comment.changeset(comment_params)
      |> Map.put(:action, :validate)

    {:ok, %{changeset: changeset}}
  end

  def handle_validate_update_comment(%{"comment" => comment_params} = _params) do
    changeset =
      %Comment{}
      |> Comment.update_changeset(comment_params)
      |> Map.put(:action, :validate)

    {:ok, %{update_comment_changeset: changeset}}
  end

  def handle_edit_comment(%{"comment-id" => comment_id}) do
    IO.puts("📝 Starting to edit comment #{comment_id}")

    {:ok, %{
      editing_comment: true,
      editing_comment_id: comment_id
    }}
  end

  def handle_cancel_comment_edit(_params) do
    IO.puts("❌ Cancelling comment edit")

    {:ok, %{
      editing_comment: false,
      editing_comment_id: nil
    }}
  end

  def handle_reply_comment(%{"comment-id" => comment_id}) do
    comment_id_charlist = comment_id |> to_charlist()
    IO.puts("📨 Setting reply state for comment: #{comment_id}")

    {:ok, %{
      reply_comment: true,
      replying_to_comment_id: comment_id_charlist
    }}
  end

  def handle_cancel_comment_reply(_params) do
    IO.puts("❌ Cancelling comment reply - resetting state")

    {:ok, %{
      reply_comment: false,
      replying_to_comment_id: nil
    }}
  end

  def handle_reply_comment_content(%{"comment" => comment_params} = _params, user_id, comments) do
    case create_reply(comment_params, user_id, comments) do
      {:ok, %{comments: updated_comments, reply_comment: reply_comment, replying_to_comment_id: replying_to_comment_id}} ->
        {:ok, %{
          comments: updated_comments,
          reply_comment: reply_comment,
          replying_to_comment_id: replying_to_comment_id
        }}

      {:error, _reason} ->
        {:error, %{flash: {:error, "Failed to create reply"}}}
    end
  end

  def handle_delete_comment(%{"comment-id" => comment_id, "post-id" => post_id} = _params, comments) do
    IO.puts("🗑️ Deleting comment #{comment_id} with optimistic update")

    comment_to_delete = Enum.find(comments, fn comment ->
      to_string(comment.id) == to_string(comment_id)
    end)

    updated_comments = remove_comment_from_list(comments, comment_id)

    component_pid = self()
    Task.start(fn ->
      try do
        comment_id_charlist = comment_id |> to_charlist
        :postdb.delete_comment_from_mnesia(comment_id_charlist)

        clear_content_cache(:comment, comment_id_charlist)
        clear_content_cache(:comment_status, comment_id_charlist)

        IO.puts("✅ Comment #{comment_id} deleted successfully from backend")
        send(component_pid, {:comment_deleted_success, comment_id})

      rescue
        e ->
          IO.puts("❌ Failed to delete comment #{comment_id}: #{inspect(e)}")
          send(component_pid, {:comment_deletion_failed, comment_id, comment_to_delete})
      end
    end)

    {:ok, %{comments: updated_comments}}
  end

  def handle_delete_reply(%{"reply-id" => reply_id, "comment-id" => comment_id}, comments) do
    IO.puts("🗑️ Deleting reply #{reply_id} from comment #{comment_id} with optimistic update")

    reply_to_delete = comments
    |> Enum.find(fn comment -> to_string(comment.id) == to_string(comment_id) end)
    |> case do
      nil -> nil
      comment ->
        Enum.find(comment.replies || [], fn reply ->
          to_string(reply.id) == to_string(reply_id)
        end)
    end

    updated_comments = remove_reply_from_comments(
      comments,
      comment_id,
      reply_id
    )

    component_pid = self()
    Task.start(fn ->
      try do
        reply_id_charlist = reply_id |> to_charlist
        :postdb.delete_reply_from_mnesia(reply_id_charlist)

        clear_content_cache(:reply, reply_id_charlist)

        IO.puts("✅ Reply #{reply_id} deleted successfully from backend")
        send(component_pid, {:reply_deleted_success, reply_id, comment_id})

      rescue
        e ->
          IO.puts("❌ Failed to delete reply #{reply_id}: #{inspect(e)}")
          send(component_pid, {:reply_deletion_failed, reply_id, comment_id, reply_to_delete})
      end
    end)

    {:ok, %{comments: updated_comments}}
  end

  def handle_show_comments(%{"id" => post_id}) do
    comments = get_comments_with_content(post_id)
    {:ok, %{comments: comments}}
  end

  def handle_like_comment(%{"comment-id" => comment_id}, post_id, user_id) do
    comment_id = comment_id |> to_charlist

    PostClient.like_comment(user_id, comment_id)

    post_id_charlist = post_id |> to_charlist
    post = rebuild_post(post_id_charlist)
    comments = get_comments_with_content(List.to_string(post_id_charlist))

    {:ok, %{
      post: post,
      comments: comments
    }}
  end

  def handle_unlike_comment(%{"comment-id" => comment_id}, post_id, user_id, comments) do
    comment_id = comment_id |> to_charlist
    user_id_charlist = user_id |> to_charlist

    comment = Enum.find(comments, fn comment -> comment.id == comment_id |> to_charlist end)

    like =
      comment_id
      |> PostClient.get_comment_likes()
      |> Enum.map(&(&1 |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)))
      |> Enum.filter(&(&1.user_id == user_id_charlist))
      |> hd()

    updated_likes = Enum.filter(comment.likes, fn like_item -> like_item != like.id end)
    :postdb.update_comment_likes(comment_id, updated_likes)

    post_id_charlist = post_id |> to_charlist
    post = rebuild_post(post_id_charlist)
    fresh_comments = get_comments_with_content(post_id_charlist)

    {:ok, %{
      post: post,
      comments: fresh_comments
    }}
  end

  def get_comments_with_content_optimized(post_id) do
    try do
      IO.puts("🔍 Fetching comments OPTIMIZED for post #{post_id}")

      cache_key = {:comments_processed, post_id}
      case :ets.lookup(@content_cache, cache_key) do
        [{^cache_key, comments, timestamp}] ->
          age = :erlang.system_time(:second) - timestamp
          if age < 180 do
            IO.puts("📦 Comments Cache HIT for post #{post_id}")
            verify_and_fix_comment_content(comments, post_id)
          else
            fetch_and_process_comments_improved(post_id, cache_key)
          end
        [] ->
          fetch_and_process_comments_improved(post_id, cache_key)
      end
    rescue
      e ->
        IO.puts("❌ Error getting comments: #{inspect(e)}")
        []
    end
  end

  def fetch_and_process_comments_improved(post_id, cache_key) do
    comments_start = :erlang.system_time(:millisecond)

    task = Task.async(fn -> Posts.get_comment_by_post_id(post_id) end)
    comments = case Task.yield(task, 2000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result || []
      nil -> []
    end

    IO.inspect(length(comments), label: "Number of comments found")

    processed_comments = comments
    |> Enum.take(5)
    |> Enum.map(fn comment ->
      process_single_comment_with_content(comment)
    end)
    |> Enum.filter(&(&1 != nil))

    :ets.insert(@content_cache, {cache_key, processed_comments, :erlang.system_time(:second)})

    comments_end = :erlang.system_time(:millisecond)
    IO.puts("✅ Comments processed in #{comments_end - comments_start}ms")

    processed_comments
  end

  def process_single_comment_with_content(comment) do
    try do
      IO.puts("📝 Processing comment #{comment.id}")

      content = get_comment_content_with_fallbacks(comment.id)

      comment
      |> Map.put(:content, content)
      |> Map.put(:like_comment_event, like_comment_event_cached(comment.id))
      |> Map.put(:replies, get_comment_replies_optimized(comment.id))
    rescue
      e ->
        IO.puts("❌ Error processing comment #{comment.id}: #{inspect(e)}")
        nil
    end
  end

  def get_comment_content_with_fallbacks(comment_id) do
    case get_cached_content(:comment, comment_id) do
      content when is_binary(content) and content != "" and
                content not in ["Loading...", "Loading content...", "Content loading...", "Content temporarily unavailable"] ->
        IO.puts("📦 Using cached content for comment #{comment_id}")
        content

      _ ->
        case attempt_synchronous_fetch(comment_id) do
          {:ok, content} when is_binary(content) and content != "" ->
            cache_content(:comment, comment_id, content)
            content
          _ ->
            case fetch_comment_content_final_attempt(comment_id) do
              {:ok, content} ->
                cache_content(:comment, comment_id, content)
                content
              _ ->
                spawn_comment_content_fetch_improved(comment_id)
                "Loading content..."
            end
        end
    end
  end

  def attempt_synchronous_fetch(comment_id) do
    attempts = [
      fn -> fetch_comment_content_direct(comment_id) end,
      fn -> fetch_comment_content_alternative(comment_id) end,
      fn -> fetch_from_ipfs_alternative(comment_id) end
    ]

    Enum.reduce_while(attempts, {:error, :all_failed}, fn attempt, _acc ->
      case attempt.() do
        {:ok, content} when is_binary(content) and content != "" ->
          {:halt, {:ok, content}}
        _ ->
          Process.sleep(200)
          {:cont, {:error, :failed}}
      end
    end)
  end

  def fetch_comment_content_final_attempt(comment_id) do
  fetch_start = :erlang.system_time(:millisecond)
  IO.puts("📡 Starting final attempt to fetch comment content for #{comment_id}")

  task = Task.async(fn ->
    try do
      case Core.PostClient.get_comment_content(comment_id) do
        content when is_binary(content) and content != "" ->
          {:ok, content}
        content when is_list(content) ->
          string_content = List.to_string(content)
          if string_content != "" do
            {:ok, string_content}
          else
            case Posts.get_comment_by_id(comment_id) do
              %{content: db_content} when is_binary(db_content) and db_content != "" ->
                {:ok, db_content}
              _ ->
                {:error, :no_content}
            end
          end
        _ ->
          {:error, :no_content}
      end
    rescue
      e ->
        IO.puts("❌ Error in final fetch for comment #{comment_id}: #{inspect(e)}")
        {:error, e}
    end
  end)

  result = case Task.yield(task, 1500) || Task.shutdown(task, :brutal_kill) do
    {:ok, result} ->
      IO.puts("✅ Final fetch completed for comment #{comment_id}")
      result
    nil ->
      IO.puts("⏰ Timeout after 1500ms in final fetch for comment #{comment_id}")
      {:error, :timeout}
  end

  fetch_end = :erlang.system_time(:millisecond)
  IO.puts("📡 Final fetch for comment #{comment_id} took #{fetch_end - fetch_start}ms")
  result
end

  def fetch_comment_content_direct(comment_id) do
    task = Task.async(fn ->
      try do
        case Core.PostClient.get_comment_content(comment_id) do
          content when is_binary(content) and content != "" ->
            {:ok, content}
          content when is_list(content) ->
            string_content = List.to_string(content)
            if string_content != "" do
              {:ok, string_content}
            else
              {:error, :empty_content}
            end
          _ ->
            {:error, :no_content}
        end
      rescue
        e -> {:error, e}
      end
    end)

    case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  def fetch_comment_content_alternative(comment_id) do
    try do
      case Posts.get_comment_by_id(comment_id) do
        %{content: content} when is_binary(content) and content != "" ->
          {:ok, content}
        %{content: content} when is_list(content) ->
          {:ok, List.to_string(content)}
        _ ->
          fetch_from_ipfs_alternative(comment_id)
      end
    rescue
      _ -> {:error, :alternative_failed}
    end
  end

  def fetch_from_ipfs_alternative(comment_id) do
    try do
      case :postdb.get_comment_content(comment_id) do
        content when is_binary(content) and content != "" ->
          {:ok, content}
        content when is_list(content) ->
          string_content = List.to_string(content)
          if string_content != "" do
            {:ok, string_content}
          else
            {:error, :empty_alternative}
          end
        _ ->
          {:error, :no_alternative_content}
      end
    rescue
      _ -> {:error, :ipfs_alternative_failed}
    end
  end

  def spawn_comment_content_fetch_improved(comment_id) do
    parent_pid = self()

    Task.start(fn ->
      IO.puts("🔄 Background fetch started for comment #{comment_id}")

      content = case fetch_comment_content_direct(comment_id) do
        {:ok, content} -> content
        _ ->
          case fetch_comment_content_alternative(comment_id) do
            {:ok, content} -> content
            _ ->
              case fetch_content_with_timeout(:comment, comment_id, 5000) do
                {:ok, content} -> content
                _ -> nil
              end
          end
      end

      if content && content != "" do
        cache_content(:comment, comment_id, content)
        send(parent_pid, {:comment_content_updated, comment_id, content})
        IO.puts("✅ Background fetch completed for comment #{comment_id}")
      else
        IO.puts("❌ Background fetch failed for comment #{comment_id}")
      end
    end)
  end

  def update_comment_content_in_list(comments, comment_id, content) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        Map.put(comment, :content, content)
      else
        comment
      end
    end)
  end

  def verify_and_fix_comment_content(comments, post_id) do
    needs_refresh = Enum.any?(comments, fn comment ->
      comment.content in ["Loading content...", "Loading...", "Content loading...", nil, ""]
    end)

    if needs_refresh do
      IO.puts("🔧 Some comments need content refresh, fetching fresh data")
      fetch_and_process_comments_improved(post_id, {:comments_processed, post_id})
    else
      comments
    end
  end

  def get_comment_replies_optimized(comment_id) do
    try do
      IO.puts("🔄 Fetching replies for comment #{comment_id}")

      task = Task.async(fn ->
        :postdb.get_comment_replies(comment_id |> to_charlist)
      end)

      case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
        {:ok, replies} ->
          replies
          |> Enum.take(3)
          |> Enum.map(fn reply ->
            case reply |> Mazaryn.Schema.Reply.erl_changeset() |> Mazaryn.Schema.Reply.build() do
              {:ok, built_reply} ->
                content = get_reply_content_with_fallbacks(built_reply.id)
                Map.put(built_reply, :content, content)
              {:error, _} -> nil
            end
          end)
          |> Enum.filter(&(&1 != nil))

        nil -> []
      end
    rescue
      e ->
        IO.puts("❌ Error getting replies: #{inspect(e)}")
        []
    end
  end

  def get_reply_content_with_fallbacks(reply_id) do
    case get_cached_content(:reply, reply_id) do
      content when is_binary(content) and content != "" and content != "Loading..." ->
        IO.puts("📦 Using cached content for reply #{reply_id}")
        content

      _ ->
        case fetch_reply_content_direct(reply_id) do
          {:ok, content} when is_binary(content) and content != "" ->
            cache_content(:reply, reply_id, content)
            content

          _ ->
            case fetch_reply_content_alternative(reply_id) do
              {:ok, content} ->
                cache_content(:reply, reply_id, content)
                content

              _ ->
                spawn_reply_content_fetch_improved(reply_id)
                "Loading reply..."
            end
        end
    end
  end

  def fetch_reply_content_direct(reply_id) do
    task = Task.async(fn ->
      try do
        case Core.PostClient.get_reply_content(reply_id) do
          content when is_binary(content) and content != "" ->
            {:ok, content}
          content when is_list(content) ->
            string_content = List.to_string(content)
            if string_content != "" do
              {:ok, string_content}
            else
              {:error, :empty_content}
            end
          _ ->
            {:error, :no_content}
        end
      rescue
        e -> {:error, e}
      end
    end)

    case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  def fetch_reply_content_alternative(reply_id) do
    try do
      case :postdb.get_reply_by_id(reply_id) do
        reply_data when reply_data != nil ->
          case reply_data |> Mazaryn.Schema.Reply.erl_changeset() |> Mazaryn.Schema.Reply.build() do
            {:ok, %{content: content}} when is_binary(content) and content != "" ->
              {:ok, content}
            {:ok, %{content: content}} when is_list(content) ->
              {:ok, List.to_string(content)}
            _ ->
              fetch_reply_from_ipfs_alternative(reply_id)
          end
        _ ->
          fetch_reply_from_ipfs_alternative(reply_id)
      end
    rescue
      _ -> {:error, :alternative_failed}
    end
  end

  def fetch_reply_from_ipfs_alternative(reply_id) do
    try do
      case :postdb.get_reply_content_alternative(reply_id) do
        content when is_binary(content) and content != "" ->
          {:ok, content}
        content when is_list(content) ->
          string_content = List.to_string(content)
          if string_content != "" do
            {:ok, string_content}
          else
            {:error, :empty_alternative}
          end
        _ ->
          {:error, :no_alternative_content}
      end
    rescue
      _ -> {:error, :ipfs_alternative_failed}
    end
  end

  def spawn_reply_content_fetch_improved(reply_id) do
    parent_pid = self()

    Task.start(fn ->
      IO.puts("🔄 Background fetch started for reply #{reply_id}")

      content = case fetch_reply_content_direct(reply_id) do
        {:ok, content} -> content
        _ ->
          case fetch_reply_content_alternative(reply_id) do
            {:ok, content} -> content
            _ ->
              case fetch_content_with_timeout(:reply, reply_id, 5000) do
                {:ok, content} -> content
                _ -> nil
              end
          end
      end

      if content && content != "" do
        cache_content(:reply, reply_id, content)
        send(parent_pid, {:reply_content_updated, reply_id, content})
        IO.puts("✅ Background fetch completed for reply #{reply_id}")
      else
        IO.puts("❌ Background fetch failed for reply #{reply_id}")
      end
    end)
  end

  def update_reply_content_in_comments(comments, reply_id, content) do
    Enum.map(comments, fn comment ->
      updated_replies = Enum.map(comment.replies || [], fn reply ->
        if to_string(reply.id) == to_string(reply_id) do
          Map.put(reply, :content, content)
        else
          reply
        end
      end)
      Map.put(comment, :replies, updated_replies)
    end)
  end

  def save_comment(%{post_id: post_id, author: author, content: content}) do
    changeset = %Comment{}
      |> Comment.changeset(%{
        post_id: post_id,
        author: author,
        content: content
      })

    case Posts.create_comment(changeset) do
      {:ok, comment} ->
        cache_content(:comment, comment.id, content)

        :ets.delete(@content_cache, {:comments_processed, post_id})

        comments = get_comments_with_content_optimized(post_id)
        post = rebuild_post(post_id)

        {:ok, %{
          post: post,
          comments: comments,
          changeset: Comment.changeset(%Comment{})
        }}

      {:error, changeset} ->
        IO.puts("❌ Error creating comment: #{inspect(changeset.errors)}")
        {:error, changeset}
    end
  end

  def save_comment(_) do
    IO.puts("❌ Missing required comment parameters")
    {:error, :missing_params}
  end

  def create_reply(%{"comment_id" => comment_id, "content" => content}, user_id, comments) do
    IO.puts("🚀 Creating reply for comment #{comment_id} with content: #{content}")

    temp_reply = create_temp_reply(user_id, comment_id, content)
    updated_comments = add_reply_to_comment(comments, comment_id, temp_reply)

    parent_pid = self()
    Task.start(fn ->
      case PostClient.reply_comment(user_id, to_charlist(comment_id), content) do
        {:ok, reply} ->
          IO.puts("✅ Reply saved successfully: #{inspect(reply.id)}")
          if reply && reply.id do
            cache_content(:reply, reply.id, content)
          end
          send(parent_pid, {:reply_saved, comment_id, reply, temp_reply.id})

        error ->
          IO.puts("❌ Error saving reply: #{inspect(error)}")
          send(parent_pid, {:reply_failed, comment_id, temp_reply.id})
      end
    end)

    {:ok, %{
      comments: updated_comments,
      reply_comment: false,
      replying_to_comment_id: nil
    }}
  end

  def update_comment(%{"id" => comment_id, "content" => new_content} = comment_params) do
    IO.puts("📝 UPDATE-COMMENT EVENT TRIGGERED")

    try do
      changeset = %Comment{} |> Comment.update_changeset(comment_params)

      if changeset.valid? do
        result = Posts.update_comment(changeset)

        case result do
          {:ok, updated_comment} ->
            handle_successful_update(comment_id, new_content, updated_comment)

          {:error, changeset} ->
            IO.puts("❌ Error updating comment: #{inspect(changeset.errors)}")
            {:error, changeset}

          %{} ->
            IO.puts("⚠️ Posts.update_comment returned empty map, trying direct method")
            update_comment_direct(comment_params)

          other ->
            IO.puts("❌ Unexpected return from Posts.update_comment: #{inspect(other)}")
            IO.puts("🔄 Falling back to direct update method")
            update_comment_direct(comment_params)
        end
      else
        {:error, %{changeset | action: :validate}}
      end
    rescue
      e ->
        IO.puts("❌ Exception in update_comment, trying direct method: #{inspect(e)}")
        update_comment_direct(comment_params)
    end
  end

  def update_comment(comment_params) when is_map(comment_params) do
    comment_id = comment_params["id"] || comment_params[:id]
    new_content = comment_params["content"] || comment_params[:content]

    if comment_id && new_content do
      update_comment_simple(%{"id" => comment_id, "content" => new_content})
    else
      IO.puts("❌ Missing required parameters for comment update")
      IO.inspect(comment_params, label: "Received params")

      changeset = %Comment{}
      |> Comment.changeset(comment_params)
      |> Map.put(:action, :validate)

      {:error, changeset}
    end
  end

  def update_comment_direct(%{"id" => comment_id, "content" => new_content} = comment_params) do
    IO.puts("📝 DIRECT UPDATE-COMMENT EVENT TRIGGERED")
    IO.inspect(comment_params, label: "Comment params received")

    try do
      changeset = %Comment{} |> Comment.update_changeset(comment_params)

      if changeset.valid? do
        comment_id_charlist = if is_binary(comment_id), do: to_charlist(comment_id), else: comment_id

        case :postdb.update_comment_content(comment_id_charlist, new_content) do
          :ok ->
            IO.puts("✅ Comment updated successfully via postdb")

            cache_content(:comment, comment_id, new_content)

            case get_comment_post_id(comment_id) do
              {:ok, post_id} ->
                post = rebuild_post(post_id)

                {:ok, %{
                  post: post,
                  comment_id: comment_id,
                  new_content: new_content,
                  update_comment_changeset: Comment.changeset(%Comment{}),
                  editing_comment: false,
                  editing_comment_id: nil
                }}

              {:error, _} ->
                IO.puts("⚠️ Could not rebuild post, but comment was updated")
                {:ok, %{
                  post: nil,
                  comment_id: comment_id,
                  new_content: new_content,
                  update_comment_changeset: Comment.changeset(%Comment{}),
                  editing_comment: false,
                  editing_comment_id: nil
                }}
            end

          error ->
            IO.puts("❌ Failed to update comment via postdb: #{inspect(error)}")
            {:error, %{changeset | action: :update, errors: [content: {"Update failed", []}]}}
        end
      else
        IO.puts("❌ Invalid changeset for comment update: #{inspect(changeset.errors)}")
        {:error, %{changeset | action: :validate}}
      end
    rescue
      e ->
        IO.puts("❌ Exception in update_comment_direct: #{inspect(e)}")
        changeset = %Comment{} |> Comment.update_changeset(comment_params)
        {:error, %{changeset | action: :update, errors: [content: {"Update failed: #{inspect(e)}", []}]}}
    end
  end

  def update_comment_simple(%{"id" => comment_id, "content" => new_content}) do
    IO.puts("📝 SIMPLE UPDATE-COMMENT EVENT TRIGGERED")

    try do
      if new_content && String.trim(new_content) != "" do
        cache_content(:comment, comment_id, new_content)
        IO.puts("💾 Cached new content for comment #{comment_id}")

        comment_id_charlist = if is_binary(comment_id), do: to_charlist(comment_id), else: comment_id

        Task.start(fn ->
          try do
            case :postdb.update_comment_content(comment_id_charlist, new_content) do
              :ok ->
                IO.puts("✅ Comment #{comment_id} updated successfully in backend")
              error ->
                IO.puts("⚠️ Backend update failed for comment #{comment_id}: #{inspect(error)}")
            end
          rescue
            e ->
              IO.puts("❌ Exception during backend update: #{inspect(e)}")
          end
        end)

        {:ok, %{
          post: nil,
          comment_id: comment_id,
          new_content: new_content,
          update_comment_changeset: Comment.changeset(%Comment{}),
          editing_comment: false,
          editing_comment_id: nil
        }}
      else
        changeset = %Comment{}
        |> Comment.changeset(%{content: new_content})
        |> Map.put(:action, :validate)

        {:error, changeset}
      end
    rescue
      e ->
        IO.puts("❌ Exception in update_comment_simple: #{inspect(e)}")
        changeset = %Comment{}
        |> Comment.changeset(%{content: new_content || ""})
        |> Map.put(:action, :update)
        |> Ecto.Changeset.add_error(:content, "Update failed: #{inspect(e)}")

        {:error, changeset}
    end
  end

  defp handle_successful_update(comment_id, new_content, updated_comment) do
    if comment_id && new_content do
      cache_content(:comment, comment_id, new_content)
      IO.puts("💾 Immediately cached new content for comment #{comment_id}")
    end

    post = rebuild_post(updated_comment.post_id)

    {:ok, %{
      post: post,
      comment_id: comment_id,
      new_content: new_content,
      update_comment_changeset: Comment.changeset(%Comment{}),
      editing_comment: false,
      editing_comment_id: nil
    }}
  end

  def create_temp_reply(user_id, comment_id, content) do
    temp_id = "temp_" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))

    cache_content(:reply, temp_id, content)

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

  def add_reply_to_comment(comments, comment_id, new_reply) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        existing_replies = comment.replies || []
        updated_replies = existing_replies ++ [new_reply]

        IO.puts("📝 Added reply to comment #{comment_id}. Total replies: #{length(updated_replies)}")
        Map.put(comment, :replies, updated_replies)
      else
        comment
      end
    end)
  end

  def replace_temp_reply_with_real(comments, comment_id, temp_id, real_reply) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        updated_replies = Enum.map(comment.replies || [], fn reply ->
          if reply.id == temp_id do
            preserved_content = reply.content
            cache_content(:reply, real_reply.id, preserved_content)

            %{
              id: real_reply.id,
              content: preserved_content,
              user_id: real_reply.user_id,
              comment_id: comment_id,
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
          reply.id == temp_id
        end)

        Map.put(comment, :replies, updated_replies)
      else
        comment
      end
    end)
  end

  def update_comment_content_optimistically(comments, comment_id, new_content) do
    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        IO.puts("✅ Optimistically updating comment #{comment_id} with new content")
        Map.put(comment, :content, new_content)
      else
        comment
      end
    end)
  end

  def remove_reply_from_comments(comments, comment_id, reply_id) do
    IO.puts("🗑️ Optimistically removing reply #{reply_id} from comment #{comment_id}")

    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        updated_replies = Enum.reject(comment.replies || [], fn reply ->
          to_string(reply.id) == to_string(reply_id)
        end)

        IO.puts("📝 Comment #{comment_id} now has #{length(updated_replies)} replies (was #{length(comment.replies || [])})")
        %{comment | replies: updated_replies}
      else
        comment
      end
    end)
  end

  def remove_comment_from_list(comments, comment_id) do
    IO.puts("🗑️ Optimistically removing comment #{comment_id}")

    filtered_comments = Enum.reject(comments, fn comment ->
      to_string(comment.id) == to_string(comment_id)
    end)

    IO.puts("📝 Comments list now has #{length(filtered_comments)} comments (was #{length(comments)})")
    filtered_comments
  end

  def restore_reply_to_comments(comments, comment_id, reply) do
    IO.puts("🔄 Restoring reply #{reply.id} to comment #{comment_id} due to deletion failure")

    Enum.map(comments, fn comment ->
      if to_string(comment.id) == to_string(comment_id) do
        existing_replies = comment.replies || []

        reply_exists = Enum.any?(existing_replies, fn existing_reply ->
          to_string(existing_reply.id) == to_string(reply.id)
        end)

        if reply_exists do
          IO.puts("⚠️ Reply #{reply.id} already exists in comment #{comment_id}, skipping restore")
          comment
        else
          updated_replies = existing_replies ++ [reply]
          %{comment | replies: updated_replies}
        end
      else
        comment
      end
    end)
  end

  def restore_comment_to_list(comments, comment) do
    IO.puts("🔄 Restoring comment #{comment.id} due to deletion failure")

    comment_exists = Enum.any?(comments, fn existing_comment ->
      to_string(existing_comment.id) == to_string(comment.id)
    end)

    if comment_exists do
      IO.puts("⚠️ Comment #{comment.id} already exists in list, skipping restore")
      comments
    else
      comments ++ [comment]
    end
  end

  def find_comment_by_id(comments, comment_id) do
    Enum.find(comments, fn comment ->
      to_string(comment.id) == to_string(comment_id)
    end)
  end

  def find_reply_by_id(comment, reply_id) do
    (comment.replies || [])
    |> Enum.find(fn reply ->
      to_string(reply.id) == to_string(reply_id)
    end)
  end

  def get_comment_post_id(comment_id) do
    try do
      case Posts.get_comment_by_id(comment_id) do
        %{post_id: post_id} when post_id != nil ->
          IO.puts("✅ Found post_id #{post_id} for comment #{comment_id} via Posts module")
          {:ok, post_id}

        comment when is_map(comment) ->
          post_id = comment[:post_id] || comment.post_id || comment["post_id"]
          if post_id do
            {:ok, post_id}
          else
            IO.puts("⚠️ Comment found but no post_id field")
            try_alternative_comment_lookup(comment_id)
          end

        _ ->
          IO.puts("⚠️ Comment not found via Posts module, trying alternative")
          try_alternative_comment_lookup(comment_id)
      end
    rescue
      e ->
        IO.puts("❌ Error in Posts.get_comment_by_id: #{inspect(e)}")
        try_alternative_comment_lookup(comment_id)
    end
  end

  def try_alternative_comment_lookup(comment_id) do
    try do
      comment_id_charlist = if is_binary(comment_id), do: to_charlist(comment_id), else: comment_id

      case :postdb.get_comment_by_id(comment_id_charlist) do
        comment_data when comment_data != nil ->
          case comment_data |> Mazaryn.Schema.Comment.erl_changeset() |> Mazaryn.Schema.Comment.build() do
            {:ok, %{post_id: post_id}} when post_id != nil ->
              IO.puts("✅ Found post_id #{post_id} for comment #{comment_id} via postdb")
              {:ok, post_id}

            {:ok, comment} ->
              post_id = comment.post_id || Map.get(comment, :post_id)
              if post_id do
                {:ok, post_id}
              else
                IO.puts("❌ Built comment has no post_id")
                {:error, :no_post_id}
              end

            error ->
              IO.puts("❌ Failed to build comment from erlang data: #{inspect(error)}")
              {:error, :build_failed}
          end

        _ ->
          IO.puts("❌ Comment not found in postdb")
          {:error, :comment_not_found}
      end
    rescue
      e ->
        IO.puts("❌ Error in postdb lookup: #{inspect(e)}")
        {:error, :lookup_failed}
    end
  end

  def get_comments_with_content(post_id) do
    get_comments_with_content_optimized(post_id)
  end

  def get_comments_with_content_reliable(post_id) do
    get_comments_with_content_optimized(post_id)
  end

  def like_comment_event_cached(_comment_id) do
    "like-comment"
  end

  defp get_cached_content(type, id) do
    cache_key = {type, id}
    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, content, timestamp}] ->
        age = :erlang.system_time(:second) - timestamp
        if age < 600 do
          case content do
            content when is_binary(content) and content != "" and
                         content not in ["Loading...", "Loading content...", "Content loading...",
                                       "Content temporarily unavailable", "Content loading failed"] ->
              IO.puts("📦 Valid cache HIT for #{type}:#{id} (age: #{age}s)")
              content
            _ ->
              IO.puts("⚠️ Invalid cached content for #{type}:#{id}, removing from cache")
              :ets.delete(@content_cache, cache_key)
              nil
          end
        else
          IO.puts("⏰ Cache EXPIRED for #{type}:#{id} (age: #{age}s)")
          :ets.delete(@content_cache, cache_key)
          nil
        end
      [] ->
        IO.puts("❌ Cache MISS for #{type}:#{id}")
        nil
    end
  end

  def cache_content(type, id, content) do
    if content not in ["Loading...", "Loading content...", "Content loading...",
                    "Content temporarily unavailable", "Content loading failed"] do
      cache_key = {type, id}
      timestamp = :erlang.system_time(:second)
      :ets.insert(@content_cache, {cache_key, content, timestamp})
      IO.puts("💾 Cached content for #{type}:#{id}")
    else
      IO.puts("⚠️ Skipping cache for loading state: #{content}")
    end
    content
  end

  def fetch_content_with_timeout(type, id, timeout) do
    task = Task.async(fn ->
      try do
        ipfs_start = :erlang.system_time(:millisecond)

        content = case type do
          :comment -> Core.PostClient.get_comment_content(id)
          :reply -> Core.PostClient.get_reply_content(id)
        end

        result = case content do
          {:error, :content_processing} ->
            {:error, :content_processing}
          {:error, :content_cache_missing} ->
            {:error, :content_cache_missing}
          content when is_binary(content) and content != "" ->
            {:ok, content}
          content when is_list(content) ->
            case List.to_string(content) do
              "" -> {:error, :empty_content}
              str -> {:ok, str}
            end
          _ ->
            {:error, :invalid_content}
        end

        ipfs_end = :erlang.system_time(:millisecond)
        duration = ipfs_end - ipfs_start
        IO.puts("📡 IPFS #{type} content fetch for #{id} took #{duration}ms")

        if duration > 2000 do
          IO.puts("🚨 SLOW IPFS #{String.upcase(to_string(type))} FETCH: #{duration}ms for #{type} #{id}")
        end

        result
      catch
        _, e ->
          IO.puts("❌ Error fetching #{type} content from IPFS: #{inspect(e)}")
          {:error, e}
      end
    end)

    case Task.yield(task, timeout) || Task.shutdown(task) do
      {:ok, result} -> result
      nil -> {:timeout, "Timeout after #{timeout}ms"}
    end
  end

  defp rebuild_post(post_id) do
    {:ok, post} =
      PostClient.get_by_id(post_id)
      |> Mazaryn.Schema.Post.erl_changeset()
      |> Mazaryn.Schema.Post.build()
    post
  end

  defp clear_content_cache(type, id) do
    cache_key = {type, id}
    :ets.delete(@content_cache, cache_key)
  end
end
