defmodule MazarynWeb.HomeLive.Home do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Mazaryn.Schema.Post
  alias Mazaryn.Posts
  alias Account.Users
  alias Account.User
  require Logger

  @user_cache_ttl 60_000
  @posts_cache_ttl 30_000
  @comments_cache_ttl 45_000
  @search_cache_ttl 120_000

  @user_load_timeout 2_000
  @posts_load_timeout 3_000
  @comments_load_timeout 1_500
  @search_timeout 1_000

  @posts_per_page 20
  @initial_posts_limit 10

  @impl true
  def handle_info({:ipns_ready, post_id, ipns_id}, socket) do
    ipns_start = :erlang.system_time(:millisecond)
    Logger.info("🌐 IPNS ready for post #{post_id}, IPNS ID: #{ipns_id}")

    updated_posts = update_post_in_list(socket.assigns.posts, post_id, fn post ->
      Map.put(post, :ipns_id, ipns_id)
    end)

    ipns_end = :erlang.system_time(:millisecond)
    Logger.info("🌐 IPNS ready handling completed in #{ipns_end - ipns_start}ms")

    {:noreply, assign(socket, posts: updated_posts)}
  end

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🏠 Starting MOUNT MazarynWeb.HomeLive.Home for session_uuid #{session_uuid}")

    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "home_feed_updates")
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "user_updates")
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "post_updates")

    socket = socket
    |> assign_initial_state()
    |> assign(session_uuid: session_uuid)

    user_id = get_user_id(session_uuid)
    result = {:ok, do_mount_async_optimized(user_id, socket)}

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🏠 MOUNT MazarynWeb.HomeLive.Home completed in #{mount_end - mount_start}ms")
    result
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🏠 Starting MOUNT MazarynWeb.HomeLive.Home for user_id #{user_id}")

    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "home_feed_updates")
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "user_updates")
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "post_updates")

    socket = socket
    |> assign_initial_state()
    |> assign(user_id: user_id)

    result = {:ok, do_mount_async_optimized(user_id, socket)}

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🏠 MOUNT MazarynWeb.HomeLive.Home completed in #{mount_end - mount_start}ms")
    result
  end

  @impl true
  def handle_params(_params, url, socket) do
    params_start = :erlang.system_time(:millisecond)
    Logger.info("🏠 Starting HANDLE PARAMS MazarynWeb.HomeLive.Home")

    socket = assign(socket, current_path: URI.parse(url).path)

    params_end = :erlang.system_time(:millisecond)
    Logger.info("🏠 HANDLE PARAMS MazarynWeb.HomeLive.Home completed in #{params_end - params_start}ms")
    {:noreply, socket}
  end

  @impl true
  def handle_event("show-comments", %{"id" => post_id}, socket) do
    event_start = :erlang.system_time(:millisecond)
    Logger.info("💬 Starting handle_event show-comments for post #{post_id}")

    import Phoenix.LiveView.JS
    JS.toggle(to: "#test-toggle", in: "fade-in-scale", out: "fade-out-scale")

    send(self(), {:load_comments_cached, post_id})

    event_end = :erlang.system_time(:millisecond)
    Logger.info("💬 handle_event show-comments completed in #{event_end - event_start}ms")
    {:noreply, socket |> assign(:comments, []) |> assign(:comments_loading, true)}
  end

  @impl true
  def handle_event("load-more-posts", _params, socket) do
    Logger.info("📚 Loading more posts")
    user = socket.assigns.user
    current_posts = socket.assigns.posts
    offset = length(current_posts)

    if user do
      send(self(), {:load_more_posts_async, user.id, offset})
      {:noreply, assign(socket, posts_loading_more: true)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("refresh-feed", _params, socket) do
    Logger.info("🔄 Refreshing feed")
    user = socket.assigns.user

    if user do
      clear_posts_cache(user.id)
      send(self(), {:load_posts_async_priority, user.id})
      {:noreply, assign(socket, posts_loading: true)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("cancel-comment-reply", %{"value" => _value}, socket) do
    Logger.info("🚫 Cancelling comment reply")
    {:noreply, socket}
  end

  @impl true
  def handle_event("messages", _param, socket) do
    Logger.info("📨 Redirecting to messages")
    random_id = "/messages/" <> "1"
    {:noreply, push_redirect(socket, to: random_id)}
  end

  def handle_event("do_search", %{"search" => search}, socket) do
    search_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting handle_event do_search for query #{search}")

    send(self(), {:search_users_cached, search})

    search_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 handle_event do_search completed in #{search_end - search_start}ms")
    {:noreply, assign(socket, search: search, results: [], search_loading: true)}
  end

  def handle_event("select_emoji", %{"name" => name}, socket) do
    Logger.info("😀 Selected emoji: #{name}")
    {:noreply, socket}
  end

  @impl true
  def handle_info({:load_posts_async_priority, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("📚 Starting PRIORITY async load_posts_async for user_id #{user_id}")

    posts_task = Task.async(fn ->
      get_user_and_following_posts_optimized(user_id, @initial_posts_limit)
    end)

    posts_result = case Task.yield(posts_task, @posts_load_timeout) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, posts} ->
        Logger.info("📚 Successfully loaded #{length(posts)} priority posts for home feed")
        posts
      nil ->
        Logger.warn("⏰ Timeout loading priority posts, using cached or empty list")
        get_cached_posts_fallback(user_id)
    end

    socket = socket
    |> assign(posts: posts_result)
    |> assign(posts_loading: false)
    |> assign(last_post_loaded_at: :erlang.system_time(:millisecond))

    send(self(), {:preload_more_posts, user_id, @initial_posts_limit})

    load_end = :erlang.system_time(:millisecond)
    Logger.info("📚 Priority async load_posts_async completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:load_posts_async, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("📚 Starting async load_posts_async for user_id #{user_id}")

    posts_task = Task.async(fn ->
      get_user_and_following_posts_cached_optimized(user_id, @initial_posts_limit)
    end)

    posts_result = case Task.yield(posts_task, @posts_load_timeout) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, posts} ->
        Logger.info("📚 Successfully loaded #{length(posts)} posts for home feed")
        posts
      nil ->
        Logger.warn("⏰ Timeout loading posts, using cached or empty list")
        get_cached_posts_fallback(user_id)
    end

    socket = socket
    |> assign(posts: posts_result)
    |> assign(posts_loading: false)
    |> assign(last_post_loaded_at: :erlang.system_time(:millisecond))

    load_end = :erlang.system_time(:millisecond)
    Logger.info("📚 Async load_posts_async completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:load_more_posts_async, user_id, offset}, socket) do
    Logger.info("📚 Loading more posts from offset #{offset}")

    posts_task = Task.async(fn ->
      get_user_and_following_posts_optimized(user_id, @posts_per_page, offset)
    end)

    new_posts = case Task.yield(posts_task, @posts_load_timeout) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, posts} ->
        Logger.info("📚 Successfully loaded #{length(posts)} more posts")
        posts
      nil ->
        Logger.warn("⏰ Timeout loading more posts")
        []
    end

    current_posts = socket.assigns.posts
    all_posts = current_posts ++ new_posts

    {:noreply, assign(socket, posts: all_posts, posts_loading_more: false)}
  end

  def handle_info({:preload_more_posts, user_id, offset}, socket) do
    Logger.info("🔄 Preloading next batch of posts")

    Task.start(fn ->
      get_user_and_following_posts_optimized(user_id, @posts_per_page, offset)
    end)

    {:noreply, socket}
  end

  def handle_info({:load_user_and_posts_optimized, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("👤 Starting OPTIMIZED async load_user_and_posts for user_id #{user_id}")

    user_task = Task.async(fn ->
      get_user_cached_optimized(user_id)
    end)

    posts_task = Task.async(fn ->
      get_user_and_following_posts_cached_optimized(user_id, @initial_posts_limit)
    end)

    user_result = case Task.yield(user_task, @user_load_timeout) || Task.shutdown(user_task, :brutal_kill) do
      {:ok, {:ok, user}} ->
        Logger.info("👤 Successfully loaded user #{user.username}")
        user
      {:ok, {:error, reason}} ->
        Logger.error("❌ Failed to load user: #{inspect(reason)}")
        nil
      nil ->
        Logger.warn("⏰ Timeout loading user")
        nil
    end

    posts_result = case Task.yield(posts_task, @posts_load_timeout) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, posts} ->
        Logger.info("📚 Successfully loaded #{length(posts)} posts for home feed")
        posts
      nil ->
        Logger.warn("⏰ Timeout loading posts, using cached fallback")
        if user_result, do: get_cached_posts_fallback(user_result.id), else: []
    end

    socket = if user_result do
      post_changeset = Post.changeset(%Post{})
      socket
      |> assign(post_changeset: post_changeset)
      |> assign(search: "")
      |> assign(results: [])
      |> assign(user: user_result)
      |> assign(posts: posts_result)
      |> assign(posts_loading: false)
      |> assign(last_post_loaded_at: :erlang.system_time(:millisecond))

      send(self(), {:preload_more_posts, user_result.id, @initial_posts_limit})
      socket
    else
      socket |> assign(posts: [], posts_loading: false)
    end

    load_end = :erlang.system_time(:millisecond)
    Logger.info("👤 OPTIMIZED async load_user_and_posts completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:load_comments_cached, post_id}, socket) do
    comment_start = :erlang.system_time(:millisecond)
    Logger.info("💬 Starting CACHED async load_comments for post #{post_id}")

    comments_task = Task.async(fn ->
      get_comments_cached(post_id)
    end)

    comments = case Task.yield(comments_task, @comments_load_timeout) || Task.shutdown(comments_task, :brutal_kill) do
      {:ok, comments} ->
        Logger.info("💬 Successfully loaded #{length(comments)} comments for post #{post_id}")
        comments
      nil ->
        Logger.warn("⏰ Timeout loading comments for post #{post_id}")
        []
    end

    comment_end = :erlang.system_time(:millisecond)
    Logger.info("💬 CACHED async load_comments completed in #{comment_end - comment_start}ms")

    {:noreply, assign(socket, comments: comments, comments_loading: false)}
  end

  def handle_info({:search_users_cached, search}, socket) do
    search_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting CACHED async search_users for query #{search}")

    search_task = Task.async(fn ->
      search_user_by_username_cached(search)
    end)

    results = case Task.yield(search_task, @search_timeout) || Task.shutdown(search_task, :brutal_kill) do
      {:ok, users} ->
        Logger.info("🔍 Successfully found #{length(users || [])} users for query #{search}")
        users || []
      nil ->
        Logger.warn("⏰ Timeout searching users for query #{search}")
        []
    end

    search_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 CACHED async search_users completed in #{search_end - search_start}ms")

    {:noreply, assign(socket, results: results, search_loading: false)}
  end

  def handle_info(:reload_posts, socket) do
    reload_start = :erlang.system_time(:millisecond)
    Logger.info("🔄 Starting handle_info reload_posts")

    user = socket.assigns.user
    socket = if user do
      clear_posts_cache(user.id)
      send(self(), {:load_posts_async_priority, user.id})
      assign(socket, posts_loading: true)
    else
      socket
    end

    reload_end = :erlang.system_time(:millisecond)
    Logger.info("🔄 handle_info reload_posts completed in #{reload_end - reload_start}ms")
    {:noreply, socket}
  end

  def handle_info({:post_updated, post}, socket) do
    Logger.info("📝 Real-time post update received: #{post.id}")

    updated_posts = update_post_in_list(socket.assigns.posts, post.id, fn _ -> post end)
    {:noreply, assign(socket, posts: updated_posts)}
  end

  def handle_info({:post_deleted, post_id}, socket) do
    Logger.info("🗑️ Real-time post deletion received: #{post_id}")

    updated_posts = Enum.reject(socket.assigns.posts, &(&1.id == post_id))
    {:noreply, assign(socket, posts: updated_posts)}
  end

  def handle_info({:new_post, post}, socket) do
    Logger.info("✨ Real-time new post received: #{post.id}")

    current_posts = socket.assigns.posts
    updated_posts = [post | current_posts] |> Enum.take(50)
    {:noreply, assign(socket, posts: updated_posts)}
  end

  @impl true
  def handle_info({:ipns_result, _post_id, _ipns_id}, socket) do
    {:noreply, socket}
  end

  def handle_info({:comment_deleted_success, comment_id}, socket) do
    Logger.info("✅ Comment #{comment_id} deletion confirmed at Home level")
    {:noreply, socket}
  end

  def handle_info({:comment_deletion_failed, comment_id, _comment_to_delete}, socket) do
    Logger.info("❌ Comment #{comment_id} deletion failed at Home level")
    {:noreply, socket |> put_flash(:error, "Failed to delete comment")}
  end

  def handle_info({:reply_deleted_success, reply_id, _comment_id}, socket) do
    Logger.info("✅ Reply #{reply_id} deletion confirmed at Home level")
    {:noreply, socket}
  end

  def handle_info({:reply_deletion_failed, reply_id, _comment_id, _reply_to_delete}, socket) do
    Logger.info("❌ Reply #{reply_id} deletion failed at Home level")
    {:noreply, socket |> put_flash(:error, "Failed to delete reply")}
  end

  def handle_info({:comment_content_updated, comment_id, _content}, socket) do
    Logger.info("🔄 Comment content updated: #{comment_id}")
    {:noreply, socket}
  end

  def handle_info({:reply_content_updated, reply_id, _content}, socket) do
    Logger.info("🔄 Reply content updated: #{reply_id}")
    {:noreply, socket}
  end

  def handle_info(msg, socket) do
    Logger.info("⚠️ Unhandled message in Home LiveView: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp assign_initial_state(socket) do
    socket
    |> assign(results: [])
    |> assign(posts: [])
    |> assign(posts_loading: true)
    |> assign(posts_loading_more: false)
    |> assign(comments_loading: false)
    |> assign(search_loading: false)
    |> assign(last_post_loaded_at: nil)
  end

  defp do_mount_async_optimized(user_id, socket) do
    post_changeset = Post.changeset(%Post{})

    case get_user_cached_optimized(user_id) do
      {:ok, user} ->
        socket = socket
        |> assign(post_changeset: post_changeset)
        |> assign(search: "")
        |> assign(results: [])
        |> assign(user: user)
        |> assign(posts: [])
        |> assign(posts_loading: true)

        send(self(), {:load_posts_async_priority, user.id})
        socket

      {:error, reason} ->
        Logger.error("❌ Failed to load user during mount: #{inspect(reason)}")
        send(self(), {:load_user_and_posts_optimized, user_id})

        socket
        |> assign(post_changeset: post_changeset)
        |> assign(search: "")
        |> assign(results: [])
        |> assign(user: nil)
        |> assign(posts: [])
        |> assign(posts_loading: true)
    end
  end

  defp get_user_cached_optimized(user_id) do
    cache_key = {:home_user, user_id}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:user_cache, cache_key) do
      [{_, user, timestamp}] ->
        if current_time - timestamp < @user_cache_ttl do
          Logger.info("📦 Home User Cache HIT for #{user_id}")
          {:ok, user}
        else
          Logger.info("📦 Home User Cache EXPIRED for #{user_id}")
          fetch_and_cache_home_user_optimized(user_id, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Home User Cache MISS for #{user_id}")
        fetch_and_cache_home_user_optimized(user_id, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      ensure_cache_tables()
      get_user_cached_optimized(user_id)
  end

  defp fetch_and_cache_home_user_optimized(user_id, cache_key, current_time) do
    case Users.one_by_email(user_id) do
      {:ok, user} = result ->
        :ets.insert(:user_cache, {cache_key, user, current_time})
        result
      other -> other
    end
  end

  defp get_user_and_following_posts_cached_optimized(user_id, limit \\ @posts_per_page, offset \\ 0) do
    cache_key = {:home_posts, user_id, limit, offset}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:posts_cache, cache_key) do
      [{_, posts, timestamp}] ->
        if current_time - timestamp < @posts_cache_ttl do
          Logger.info("📦 Home Posts Cache HIT for #{user_id} (#{limit}/#{offset})")
          posts
        else
          Logger.info("📦 Home Posts Cache EXPIRED for #{user_id} (#{limit}/#{offset})")
          fetch_and_cache_home_posts_optimized(user_id, cache_key, current_time, limit, offset)
        end
      _ ->
        Logger.info("📦 Home Posts Cache MISS for #{user_id} (#{limit}/#{offset})")
        fetch_and_cache_home_posts_optimized(user_id, cache_key, current_time, limit, offset)
    end
  rescue
    ArgumentError ->
      ensure_cache_tables()
      get_user_and_following_posts_cached_optimized(user_id, limit, offset)
  end

  defp fetch_and_cache_home_posts_optimized(user_id, cache_key, current_time, limit, offset) do
    posts = get_user_and_following_posts_optimized(user_id, limit, offset)
    :ets.insert(:posts_cache, {cache_key, posts, current_time})
    posts
  end

  defp get_user_and_following_posts_optimized(user_id, limit \\ @posts_per_page, offset \\ 0) do
    following_user_ids = get_following_cached(user_id)
    all_user_ids = [user_id | following_user_ids]

    Logger.info("👥 Following user IDs: #{inspect(following_user_ids)} (#{length(following_user_ids)} users)")

    all_posts = all_user_ids
    |> Task.async_stream(fn uid ->
      Posts.get_posts_by_user_id(uid)
    end, max_concurrency: 8, timeout: 1500, on_timeout: :kill_task)
    |> Stream.filter(&match?({:ok, _}, &1))
    |> Stream.flat_map(&elem(&1, 1))
    |> Enum.sort_by(& &1.date_created, &>=/2)
    |> Enum.drop(offset)
    |> Enum.take(limit)

    Logger.info("📝 Loaded #{length(all_posts)} posts (limit: #{limit}, offset: #{offset})")
    all_posts
  end

  defp get_following_cached(user_id) do
    cache_key = {:following, user_id}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:user_cache, cache_key) do
      [{_, following_ids, timestamp}] ->
        if current_time - timestamp < @user_cache_ttl do
          following_ids
        else
          fetch_and_cache_following(user_id, cache_key, current_time)
        end
      _ ->
        fetch_and_cache_following(user_id, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      ensure_cache_tables()
      get_following_cached(user_id)
  end

  defp fetch_and_cache_following(user_id, cache_key, current_time) do
    following_ids = Users.get_following(user_id)
    :ets.insert(:user_cache, {cache_key, following_ids, current_time})
    following_ids
  end

  defp get_comments_cached(post_id) do
    cache_key = {:comments, post_id}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:comments_cache, cache_key) do
      [{_, comments, timestamp}] ->
        if current_time - timestamp < @comments_cache_ttl do
          Logger.info("📦 Comments Cache HIT for post #{post_id}")
          comments
        else
          Logger.info("📦 Comments Cache EXPIRED for post #{post_id}")
          fetch_and_cache_comments(post_id, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Comments Cache MISS for post #{post_id}")
        fetch_and_cache_comments(post_id, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      ensure_cache_tables()
      get_comments_cached(post_id)
  end

  defp fetch_and_cache_comments(post_id, cache_key, current_time) do
    comments = post_id |> to_charlist() |> Mazaryn.Posts.get_comment_by_post_id()
    :ets.insert(:comments_cache, {cache_key, comments, current_time})
    comments
  end

  defp search_user_by_username_cached(username) do
    cache_key = {:search, username}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:search_cache, cache_key) do
      [{_, results, timestamp}] ->
        if current_time - timestamp < @search_cache_ttl do
          Logger.info("📦 Search Cache HIT for #{username}")
          results
        else
          Logger.info("📦 Search Cache EXPIRED for #{username}")
          fetch_and_cache_search(username, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Search Cache MISS for #{username}")
        fetch_and_cache_search(username, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      ensure_cache_tables()
      search_user_by_username_cached(username)
  end

  defp fetch_and_cache_search(username, cache_key, current_time) do
    fetch_start = :erlang.system_time(:millisecond)

    result = case username |> Core.UserClient.search_user() do
      :username_not_exist ->
        nil
      erl_user ->
        [
          erl_user
          |> User.erl_changeset()
          |> User.build()
          |> elem(1)
        ]
    end

    :ets.insert(:search_cache, {cache_key, result, current_time})

    fetch_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 search_user_by_username completed in #{fetch_end - fetch_start}ms")
    result
  end

  defp get_cached_posts_fallback(user_id) do
    cache_key = {:home_posts, user_id, @initial_posts_limit, 0}
    case :ets.lookup(:posts_cache, cache_key) do
      [{_, posts, _}] -> posts
      _ -> []
    end
  rescue
    ArgumentError -> []
  end

  defp clear_posts_cache(user_id) do
    try do
      :ets.match_delete(:posts_cache, {{:home_posts, user_id, :_, :_}, :_, :_})
      :ets.match_delete(:user_cache, {{:following, user_id}, :_, :_})
    rescue
      ArgumentError -> :ok
    end
  end

  defp update_post_in_list(posts, post_id, update_fn) do
    post_id_str = to_string(post_id)

    Enum.map(posts, fn post ->
      if to_string(post.id) == post_id_str do
        update_fn.(post)
      else
        post
      end
    end)
  end

  defp ensure_cache_tables do
    try do
      :ets.new(:user_cache, [:set, :public, :named_table])
    rescue
      ArgumentError -> :ok
    end

    try do
      :ets.new(:posts_cache, [:set, :public, :named_table])
    rescue
      ArgumentError -> :ok
    end

    try do
      :ets.new(:comments_cache, [:set, :public, :named_table])
    rescue
      ArgumentError -> :ok
    end

    try do
      :ets.new(:search_cache, [:set, :public, :named_table])
    rescue
      ArgumentError -> :ok
    end
  end

  defp warm_cache_async(user_id) do
    Task.start(fn ->
      Logger.info("🔥 Warming cache for user #{user_id}")

      get_user_cached_optimized(user_id)

      get_following_cached(user_id)

      get_user_and_following_posts_cached_optimized(user_id, @initial_posts_limit)

      Logger.info("🔥 Cache warming completed for user #{user_id}")
    end)
  end

  defp track_performance(operation, start_time) do
    end_time = :erlang.system_time(:millisecond)
    duration = end_time - start_time

    if duration > 1000 do
      Logger.warn("⚠️ Slow operation #{operation}: #{duration}ms")
    else
      Logger.info("⚡ Operation #{operation}: #{duration}ms")
    end

    duration
  end

  defp cleanup_old_cache_entries do
    current_time = :erlang.system_time(:millisecond)

    :ets.select_delete(:user_cache, [
      {{:_, :_, :"$1"}, [{:<, :"$1", current_time - @user_cache_ttl * 2}], [true]}
    ])

    :ets.select_delete(:posts_cache, [
      {{:_, :_, :"$1"}, [{:<, :"$1", current_time - @posts_cache_ttl * 2}], [true]}
    ])

    :ets.select_delete(:comments_cache, [
      {{:_, :_, :"$1"}, [{:<, :"$1", current_time - @comments_cache_ttl * 2}], [true]}
    ])

    :ets.select_delete(:search_cache, [
      {{:_, :_, :"$1"}, [{:<, :"$1", current_time - @search_cache_ttl * 2}], [true]}
    ])
  end

  defp with_retry(operation, retries \\ 2) do
    try do
      operation.()
    rescue
      error ->
        if retries > 0 do
          Logger.warn("🔄 Retrying operation due to error: #{inspect(error)}")
          :timer.sleep(100)
          with_retry(operation, retries - 1)
        else
          Logger.error("❌ Operation failed after retries: #{inspect(error)}")
          {:error, error}
        end
    end
  end

  defp batch_load_posts(user_ids, limit) do
    user_ids
    |> Enum.chunk_every(5)
    |> Enum.flat_map(fn batch ->
      batch
      |> Task.async_stream(fn user_id ->
        Posts.get_posts_by_user_id(user_id)
      end, max_concurrency: 5, timeout: 2000, on_timeout: :kill_task)
      |> Enum.filter(&match?({:ok, _}, &1))
      |> Enum.flat_map(&elem(&1, 1))
    end)
    |> Enum.sort_by(& &1.date_created, &>=/2)
    |> Enum.take(limit)
  end

  defp get_user_sync_optimized(user_id) do
    sync_start = :erlang.system_time(:millisecond)
    Logger.info("👤 Starting optimized synchronous user load for #{user_id}")

    result = with_retry(fn -> Users.one_by_email(user_id) end)

    track_performance("user_sync_load", sync_start)
    result
  end

  defp maybe_precompute_feed(user_id) do
    Task.start(fn ->
      Logger.info("🌟 Precomputing feed for user #{user_id}")
      get_user_and_following_posts_optimized(user_id, @posts_per_page * 2)
    end)
  end

  defp report_metrics(metric_name, value, metadata \\ %{}) do
    Logger.info("📊 Metric #{metric_name}: #{value} #{inspect(metadata)}")
  end

  defp with_circuit_breaker(operation, service_name) do
    try do
      operation.()
    rescue
      error ->
        Logger.error("🚨 Circuit breaker triggered for #{service_name}: #{inspect(error)}")
        {:error, :service_unavailable}
    end
  end
end
