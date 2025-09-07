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
  @session_cache_ttl 90_000

  @user_load_timeout 2_000
  @posts_load_timeout 3_000
  @comments_load_timeout 1_500
  @search_timeout 1_000
  @task_timeout 5_000

  @posts_per_page 20
  @initial_posts_limit 10
  @max_concurrent_tasks 8

  @fresh_cache_threshold 10_000
  @stale_cache_threshold 30_000

  @spec init_cache_tables() :: :ok
  defp init_cache_tables do
    tables = [:user_cache, :posts_cache, :comments_cache, :search_cache, :session_cache, :following_cache, :rate_limit_cache]

    Enum.each(tables, fn table ->
      case :ets.whereis(table) do
        :undefined -> :ets.new(table, [:set, :public, :named_table])
        _ -> :ok
      end
    end)
  end

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

    init_cache_tables()
    Process.flag(:trap_exit, true)

    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "home_feed_updates")
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "user_updates")
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "post_updates")

    result = with {:ok, user} <- get_user_by_session_cached(session_uuid) do
      {cached_posts, cache_age} = get_cached_posts_with_age(user.id)

      socket = socket
      |> assign_initial_state()
      |> assign_base_data(session_uuid, user)
      |> assign_immediate_posts(cached_posts, cache_age)

      schedule_smart_refresh(user.id, cache_age)

      {:ok, socket}
    else
      {:error, reason} ->
        Logger.error("❌ Mount error: #{inspect(reason)}")

        user_id = get_user_id_from_session(session_uuid)
        {cached_posts, cache_age} = get_cached_posts_with_age(user_id)

        socket = socket
        |> assign_initial_state()
        |> assign(session_uuid: session_uuid)
        |> assign_immediate_posts(cached_posts, cache_age)

        schedule_smart_refresh(user_id, cache_age)
        {:ok, socket}
    end

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🏠 MOUNT MazarynWeb.HomeLive.Home completed in #{mount_end - mount_start}ms")
    result
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🏠 Starting MOUNT MazarynWeb.HomeLive.Home for user_id #{user_id}")

    init_cache_tables()
    Process.flag(:trap_exit, true)

    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "home_feed_updates")
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "user_updates")
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "post_updates")

    result = case get_user_cached_optimized(user_id) do
      {:ok, user} ->
        {cached_posts, cache_age} = get_cached_posts_with_age(user.id)

        socket = socket
        |> assign_initial_state()
        |> assign_base_data(user_id, user)
        |> assign_immediate_posts(cached_posts, cache_age)

        schedule_smart_refresh(user.id, cache_age)

        {:ok, socket}

      {:error, _reason} ->
        {cached_posts, cache_age} = get_cached_posts_with_age(user_id)

        socket = socket
        |> assign_initial_state()
        |> assign(user_id: user_id)
        |> assign_immediate_posts(cached_posts, cache_age)

        schedule_smart_refresh(user_id, cache_age)
        {:ok, socket}
    end

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

    socket = assign(socket, search: search, last_search: search)

    if socket.assigns[:search_timer] do
      Process.cancel_timer(socket.assigns.search_timer)
    end

    timer = Process.send_after(self(), {:search_debounce, search}, 300)
    socket = assign(socket, search_timer: timer)

    search_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 handle_event do_search completed in #{search_end - search_start}ms")
    {:noreply, socket}
  end

  def handle_event("select_emoji", %{"name" => name}, socket) do
    Logger.info("😀 Selected emoji: #{name}")
    {:noreply, socket}
  end

  @impl true
  def handle_info({:load_user_and_posts_async, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("👤📚 Starting async load_user_and_posts for user_id #{user_id}")

    user_task = Task.async(fn ->
      try do
        get_user_cached_optimized(user_id)
      rescue
        error ->
          Logger.error("Error in load_user task: #{inspect(error)}")
          {:error, :user_load_failed}
      end
    end)

    posts_task = Task.async(fn ->
      try do
        get_user_and_following_posts_cached_optimized(user_id, @initial_posts_limit)
      rescue
        error ->
          Logger.error("Error in load_posts task: #{inspect(error)}")
          []
      end
    end)

    user_result = case Task.yield(user_task, @user_load_timeout) do
      {:ok, {:ok, user}} ->
        Logger.info("👤 Successfully loaded user #{user.username}")
        user
      {:ok, {:error, reason}} ->
        Logger.error("❌ Failed to load user: #{inspect(reason)}")
        nil
      nil ->
        Logger.warning("Timeout loading user")
        Task.shutdown(user_task, :brutal_kill)
        nil
    end

    posts_result = case Task.yield(posts_task, @posts_load_timeout) do
      {:ok, posts} ->
        Logger.info("Successfully loaded #{length(posts)} posts for home feed")
        posts
      nil ->
        Logger.warning("Timeout loading posts, using cached fallback")
        Task.shutdown(posts_task, :brutal_kill)
        get_cached_posts_fallback(user_id)
    end

    socket = if user_result do
      post_changeset = Post.changeset(%Post{})
      socket = socket
      |> assign(post_changeset: post_changeset)
      |> assign(user: user_result)
      |> assign(last_post_loaded_at: :erlang.system_time(:millisecond))

      socket = if length(posts_result) > 0 and posts_result != socket.assigns.posts do
        assign(socket, posts: posts_result)
      else
        socket
      end

      socket = assign(socket, posts_loading: false)

      send(self(), {:preload_more_posts, user_result.id, @initial_posts_limit})
      socket
    else
      socket
      |> assign(posts: posts_result)
      |> assign(posts_loading: false)
    end

    load_end = :erlang.system_time(:millisecond)
    Logger.info("Async load_user_and_posts completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:background_refresh, user_id}, socket) do
    Logger.info("🔄 Starting background refresh for user #{user_id}")

    Task.start(fn ->
      try do
        fresh_posts = get_user_and_following_posts_optimized(user_id, @initial_posts_limit)
        if length(fresh_posts) > 0 do
          send(self(), {:background_posts_ready, fresh_posts})
        end
      rescue
        error ->
          Logger.error("Error in background refresh: #{inspect(error)}")
      end
    end)

    {:noreply, socket}
  end

  def handle_info({:background_posts_ready, fresh_posts}, socket) do
    Logger.info("✅ Background refresh completed with #{length(fresh_posts)} posts")

    socket = if fresh_posts != socket.assigns.posts do
      assign(socket, posts: fresh_posts, last_post_loaded_at: :erlang.system_time(:millisecond))
    else
      socket
    end

    {:noreply, socket}
  end

  def handle_info({:load_posts_async_priority, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("Starting PRIORITY async load_posts_async for user_id #{user_id}")

    posts_task = Task.async(fn ->
      try do
        get_user_and_following_posts_optimized(user_id, @initial_posts_limit)
      rescue
        error ->
          Logger.error("Error in priority load_posts task: #{inspect(error)}")
          get_cached_posts_fallback(user_id)
      end
    end)

    posts_result = case Task.yield(posts_task, @posts_load_timeout) do
      {:ok, posts} ->
        Logger.info("Successfully loaded #{length(posts)} priority posts for home feed")
        posts
      nil ->
        Logger.warning("Timeout loading priority posts, using cached or empty list")
        Task.shutdown(posts_task, :brutal_kill)
        get_cached_posts_fallback(user_id)
    end

    socket = socket
    |> assign(posts: posts_result)
    |> assign(posts_loading: false)
    |> assign(last_post_loaded_at: :erlang.system_time(:millisecond))

    send(self(), {:preload_more_posts, user_id, @initial_posts_limit})

    load_end = :erlang.system_time(:millisecond)
    Logger.info("Priority async load_posts_async completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:load_posts_async, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("Starting async load_posts_async for user_id #{user_id}")

    posts_task = Task.async(fn ->
      try do
        get_user_and_following_posts_cached_optimized(user_id, @initial_posts_limit)
      rescue
        error ->
          Logger.error("Error in load_posts task: #{inspect(error)}")
          get_cached_posts_fallback(user_id)
      end
    end)

    posts_result = case Task.yield(posts_task, @posts_load_timeout) do
      {:ok, posts} ->
        Logger.info("Successfully loaded #{length(posts)} posts for home feed")
        posts
      nil ->
        Logger.warning("Timeout loading posts, using cached or empty list")
        Task.shutdown(posts_task, :brutal_kill)
        get_cached_posts_fallback(user_id)
    end

    socket = socket
    |> assign(posts: posts_result)
    |> assign(posts_loading: false)
    |> assign(last_post_loaded_at: :erlang.system_time(:millisecond))

    load_end = :erlang.system_time(:millisecond)
    Logger.info("Async load_posts_async completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:load_more_posts_async, user_id, offset}, socket) do
    Logger.info("Loading more posts from offset #{offset}")

    posts_task = Task.async(fn ->
      try do
        get_user_and_following_posts_optimized(user_id, @posts_per_page, offset)
      rescue
        error ->
          Logger.error("Error in load_more_posts task: #{inspect(error)}")
          []
      end
    end)

    new_posts = case Task.yield(posts_task, @posts_load_timeout) do
      {:ok, posts} ->
        Logger.info("Successfully loaded #{length(posts)} more posts")
        posts
      nil ->
        Logger.warning("Timeout loading more posts")
        Task.shutdown(posts_task, :brutal_kill)
        []
    end

    current_posts = socket.assigns.posts
    all_posts = current_posts ++ new_posts

    {:noreply, assign(socket, posts: all_posts, posts_loading_more: false)}
  end

  def handle_info({:preload_more_posts, user_id, offset}, socket) do
    Logger.info("Preloading next batch of posts")

    Task.start(fn ->
      try do
        get_user_and_following_posts_optimized(user_id, @posts_per_page, offset)
      rescue
        error ->
          Logger.error("Error in preload_more_posts task: #{inspect(error)}")
      end
    end)

    {:noreply, socket}
  end

  def handle_info({:load_comments_cached, post_id}, socket) do
    comment_start = :erlang.system_time(:millisecond)
    Logger.info("Starting CACHED async load_comments for post #{post_id}")

    comments_task = Task.async(fn ->
      try do
        get_comments_cached(post_id)
      rescue
        error ->
          Logger.error("Error in load_comments task: #{inspect(error)}")
          []
      end
    end)

    comments = case Task.yield(comments_task, @comments_load_timeout) do
      {:ok, comments} ->
        Logger.info("Successfully loaded #{length(comments)} comments for post #{post_id}")
        comments
      nil ->
        Logger.warning("Timeout loading comments for post #{post_id}")
        Task.shutdown(comments_task, :brutal_kill)
        []
    end

    comment_end = :erlang.system_time(:millisecond)
    Logger.info("CACHED async load_comments completed in #{comment_end - comment_start}ms")

    {:noreply, assign(socket, comments: comments, comments_loading: false)}
  end

  def handle_info({:search_debounce, search_term}, socket) do
    case socket.assigns[:last_search] do
      ^search_term ->
        search_start = :erlang.system_time(:millisecond)
        Logger.info("Executing debounced search for #{search_term}")

        search_task = Task.async(fn ->
          try do
            search_user_by_username_cached(search_term)
          rescue
            error ->
              Logger.error("Error in search task: #{inspect(error)}")
              []
          end
        end)

        results = case Task.yield(search_task, @search_timeout) do
          {:ok, users} ->
            Logger.info("Successfully found #{length(users || [])} users for query #{search_term}")
            users || []
          nil ->
            Logger.warning("Search timeout")
            Task.shutdown(search_task, :brutal_kill)
            []
        end

        search_end = :erlang.system_time(:millisecond)
        Logger.info("Debounced search completed in #{search_end - search_start}ms")

        {:noreply, assign(socket, results: results, search_loading: false)}

      _ ->
        {:noreply, socket}
    end
  end

  def handle_info(:reload_posts, socket) do
    reload_start = :erlang.system_time(:millisecond)
    Logger.info("Starting handle_info reload_posts")

    user = socket.assigns.user
    socket = if user do
      clear_posts_cache(user.id)
      send(self(), {:load_posts_async_priority, user.id})
      assign(socket, posts_loading: true)
    else
      socket
    end

    reload_end = :erlang.system_time(:millisecond)
    Logger.info("handle_info reload_posts completed in #{reload_end - reload_start}ms")
    {:noreply, socket}
  end

  def handle_info({:post_updated, post}, socket) do
    Logger.info("Real-time post update received: #{post.id}")

    updated_posts = update_post_in_list(socket.assigns.posts, post.id, fn _ -> post end)
    {:noreply, assign(socket, posts: updated_posts)}
  end

  def handle_info({:post_deleted, post_id}, socket) do
    Logger.info("Real-time post deletion received: #{post_id}")

    updated_posts = Enum.reject(socket.assigns.posts, &(&1.id == post_id))
    {:noreply, assign(socket, posts: updated_posts)}
  end

   def handle_info({:new_post, post}, socket) do
    Logger.info("Real-time new post received: #{post.id}")

    current_posts = socket.assigns.posts
    updated_posts = [post | current_posts] |> Enum.take(50)
    {:noreply, assign(socket, posts: updated_posts)}
  end

  def handle_info({:EXIT, pid, reason}, socket) do
    case reason do
      :normal ->
        Logger.debug("Task #{inspect(pid)} completed normally")
      :killed ->
        Logger.info("Task #{inspect(pid)} was killed")
      other ->
        Logger.warning("Task #{inspect(pid)} exited with reason: #{inspect(other)}")
    end

    {:noreply, socket}
  end

  @impl true
  def handle_info({:ipns_result, _post_id, _ipns_id}, socket) do
    {:noreply, socket}
  end

  def handle_info({:comment_deleted_success, comment_id}, socket) do
    Logger.info("Comment #{comment_id} deletion confirmed at Home level")
    {:noreply, socket}
  end

  def handle_info({:comment_deletion_failed, comment_id, _comment_to_delete}, socket) do
    Logger.info("Comment #{comment_id} deletion failed at Home level")
    {:noreply, socket |> put_flash(:error, "Failed to delete comment")}
  end

  def handle_info({:reply_deleted_success, reply_id, _comment_id}, socket) do
    Logger.info("Reply #{reply_id} deletion confirmed at Home level")
    {:noreply, socket}
  end

  def handle_info({:reply_deletion_failed, reply_id, _comment_id, _reply_to_delete}, socket) do
    Logger.info("Reply #{reply_id} deletion failed at Home level")
    {:noreply, socket |> put_flash(:error, "Failed to delete reply")}
  end

  def handle_info({:comment_content_updated, comment_id, _content}, socket) do
    Logger.info("Comment content updated: #{comment_id}")
    {:noreply, socket}
  end

  def handle_info({:reply_content_updated, reply_id, _content}, socket) do
    Logger.info("Reply content updated: #{reply_id}")
    {:noreply, socket}
  end

  def handle_info(msg, socket) do
    Logger.debug("Received unexpected message: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp get_cached_posts_with_age(user_id) do
    return_empty = {[], :no_cache}

    if user_id do
      cache_key = {:home_posts, user_id, @initial_posts_limit, 0}
      current_time = :erlang.system_time(:millisecond)

      case safe_ets_lookup(:posts_cache, cache_key) do
        [{_, posts, timestamp}] ->
          age = current_time - timestamp
          cache_status = cond do
            age < @fresh_cache_threshold -> :fresh
            age < @stale_cache_threshold -> :stale
            true -> :expired
          end
          {posts, cache_status}
        [] ->
          return_empty
      end
    else
      return_empty
    end
  end

  defp schedule_smart_refresh(user_id, cache_age) do
    if user_id do
      case cache_age do
        :fresh ->
          Process.send_after(self(), {:background_refresh, user_id}, 5000)
        :stale ->
          Process.send_after(self(), {:background_refresh, user_id}, 1000)
        :expired ->
          Process.send_after(self(), {:load_user_and_posts_async, user_id}, 10)
        :no_cache ->
          Process.send_after(self(), {:load_user_and_posts_async, user_id}, 10)
      end
    end
  end

  defp assign_immediate_posts(socket, cached_posts, cache_age) do
    posts_loading = case cache_age do
      :no_cache -> true
      :expired -> true
      _ -> false
    end

    socket
    |> assign(posts: cached_posts)
    |> assign(posts_loading: posts_loading)
  end

  defp get_user_by_session_cached(session_uuid) do
    cache_key = {:session, session_uuid}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:session_cache, cache_key) do
      [{_, user, timestamp}] when current_time - timestamp < @session_cache_ttl ->
        Logger.info("Session Cache HIT for #{session_uuid}")
        {:ok, user}
      _ ->
        Logger.info("Session Cache MISS/EXPIRED for #{session_uuid}")
        fetch_and_cache_session_user(session_uuid, cache_key, current_time)
    end
  end

  defp get_user_cached_optimized(user_id) do
    cache_key = {:home_user, user_id}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:user_cache, cache_key) do
      [{_, user, timestamp}] when current_time - timestamp < @user_cache_ttl ->
        Logger.info("Home User Cache HIT for #{user_id}")
        {:ok, user}
      _ ->
        Logger.info("Home User Cache MISS/EXPIRED for #{user_id}")
        fetch_and_cache_home_user_optimized(user_id, cache_key, current_time)
    end
  end

  defp get_user_and_following_posts_cached_optimized(user_id, limit \\ @posts_per_page, offset \\ 0) do
    cache_key = {:home_posts, user_id, limit, offset}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:posts_cache, cache_key) do
      [{_, posts, timestamp}] when current_time - timestamp < @posts_cache_ttl ->
        Logger.info("Home Posts Cache HIT for #{user_id} (#{limit}/#{offset})")
        posts
      _ ->
        Logger.info("Home Posts Cache MISS/EXPIRED for #{user_id} (#{limit}/#{offset})")
        fetch_and_cache_home_posts_optimized(user_id, cache_key, current_time, limit, offset)
    end
  end

  defp get_following_cached(user_id) do
    cache_key = {:following, user_id}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:following_cache, cache_key) do
      [{_, following_ids, timestamp}] when current_time - timestamp < @user_cache_ttl ->
        Logger.info("Following Cache HIT for #{user_id}")
        following_ids
      _ ->
        Logger.info("Following Cache MISS/EXPIRED for #{user_id}")
        fetch_and_cache_following(user_id, cache_key, current_time)
    end
  end

  defp get_comments_cached(post_id) do
    cache_key = {:comments, post_id}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:comments_cache, cache_key) do
      [{_, comments, timestamp}] when current_time - timestamp < @comments_cache_ttl ->
        Logger.info("Comments Cache HIT for post #{post_id}")
        comments
      _ ->
        Logger.info("Comments Cache MISS/EXPIRED for post #{post_id}")
        fetch_and_cache_comments(post_id, cache_key, current_time)
    end
  end

  defp search_user_by_username_cached(username) do
    cache_key = {:search, username}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:search_cache, cache_key) do
      [{_, results, timestamp}] when current_time - timestamp < @search_cache_ttl ->
        Logger.info("Search Cache HIT for #{username}")
        results
      _ ->
        Logger.info("Search Cache MISS/EXPIRED for #{username}")
        fetch_and_cache_search(username, cache_key, current_time)
    end
  end

  defp safe_ets_lookup(table, key) do
    try do
      :ets.lookup(table, key)
    rescue
      ArgumentError ->
        init_cache_tables()
        []
    end
  end

  defp safe_ets_insert(table, data) do
    try do
      :ets.insert(table, data)
    rescue
      ArgumentError ->
        init_cache_tables()
        :ets.insert(table, data)
    end
  end

  defp fetch_and_cache_session_user(session_uuid, cache_key, current_time) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} = result ->
        safe_ets_insert(:session_cache, {cache_key, user, current_time})
        result
      {:error, _} = error -> error
      other -> {:error, other}
    end
  end

  defp fetch_and_cache_home_user_optimized(user_id, cache_key, current_time) do
    case Users.one_by_email(user_id) do
      {:ok, user} = result ->
        safe_ets_insert(:user_cache, {cache_key, user, current_time})
        result
      other -> other
    end
  end

  defp fetch_and_cache_home_posts_optimized(user_id, cache_key, current_time, limit, offset) do
    posts = get_user_and_following_posts_optimized(user_id, limit, offset)
    safe_ets_insert(:posts_cache, {cache_key, posts, current_time})
    posts
  end

  defp fetch_and_cache_following(user_id, cache_key, current_time) do
    try do
      following_ids = Users.get_following(user_id)
      safe_ets_insert(:following_cache, {cache_key, following_ids, current_time})
      following_ids
    rescue
      error ->
        Logger.error("Error fetching following for #{user_id}: #{inspect(error)}")
        []
    end
  end

  defp fetch_and_cache_comments(post_id, cache_key, current_time) do
    try do
      comments = post_id |> to_charlist() |> Mazaryn.Posts.get_comment_by_post_id()
      safe_ets_insert(:comments_cache, {cache_key, comments, current_time})
      comments
    rescue
      error ->
        Logger.error("Error fetching comments for post #{post_id}: #{inspect(error)}")
        []
    end
  end

  defp fetch_and_cache_search(username, cache_key, current_time) do
    try do
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

      safe_ets_insert(:search_cache, {cache_key, result, current_time})

      fetch_end = :erlang.system_time(:millisecond)
      Logger.info("search_user_by_username completed in #{fetch_end - fetch_start}ms")
      result
    rescue
      error ->
        Logger.error("Error searching for user #{username}: #{inspect(error)}")
        nil
    end
  end

  defp get_user_and_following_posts_optimized(user_id, limit \\ @posts_per_page, offset \\ 0) do
    try do
      following_user_ids = get_following_cached(user_id)
      all_user_ids = [user_id | following_user_ids]

      Logger.info("Following user IDs: #{inspect(following_user_ids)} (#{length(following_user_ids)} users)")

      all_posts = all_user_ids
      |> Task.async_stream(fn uid ->
        Posts.get_posts_by_user_id(uid)
      end, max_concurrency: @max_concurrent_tasks, timeout: 1500, on_timeout: :kill_task)
      |> Stream.filter(&match?({:ok, _}, &1))
      |> Stream.flat_map(&elem(&1, 1))
      |> Enum.sort_by(& &1.date_created, &>=/2)
      |> Enum.drop(offset)
      |> Enum.take(limit)

      Logger.info("Loaded #{length(all_posts)} posts (limit: #{limit}, offset: #{offset})")
      all_posts
    rescue
      error ->
        Logger.error("Error in get_user_and_following_posts_optimized: #{inspect(error)}")
        get_cached_posts_fallback(user_id)
    end
  end

  defp clear_posts_cache(user_id) do
    try do
      :ets.match_delete(:posts_cache, {{:home_posts, user_id, :_, :_}, :_, :_})
      :ets.match_delete(:following_cache, {{:following, user_id}, :_, :_})
    rescue
      ArgumentError -> :ok
    end
  end

  defp clear_user_cache(user_id) do
    try do
      :ets.match_delete(:user_cache, {{:home_user, user_id}, :_, :_})
    rescue
      ArgumentError -> :ok
    end
  end

  defp clear_comments_cache(post_id) do
    try do
      :ets.match_delete(:comments_cache, {{:comments, post_id}, :_, :_})
    rescue
      ArgumentError -> :ok
    end
  end

  defp get_cached_posts_fallback(user_id) do
    cache_key = {:home_posts, user_id, @initial_posts_limit, 0}
    case safe_ets_lookup(:posts_cache, cache_key) do
      [{_, posts, _}] -> posts
      _ -> []
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

  defp assign_initial_state(socket) do
    socket
    |> assign(results: [])
    |> assign(posts: [])
    |> assign(posts_loading: true)
    |> assign(posts_loading_more: false)
    |> assign(comments_loading: false)
    |> assign(search_loading: false)
    |> assign(last_post_loaded_at: nil)
    |> assign(search: "")
    |> assign(comments: [])
  end

  defp assign_base_data(socket, session_identifier, user) do
    post_changeset = Post.changeset(%Post{})

    socket
    |> assign(session_uuid: session_identifier)
    |> assign(post_changeset: post_changeset)
    |> assign(user: user)
    |> assign(results: [])
  end

  defp get_user_id_from_session(session_uuid) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} -> user.id
      _ -> nil
    end
  end

  defp track_performance(operation, start_time) do
    end_time = :erlang.system_time(:millisecond)
    duration = end_time - start_time

    case duration do
      d when d > 1000 -> Logger.warning("Slow operation #{operation}: #{d}ms")
      d when d > 500 -> Logger.info("Operation #{operation}: #{d}ms")
      _ -> Logger.debug("Operation #{operation} completed quickly")
    end

    duration
  end

  defp cleanup_old_cache_entries do
    current_time = :erlang.system_time(:millisecond)

    try do
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

      :ets.select_delete(:following_cache, [
        {{:_, :_, :"$1"}, [{:<, :"$1", current_time - @user_cache_ttl * 2}], [true]}
      ])

      :ets.select_delete(:session_cache, [
        {{:_, :_, :"$1"}, [{:<, :"$1", current_time - @session_cache_ttl * 2}], [true]}
      ])
    rescue
      ArgumentError -> :ok
    end
  end

  defp with_retry(operation, retries \\ 2) do
    try do
      operation.()
    rescue
      error ->
        if retries > 0 do
          Logger.warning("Retrying operation due to error: #{inspect(error)}")
          :timer.sleep(100)
          with_retry(operation, retries - 1)
        else
          Logger.error("Operation failed after retries: #{inspect(error)}")
          {:error, error}
        end
    end
  end

  defp batch_load_posts(user_ids, limit) do
    try do
      user_ids
      |> Enum.chunk_every(5)
      |> Task.async_stream(fn batch ->
        batch
        |> Task.async_stream(fn user_id ->
          Posts.get_posts_by_user_id(user_id)
        end, max_concurrency: 5, timeout: 2000, on_timeout: :kill_task)
        |> Enum.filter(&match?({:ok, _}, &1))
        |> Enum.flat_map(&elem(&1, 1))
      end, max_concurrency: 3, timeout: 3000, on_timeout: :kill_task)
      |> Enum.filter(&match?({:ok, _}, &1))
      |> Enum.flat_map(&elem(&1, 1))
      |> Enum.sort_by(& &1.date_created, &>=/2)
      |> Enum.take(limit)
    rescue
      error ->
        Logger.error("Error in batch_load_posts: #{inspect(error)}")
        []
    end
  end

  defp maybe_precompute_feed(user_id) do
    Task.start(fn ->
      try do
        Logger.info("Precomputing feed for user #{user_id}")
        get_user_and_following_posts_optimized(user_id, @posts_per_page * 2)
      rescue
        error ->
          Logger.error("Error in precompute_feed: #{inspect(error)}")
      end
    end)
  end

  defp with_circuit_breaker(operation, service_name) do
    try do
      operation.()
    rescue
      error ->
        Logger.error("Circuit breaker triggered for #{service_name}: #{inspect(error)}")
        {:error, :service_unavailable}
    end
  end

  defp warm_cache_async(user_id) do
    Task.start(fn ->
      try do
        Logger.info("Warming cache for user #{user_id}")

        get_user_cached_optimized(user_id)
        get_following_cached(user_id)
        get_user_and_following_posts_cached_optimized(user_id, @initial_posts_limit)

        Logger.info("Cache warming completed for user #{user_id}")
      rescue
        error ->
          Logger.error("Error in warm_cache_async: #{inspect(error)}")
      end
    end)
  end

  defp check_rate_limit(user_id, action) do
    cache_key = {:rate_limit, user_id, action}
    current_time = :erlang.system_time(:second)

    case safe_ets_lookup(:rate_limit_cache, cache_key) do
      [{_, count, timestamp}] ->
        if current_time - timestamp < 60 do
          if count >= 100 do
            {:error, :rate_limited}
          else
            safe_ets_insert(:rate_limit_cache, {cache_key, count + 1, timestamp})
            :ok
          end
        else
          safe_ets_insert(:rate_limit_cache, {cache_key, 1, current_time})
          :ok
        end
      [] ->
        safe_ets_insert(:rate_limit_cache, {cache_key, 1, current_time})
        :ok
    end
  end

  defp log_performance_metrics(operation, start_time) do
    end_time = :erlang.system_time(:millisecond)
    duration = end_time - start_time

    case duration do
      d when d > 1000 -> Logger.warning("Slow operation: #{operation} took #{d}ms")
      d when d > 500 -> Logger.info("Operation: #{operation} took #{d}ms")
      _ -> Logger.debug("Operation: #{operation} completed quickly")
    end
  end

end
