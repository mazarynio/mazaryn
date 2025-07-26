defmodule MazarynWeb.HomeLive.Home do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Mazaryn.Schema.Post
  alias Mazaryn.Posts
  alias Account.Users
  alias Account.User
  require Logger

  @user_cache_ttl 30_000
  @posts_cache_ttl 15_000

  @impl true
  def handle_info({:ipns_ready, post_id, ipns_id}, socket) do
    ipns_start = :erlang.system_time(:millisecond)
    Logger.info("🌐 IPNS ready for post #{post_id}, IPNS ID: #{ipns_id}")

    updated_posts = socket.assigns.posts
    |> Enum.map(fn post ->
      if to_string(post.id) == to_string(post_id) do
        Map.put(post, :ipns_id, ipns_id)
      else
        post
      end
    end)

    ipns_end = :erlang.system_time(:millisecond)
    Logger.info("🌐 IPNS ready handling completed in #{ipns_end - ipns_start}ms")

    {:noreply, assign(socket, posts: updated_posts)}
  end


  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🏠 Starting MOUNT MazarynWeb.HomeLive.Home for session_uuid #{session_uuid}")

    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "home_feed_updates")

    socket = socket
    |> assign(results: [])
    |> assign(posts: [])
    |> assign(posts_loading: true)

    user_id = get_user_id(session_uuid)
    result = {:ok, do_mount_async(user_id, socket)}

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🏠 MOUNT MazarynWeb.HomeLive.Home completed in #{mount_end - mount_start}ms")
    result
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🏠 Starting MOUNT MazarynWeb.HomeLive.Home for user_id #{user_id}")

    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "home_feed_updates")

    socket = socket
    |> assign(results: [])
    |> assign(posts: [])
    |> assign(posts_loading: true)

    result = {:ok, do_mount_async(user_id, socket)}

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🏠 MOUNT MazarynWeb.HomeLive.Home completed in #{mount_end - mount_start}ms")
    result
  end

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🏠 Starting MOUNT MazarynWeb.HomeLive.Home for session_uuid #{session_uuid}")

    socket = socket
    |> assign(results: [])
    |> assign(posts: [])
    |> assign(posts_loading: true)

    user_id = get_user_id(session_uuid)
    result = {:ok, do_mount_async(user_id, socket)}

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

    send(self(), {:load_comments, post_id})

    event_end = :erlang.system_time(:millisecond)
    Logger.info("💬 handle_event show-comments completed in #{event_end - event_start}ms")
    {:noreply, socket |> assign(:comments, [])}
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

    send(self(), {:search_users, search})

    search_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 handle_event do_search completed in #{search_end - search_start}ms")
    {:noreply, assign(socket, search: search, results: [])}
  end

  def handle_event("select_emoji", %{"name" => name}, socket) do
    Logger.info("😀 Selected emoji: #{name}")
    {:noreply, socket}
  end

  @impl true
  def handle_info({:load_posts_async, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("📚 Starting async load_posts_async for user_id #{user_id}")

    posts_task = Task.async(fn ->
      get_user_and_following_posts_cached(user_id)
    end)

    posts_result = case Task.yield(posts_task, 5000) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, posts} ->
        Logger.info("📚 Successfully loaded #{length(posts)} posts for home feed")
        posts
      nil ->
        Logger.warn("⏰ Timeout loading posts, using empty list")
        []
    end

    socket = socket
    |> assign(posts: posts_result)
    |> assign(posts_loading: false)

    load_end = :erlang.system_time(:millisecond)
    Logger.info("📚 Async load_posts_async completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:load_user_and_posts, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("👤 Starting async load_user_and_posts for user_id #{user_id}")

    user_task = Task.async(fn ->
      get_user_cached(user_id)
    end)

    posts_task = Task.async(fn ->
      get_user_and_following_posts_cached(user_id)
    end)

    user_result = case Task.yield(user_task, 3000) || Task.shutdown(user_task, :brutal_kill) do
      {:ok, {:ok, user}} ->
        Logger.info("👤 Successfully loaded user #{user.username}")
        user
      {:ok, {:error, reason}} ->
        Logger.error("❌ Failed to load user: #{inspect(reason)}")
        nil
      nil ->
        Logger.warn("⏰ Timeout loading user, using nil")
        nil
    end

    posts_result = case Task.yield(posts_task, 5000) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, posts} ->
        Logger.info("📚 Successfully loaded #{length(posts)} posts for home feed")
        posts
      nil ->
        Logger.warn("⏰ Timeout loading posts, using empty list")
        []
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
    else
      socket |> assign(posts: [], posts_loading: false)
    end

    load_end = :erlang.system_time(:millisecond)
    Logger.info("👤 Async load_user_and_posts completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:load_comments, post_id}, socket) do
    comment_start = :erlang.system_time(:millisecond)
    Logger.info("💬 Starting async load_comments for post #{post_id}")

    comments_task = Task.async(fn ->
      post_id |> to_charlist() |> Mazaryn.Posts.get_comment_by_post_id()
    end)

    comments = case Task.yield(comments_task, 3000) || Task.shutdown(comments_task, :brutal_kill) do
      {:ok, comments} ->
        Logger.info("💬 Successfully loaded #{length(comments)} comments for post #{post_id}")
        comments
      nil ->
        Logger.warn("⏰ Timeout loading comments for post #{post_id}, using empty list")
        []
    end

    comment_end = :erlang.system_time(:millisecond)
    Logger.info("💬 Async load_comments completed in #{comment_end - comment_start}ms")

    {:noreply, assign(socket, comments: comments)}
  end

  def handle_info({:search_users, search}, socket) do
    search_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting async search_users for query #{search}")

    search_task = Task.async(fn ->
      search_user_by_username(search)
    end)

    results = case Task.yield(search_task, 2000) || Task.shutdown(search_task, :brutal_kill) do
      {:ok, users} ->
        Logger.info("🔍 Successfully found #{length(users || [])} users for query #{search}")
        users || []
      nil ->
        Logger.warn("⏰ Timeout searching users for query #{search}, using empty list")
        []
    end

    search_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 Async search_users completed in #{search_end - search_start}ms")

    {:noreply, assign(socket, results: results)}
  end

  def handle_info(:reload_posts, socket) do
    reload_start = :erlang.system_time(:millisecond)
    Logger.info("🔄 Starting handle_info reload_posts")

    user = socket.assigns.user
    if user do
      clear_posts_cache(user.id)
      send(self(), {:load_posts_async, user.id})
      socket = assign(socket, posts_loading: true)
    else
      socket
    end

    reload_end = :erlang.system_time(:millisecond)
    Logger.info("🔄 handle_info reload_posts completed in #{reload_end - reload_start}ms")
    {:noreply, socket}
  end

  @impl true
  def handle_info({:ipns_result, _post_id, _ipns_id}, socket) do
    {:noreply, socket}
  end

  def handle_info({:comment_deleted_success, comment_id}, socket) do
    Logger.info("✅ Comment #{comment_id} deletion confirmed at Home level")
    {:noreply, socket}
  end

  def handle_info({:comment_deletion_failed, comment_id, comment_to_delete}, socket) do
    Logger.info("❌ Comment #{comment_id} deletion failed at Home level")
    {:noreply, socket |> put_flash(:error, "Failed to delete comment")}
  end

  def handle_info({:reply_deleted_success, reply_id, comment_id}, socket) do
    Logger.info("✅ Reply #{reply_id} deletion confirmed at Home level")
    {:noreply, socket}
  end

  def handle_info({:reply_deletion_failed, reply_id, comment_id, reply_to_delete}, socket) do
    Logger.info("❌ Reply #{reply_id} deletion failed at Home level")
    {:noreply, socket |> put_flash(:error, "Failed to delete reply")}
  end

  def handle_info({:comment_content_updated, comment_id, content}, socket) do
    Logger.info("🔄 Comment content updated: #{comment_id}")
    {:noreply, socket}
  end

  def handle_info({:reply_content_updated, reply_id, content}, socket) do
    Logger.info("🔄 Reply content updated: #{reply_id}")
    {:noreply, socket}
  end

  def handle_info(msg, socket) do
    Logger.info("⚠️ Unhandled message in Home LiveView: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp do_mount_async(user_id, socket) do
    post_changeset = Post.changeset(%Post{})

    case get_user_sync(user_id) do
      {:ok, user} ->
        socket = socket
        |> assign(post_changeset: post_changeset)
        |> assign(search: "")
        |> assign(results: [])
        |> assign(user: user)
        |> assign(posts: [])
        |> assign(posts_loading: true)

        send(self(), {:load_posts_async, user.id})
        socket

      {:error, reason} ->
        Logger.error("❌ Failed to load user during mount: #{inspect(reason)}")
        socket
        |> assign(post_changeset: post_changeset)
        |> assign(search: "")
        |> assign(results: [])
        |> assign(user: nil)
        |> assign(posts: [])
        |> assign(posts_loading: false)
    end
  end

  defp get_user_cached(user_id) do
    cache_key = {:home_user, user_id}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:user_cache, cache_key) do
      [{_, user, timestamp}] ->
        if current_time - timestamp < @user_cache_ttl do
          Logger.info("📦 Home User Cache HIT for #{user_id}")
          {:ok, user}
        else
          Logger.info("📦 Home User Cache EXPIRED for #{user_id}")
          fetch_and_cache_home_user(user_id, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Home User Cache MISS for #{user_id}")
        fetch_and_cache_home_user(user_id, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      :ets.new(:user_cache, [:set, :public, :named_table])
      get_user_cached(user_id)
  end

  defp fetch_and_cache_home_user(user_id, cache_key, current_time) do
    case Users.one_by_email(user_id) do
      {:ok, user} = result ->
        :ets.insert(:user_cache, {cache_key, user, current_time})
        result
      other -> other
    end
  end

  defp get_user_and_following_posts_cached(user_id) do
    cache_key = {:home_posts, user_id}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:posts_cache, cache_key) do
      [{_, posts, timestamp}] ->
        if current_time - timestamp < @posts_cache_ttl do
          Logger.info("📦 Home Posts Cache HIT for #{user_id}")
          posts
        else
          Logger.info("📦 Home Posts Cache EXPIRED for #{user_id}")
          fetch_and_cache_home_posts(user_id, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Home Posts Cache MISS for #{user_id}")
        fetch_and_cache_home_posts(user_id, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      :ets.new(:posts_cache, [:set, :public, :named_table])
      get_user_and_following_posts_cached(user_id)
  end

  defp fetch_and_cache_home_posts(user_id, cache_key, current_time) do
    posts = get_user_and_following_posts(user_id)
    :ets.insert(:posts_cache, {cache_key, posts, current_time})
    posts
  end

  defp get_user_and_following_posts(user_id) do
    following_user_ids = Users.get_following(user_id)
    Logger.info("👥 Following user IDs: #{inspect(following_user_ids)}")

    user_posts = Posts.get_posts_by_user_id(user_id)
    Logger.info("📝 Current user posts: #{length(user_posts)}")

    following_posts = following_user_ids
    |> Task.async_stream(fn following_user_id ->
      Posts.get_posts_by_user_id(following_user_id)
    end, max_concurrency: 10, timeout: 2000)
    |> Enum.filter(&match?({:ok, _}, &1))
    |> Enum.flat_map(&elem(&1, 1))

    (user_posts ++ following_posts)
    |> Enum.sort_by(& &1.date_created, &>=/2)
  end

  defp get_user_sync(user_id) do
    sync_start = :erlang.system_time(:millisecond)
    Logger.info("👤 Starting synchronous user load for #{user_id}")

    result = Users.one_by_email(user_id)

    sync_end = :erlang.system_time(:millisecond)
    Logger.info("👤 Synchronous user load completed in #{sync_end - sync_start}ms")
    result
  end

  defp clear_posts_cache(user_id) do
    try do
      :ets.match_delete(:posts_cache, {{:home_posts, user_id}, :_, :_})
    rescue
      ArgumentError -> :ok
    end
  end

  defp search_user_by_username(username) do
    fetch_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting search_user_by_username for #{username}")

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

    fetch_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 search_user_by_username completed in #{fetch_end - fetch_start}ms")
    result
  end
end
