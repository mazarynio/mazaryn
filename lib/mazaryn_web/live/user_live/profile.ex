defmodule MazarynWeb.UserLive.Profile do
  use MazarynWeb, :live_view

  require Logger

  import MazarynWeb.Live.Helper
  import Phoenix.HTML.Form
  import Phoenix.Component
  alias Core.UserClient
  alias Account.User
  alias Account.Users
  alias Mazaryn.Schema.Post
  alias Core.PostClient
  alias Mazaryn.Posts
  alias Phoenix.LiveView.JS
  alias MazarynWeb.Router.Helpers, as: Routes

  @user_cache_ttl 60_000
  @follow_cache_ttl 30_000
  @posts_cache_ttl 45_000
  @search_cache_ttl 120_000

  @max_posts_per_page 20
  @max_concurrent_tasks 15
  @task_timeout 3_000
  @search_debounce_ms 300


def handle_info({:load_posts, username, page}, socket) do
  load_start = :erlang.system_time(:millisecond)
  Logger.info("📚 Starting async load_posts for #{username}, page #{page}")

  posts = try do
    Posts.get_posts_by_author(username)
  rescue
    error ->
      Logger.error("Error loading posts for #{username}: #{inspect(error)}")
      []
  end

  posts_to_show = if page == 1 do
    Enum.take(posts, @max_posts_per_page)
  else
    current_posts = socket.assigns.posts || []
    offset = (page - 1) * @max_posts_per_page
    new_posts = posts |> Enum.drop(offset) |> Enum.take(@max_posts_per_page)
    current_posts ++ new_posts
  end

  has_more = length(posts) > length(posts_to_show)

  load_end = :erlang.system_time(:millisecond)
  Logger.info("📚 Async load_posts completed in #{load_end - load_start}ms with #{length(posts_to_show)} posts")

  {:noreply, assign(socket, posts: posts_to_show, posts_loading: false, has_more_posts: has_more, page: page)}
end

defp start_async_operations(username, current_user_id, target_user_id) do
  send(self(), {:load_posts, username, 1})
  send(self(), {:load_follow_data, current_user_id, target_user_id})
end

defp get_posts_cached(username, page, limit) do
  Logger.info("📚 Getting posts for #{username}, page #{page}, limit #{limit}")

  all_posts = Posts.get_posts_by_author(username)

  offset = (page - 1) * limit
  total_posts = length(all_posts)

  posts = all_posts
  |> Enum.drop(offset)
  |> Enum.take(limit)

  has_more = offset + limit < total_posts

  Logger.info("📚 Found #{length(posts)} posts, has_more: #{has_more}")
  {posts, has_more}
end

defp assign_loading_states(socket) do
  socket
  |> assign(posts: [])
  |> assign(posts_loading: true)
end

def handle_info(:reload_posts, socket) do
  username = case socket.assigns[:user] do
    %{username: username} -> username
    _ -> socket.assigns.current_user.username
  end

  socket = assign(socket, posts: [], posts_loading: true, page: 1, has_more_posts: true)
  send(self(), {:load_posts, username, 1})
  {:noreply, socket}
end

def handle_info({:EXIT, pid, reason}, socket) do
  case reason do
    :normal ->
      Logger.debug("Task #{inspect(pid)} completed normally")
    :killed ->
      Logger.info("Task #{inspect(pid)} was killed")
    other ->
      Logger.warn("Task #{inspect(pid)} exited with reason: #{inspect(other)}")
  end

  {:noreply, socket}
end

def handle_info(message, socket) do
  Logger.debug("Received unexpected message: #{inspect(message)}")
  {:noreply, socket}
end

  defp fetch_and_cache_posts(username, page, limit, cache_key, current_time) do
  all_posts = Posts.get_posts_by_author(username)

  offset = (page - 1) * limit
  total_posts = length(all_posts)

  posts = all_posts
  |> Enum.drop(offset)
  |> Enum.take(limit)

  has_more = offset + limit < total_posts

  result = {posts, has_more}
  :ets.insert(:posts_cache, {cache_key, result, current_time})
  result
end

  @impl true
  def mount(%{"username" => username} = _params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting MOUNT MazarynWeb.UserLive.Profile for username #{username}")

    Process.flag(:trap_exit, true)
    :erlang.process_flag(:priority, :high)

    result = case get_current_user_cached(session_uuid) do
      {:ok, current_user} ->
        case get_user_by_username_cached(username) do
          {:ok, user} ->
            post_changeset = Post.changeset(%Post{})
            user_changeset = User.changeset(user)
            privacy = if user.private, do: "private", else: "public"

            socket =
              socket
              |> assign_base_data(session_uuid, current_user, user, post_changeset, user_changeset, privacy)
              |> assign_loading_states()
              |> assign_follow_data_optimistic(current_user.id, user.id)

            start_async_operations(username, current_user.id, user.id)

            {:ok, socket}

          error ->
            handle_user_not_found_error(error, socket, username)
        end

      {:error, reason} ->
        Logger.error("❌ Session not found: #{inspect(reason)}")
        {:ok, socket |> put_flash(:error, "Session not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}
    end

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 MOUNT MazarynWeb.UserLive.Profile completed in #{mount_end - mount_start}ms")
    result
  end

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting MOUNT MazarynWeb.UserLive.Profile (no username)")

    Process.flag(:trap_exit, true)
    :erlang.process_flag(:priority, :high)

    result = case get_current_user_cached(session_uuid) do
      {:ok, current_user} ->
        post_changeset = Post.changeset(%Post{})
        user_changeset = User.changeset(%User{})

        socket =
          socket
          |> assign(session_uuid: session_uuid)
          |> assign(posts: [])
          |> assign(posts_loading: true)
          |> assign(post_changeset: post_changeset)
          |> assign(user_changeset: user_changeset)
          |> assign(current_user: current_user)
          |> assign(results: [])
          |> assign(page: 1)
          |> assign(has_more_posts: true)

        start_async_operations(current_user.username, current_user.id, current_user.id)
        {:ok, socket}

      {:error, reason} ->
        Logger.error("❌ Session not found: #{inspect(reason)}")
        {:ok, socket |> put_flash(:error, "Session not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}
    end

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 MOUNT MazarynWeb.UserLive.Profile (no username) completed in #{mount_end - mount_start}ms")
    result
  end

  @impl true
  def handle_params(%{"username" => username} = _params, _uri, socket) do
    params_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting HANDLE PARAMS MazarynWeb.UserLive.Profile for username #{username}")

    current_user = socket.assigns.current_user
    post_changeset = Post.changeset(%Post{})
    user_changeset = User.changeset(%User{})

    result = case get_user_by_username_cached(username) do
      {:ok, user} ->
        socket =
          socket
          |> assign(post_changeset: post_changeset)
          |> assign(user_changeset: user_changeset)
          |> assign(user: user)
          |> assign(search: nil)
          |> assign(current_user: current_user)
          |> assign(posts: [])
          |> assign(posts_loading: true)
          |> assign(page: 1)
          |> assign(has_more_posts: true)
          |> assign_follow_data_optimistic(current_user.id, user.id)

        start_async_operations(username, current_user.id, user.id)
        {:noreply, socket}

      error ->
        handle_user_not_found_error(error, socket, username)
    end

    params_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 HANDLE PARAMS MazarynWeb.UserLive.Profile completed in #{params_end - params_start}ms")
    result
  end

  def handle_params(_params, _uri, socket) do
    params_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting HANDLE PARAMS MazarynWeb.UserLive.Profile (no username)")

    current_user = socket.assigns.current_user
    post_changeset = Post.changeset(%Post{})
    user_changeset = User.changeset(%User{})

    socket =
      socket
      |> assign(post_changeset: post_changeset)
      |> assign(user_changeset: user_changeset)
      |> assign(search: nil)
      |> assign(user: current_user)
      |> assign(current_user: current_user)
      |> assign(page: 1)
      |> assign(has_more_posts: true)

    params_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 HANDLE PARAMS MazarynWeb.UserLive.Profile (no username) completed in #{params_end - params_start}ms")
    {:noreply, socket}
  end

  @impl true
  def handle_info({:load_posts, username, page}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("📚 Starting async load_posts for #{username}, page #{page}")

    posts_task = Task.async(fn ->
      get_posts_cached(username, page, @max_posts_per_page)
    end)

    {posts, has_more} = case Task.yield(posts_task, @task_timeout) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, result} ->
        Logger.info("📚 Successfully loaded posts for #{username}")
        result
      nil ->
        Logger.warn("⏰ Timeout loading posts for #{username}, using empty list")
        {[], false}
    end

    current_posts = if page == 1, do: [], else: socket.assigns.posts
    all_posts = current_posts ++ posts

    load_end = :erlang.system_time(:millisecond)
    Logger.info("📚 Async load_posts completed in #{load_end - load_start}ms")

    {:noreply, assign(socket, posts: all_posts, posts_loading: false, has_more_posts: has_more, page: page)}
  end

  def handle_info({:load_more_posts}, socket) do
    current_user = socket.assigns[:user] || socket.assigns.current_user
    next_page = socket.assigns.page + 1

    socket = assign(socket, posts_loading: true)
    send(self(), {:load_posts, current_user.username, next_page})
    {:noreply, socket}
  end

  def handle_info(:reload_posts, socket) do
    current_user = socket.assigns.current_user
    socket = assign(socket, posts: [], posts_loading: true, page: 1, has_more_posts: true)
    clear_posts_cache(current_user.username)
    send(self(), {:load_posts, current_user.username, 1})
    {:noreply, socket}
  end

  def handle_info({:load_follow_data, user_id, target_id}, socket) do
    follow_start = :erlang.system_time(:millisecond)
    Logger.info("👥 Starting async load_follow_data for user_id #{user_id}")

    {followers_count, followings_count, follow_event, follow_text} =
      get_follow_data_with_timeout(user_id, target_id)

    socket = socket
    |> assign(:follow_event, follow_event)
    |> assign(:follow_text, follow_text)
    |> assign(:followers, followers_count)
    |> assign(:followings, followings_count)

    follow_end = :erlang.system_time(:millisecond)
    Logger.info("👥 Async load_follow_data completed in #{follow_end - follow_start}ms")

    {:noreply, socket}
  end

  def handle_info({:search_debounce, search_term}, socket) do
  case socket.assigns[:last_search] do
    ^search_term ->
      search_start = :erlang.system_time(:millisecond)
      Logger.info("🔍 Executing debounced search for #{search_term}")

      user = search_user_by_username_cached(search_term)

      search_end = :erlang.system_time(:millisecond)
      Logger.info("🔍 Debounced search completed in #{search_end - search_start}ms")

      {:noreply, assign(socket, results: user || [])}

    _ ->
      {:noreply, socket}
  end
end

  defp fetch_and_cache_followers(user_id, cache_key, current_time) do
    count = user_id |> UserClient.get_follower() |> Enum.count()
    :ets.insert(:follow_cache, {cache_key, count, current_time})
    count
  end

  defp followings_cached(user_id) do
    cache_key = {:followings, user_id}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:follow_cache, cache_key) do
      [{_, count, timestamp}] ->
        if current_time - timestamp < @follow_cache_ttl do
          Logger.info("📦 Followings Cache HIT for #{user_id}")
          count
        else
          Logger.info("📦 Followings Cache EXPIRED for #{user_id}")
          fetch_and_cache_followings(user_id, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Followings Cache MISS for #{user_id}")
        fetch_and_cache_followings(user_id, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      :ets.new(:follow_cache, [:set, :public, :named_table])
      followings_cached(user_id)
  end

  defp fetch_and_cache_followings(user_id, cache_key, current_time) do
    count = user_id |> UserClient.get_following() |> Enum.count()
    :ets.insert(:follow_cache, {cache_key, count, current_time})
    count
  end

  defp follow_event_cached(user_id, target_id) do
    if one_of_following_cached?(user_id, target_id), do: "unfollow_user", else: "follow_user"
  end

  defp follow_text_cached(user_id, target_id) do
    if one_of_following_cached?(user_id, target_id), do: "Unfollow", else: "Follow"
  end

  defp one_of_following_cached?(user_id, target_id) do
    cache_key = {:following_check, user_id, target_id}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:follow_cache, cache_key) do
      [{_, result, timestamp}] ->
        if current_time - timestamp < @follow_cache_ttl do
          Logger.info("📦 Following Check Cache HIT for #{user_id} -> #{target_id}")
          result
        else
          Logger.info("📦 Following Check Cache EXPIRED for #{user_id} -> #{target_id}")
          fetch_and_cache_following_check(user_id, target_id, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Following Check Cache MISS for #{user_id} -> #{target_id}")
        fetch_and_cache_following_check(user_id, target_id, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      :ets.new(:follow_cache, [:set, :public, :named_table])
      one_of_following_cached?(user_id, target_id)
  end

  defp fetch_and_cache_following_check(user_id, target_id, cache_key, current_time) do
    result = user_id
    |> UserClient.get_following()
    |> Enum.any?(&(&1 == target_id))
    :ets.insert(:follow_cache, {cache_key, result, current_time})
    result
  end

  defp clear_follow_cache(user_id) do
    try do
      :ets.match_delete(:follow_cache, {{:followers, user_id}, :_, :_})
      :ets.match_delete(:follow_cache, {{:followings, user_id}, :_, :_})
      :ets.match_delete(:follow_cache, {{:following_check, user_id, :_}, :_, :_})
      :ets.match_delete(:follow_cache, {{:following_check, :_, user_id}, :_, :_})
    rescue
      ArgumentError -> :ok
    end
  end

  defp clear_user_cache(username) do
    try do
      :ets.match_delete(:user_cache, {{:user, username}, :_, :_})
    rescue
      ArgumentError -> :ok
    end
  end

  defp clear_posts_cache(username) do
    try do
      :ets.match_delete(:posts_cache, {{:posts, username, :_}, :_, :_})
    rescue
      ArgumentError -> :ok
    end
  end

  defp clear_search_cache(username) do
    try do
      :ets.match_delete(:search_cache, {{:search, username}, :_, :_})
    rescue
      ArgumentError -> :ok
    end
  end

  defp log_performance_metrics(operation, start_time) do
    end_time = :erlang.system_time(:millisecond)
    duration = end_time - start_time

    case duration do
      d when d > 1000 -> Logger.warn("⚠️  Slow operation: #{operation} took #{d}ms")
      d when d > 500 -> Logger.info("⏰ Operation: #{operation} took #{d}ms")
      _ -> Logger.debug("✅ Operation: #{operation} completed quickly")
    end
  end

  defp preload_related_data(socket) do
    socket
  end

  defp cleanup_old_cache_entries do
    current_time = :erlang.system_time(:millisecond)

    try do
      # Clean user cache
      :ets.select_delete(:user_cache, [
        {{{:user, :_}, :_, :"$1"},
         [{:<, {:-, current_time, :"$1"}, @user_cache_ttl * 2}],
         [true]}
      ])

      :ets.select_delete(:follow_cache, [
        {{:_, :_, :"$1"},
         [{:<, {:-, current_time, :"$1"}, @follow_cache_ttl * 2}],
         [true]}
      ])

      :ets.select_delete(:posts_cache, [
        {{:_, :_, :"$1"},
         [{:<, {:-, current_time, :"$1"}, @posts_cache_ttl * 2}],
         [true]}
      ])

      :ets.select_delete(:search_cache, [
        {{:_, :_, :"$1"},
         [{:<, {:-, current_time, :"$1"}, @search_cache_ttl * 2}],
         [true]}
      ])
    rescue
      ArgumentError -> :ok
    end
  end

  defp batch_load_users(user_ids) when is_list(user_ids) do
    user_ids
    |> Enum.chunk_every(10)
    |> Task.async_stream(fn chunk ->
      Enum.map(chunk, &Account.Users.one_by_id/1)
    end, max_concurrency: 5, timeout: @task_timeout)
    |> Enum.flat_map(fn {:ok, results} -> results end)
    |> Enum.filter(&match?({:ok, _}, &1))
    |> Enum.map(&elem(&1, 1))
  end

  defp handle_cache_error(operation, error) do
    Logger.error("Cache error in #{operation}: #{inspect(error)}")

    try do
      :ets.new(:user_cache, [:set, :public, :named_table])
      :ets.new(:follow_cache, [:set, :public, :named_table])
      :ets.new(:posts_cache, [:set, :public, :named_table])
      :ets.new(:search_cache, [:set, :public, :named_table])
      :ets.new(:session_cache, [:set, :public, :named_table])
    rescue
      ArgumentError -> :ok
    end
  end

  defp with_circuit_breaker(operation, fallback \\ nil) do
    try do
      operation.()
    rescue
      error ->
        Logger.error("Circuit breaker activated for operation: #{inspect(error)}")
        fallback || {:error, :circuit_breaker_open}
    end
  end

  defp check_rate_limit(user_id, action) do
  cache_key = {:rate_limit, user_id, action}
  current_time = :erlang.system_time(:second)

  case :ets.lookup(:rate_limit_cache, cache_key) do
    [{_, count, timestamp}] ->
      if current_time - timestamp < 60 do
        if count >= 100 do
          {:error, :rate_limited}
        else
          :ets.insert(:rate_limit_cache, {cache_key, count + 1, timestamp})
          :ok
        end
      else
        :ets.insert(:rate_limit_cache, {cache_key, 1, current_time})
        :ok
      end
    [] ->
      :ets.insert(:rate_limit_cache, {cache_key, 1, current_time})
      :ok
  end
rescue
  ArgumentError ->
    :ets.new(:rate_limit_cache, [:set, :public, :named_table])
    check_rate_limit(user_id, action)
end

  @impl true
  def handle_event("do_search", %{"search" => search}, socket) do
    search_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting handle_event do_search for query #{search}")

    socket = assign(socket, search: search, last_search: search)

    if socket.assigns[:search_timer] do
      Process.cancel_timer(socket.assigns.search_timer)
    end

    timer = Process.send_after(self(), {:search_debounce, search}, @search_debounce_ms)
    socket = assign(socket, search_timer: timer)

    search_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 handle_event do_search completed in #{search_end - search_start}ms")
    {:noreply, socket}
  end

  def handle_event("load_more_posts", _params, socket) do
    if socket.assigns.has_more_posts and not socket.assigns.posts_loading do
      send(self(), {:load_more_posts})
    end
    {:noreply, socket}
  end

  def handle_event("follow_user", %{"userid" => id}, socket) do
    follow_start = :erlang.system_time(:millisecond)
    Logger.info("👥 Starting handle_event follow_user for userid #{id}")

    id = to_charlist(id)
    user_id = socket.assigns.current_user.id

    socket = socket
    |> assign(:follow_event, "unfollow_user")
    |> assign(:follow_text, "Unfollow")
    |> assign(:followers, socket.assigns.followers + 1)

    Task.start(fn ->
      UserClient.follow(user_id, id)
      Core.NotifEvent.follow(user_id, id)
      clear_follow_cache(user_id)
    end)

    send(self(), {:load_follow_data, user_id, id})

    follow_end = :erlang.system_time(:millisecond)
    Logger.info("👥 handle_event follow_user completed in #{follow_end - follow_start}ms")
    {:noreply, socket}
  end

  def handle_event("unfollow_user", %{"userid" => id}, socket) do
    unfollow_start = :erlang.system_time(:millisecond)
    Logger.info("👥 Starting handle_event unfollow_user for userid #{id}")

    id = to_charlist(id)
    user_id = socket.assigns.current_user.id

    socket = socket
    |> assign(:follow_event, "follow_user")
    |> assign(:follow_text, "Follow")
    |> assign(:followers, max(0, socket.assigns.followers - 1))

    Task.start(fn ->
      UserClient.unfollow(user_id, id)
      clear_follow_cache(user_id)
    end)

    send(self(), {:load_follow_data, user_id, id})

    unfollow_end = :erlang.system_time(:millisecond)
    Logger.info("👥 handle_event unfollow_user completed in #{unfollow_end - unfollow_start}ms")
    {:noreply, socket}
  end

  def handle_event("open_modal", %{"action" => "follower"}, socket) do
    modal_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting handle_event open_modal follower")

    followers_task = Task.async(fn ->
      get_followers_with_details(socket.assigns.user.follower)
    end)

    followers = case Task.yield(followers_task, @task_timeout) || Task.shutdown(followers_task, :brutal_kill) do
      {:ok, result} -> result
      nil -> []
    end

    socket = socket
    |> assign(follower_action: true, followers: followers, edit_action: false, follows_action: false)

    modal_end = :erlang.system_time(:millisecond)
    Logger.info("📋 handle_event open_modal follower completed in #{modal_end - modal_start}ms")
    {:noreply, socket}
  end

  def handle_event("open_modal", %{"action" => action}, socket) do
    modal_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting handle_event open_modal #{action}")

    socket = case action do
      "edit" -> socket |> assign(edit_action: true, follower_action: false, follows_action: false)
      "follows" -> socket |> assign(follows_action: true, edit_action: false, follower_action: false)
      "report-user" -> socket |> assign(report_user_action: true, follows_action: false, edit_action: false, follower_action: false)
      "verify-user" -> socket |> assign(report_user_action: false, follows_action: false, edit_action: false, follower_action: false, verified_action: true)
      "unverify-user" -> socket |> assign(report_user_action: false, follows_action: false, edit_action: false, follower_action: false, verified_action: true)
    end

    modal_end = :erlang.system_time(:millisecond)
    Logger.info("📋 handle_event open_modal #{action} completed in #{modal_end - modal_start}ms")
    {:noreply, socket}
  end

  def handle_event("delete_user", %{"username" => username}, socket) do
    delete_start = :erlang.system_time(:millisecond)
    Logger.info("🗑 Starting handle_event delete_user for username #{username}")

    Task.start(fn ->
      UserClient.delete_user(username)
      session_id = socket.assigns.session_uuid
      :ets.delete(:mazaryn_auth_table, :"#{session_id}")
    end)

    socket = socket |> put_flash(:info, "User deletion initiated") |> push_redirect(to: Routes.page_path(socket, :index, "en"))
    delete_end = :erlang.system_time(:millisecond)
    Logger.info("🗑 handle_event delete_user completed in #{delete_end - delete_start}ms")
    {:noreply, socket}
  end

  def handle_event("privacy", %{"user" => %{"privacy" => privacy}}, socket) do
    privacy_start = :erlang.system_time(:millisecond)
    Logger.info("🔒 Starting handle_event privacy with value #{privacy}")

    socket = assign(socket, privacy: privacy)

    Task.start(fn ->
      if privacy == "private" do
        UserClient.make_private(socket.assigns.current_user.id)
      else
        UserClient.make_public(socket.assigns.current_user.id)
      end

      clear_user_cache(socket.assigns.current_user.username)
    end)

    privacy_end = :erlang.system_time(:millisecond)
    Logger.info("🔒 handle_event privacy completed in #{privacy_end - privacy_start}ms")
    {:noreply, socket}
  end

  def handle_event("verify_user", %{"username" => username, "admin_username" => admin_username}, socket) do
    verify_start = :erlang.system_time(:millisecond)
    Logger.info("✅ Starting handle_event verify_user for username #{username}")

    Task.start(fn ->
      ManageUser.verify_user(username, admin_username)
      clear_user_cache(username)
    end)

    socket = socket |> put_flash(:info, "Verification initiated") |> push_redirect(to: Routes.live_path(socket, __MODULE__, socket.assigns.locale, username))
    verify_end = :erlang.system_time(:millisecond)
    Logger.info("✅ handle_event verify_user completed in #{verify_end - verify_start}ms")
    {:noreply, socket}
  end

  def handle_event("unverify_user", %{"username" => username, "admin_username" => admin_username}, socket) do
    unverify_start = :erlang.system_time(:millisecond)
    Logger.info("✅ Starting handle_event unverify_user for username #{username}")

    Task.start(fn ->
      ManageUser.unverify_user(username, admin_username)
      clear_user_cache(username)
    end)

    socket = socket |> put_flash(:info, "Unverification initiated") |> push_redirect(to: Routes.live_path(socket, __MODULE__, socket.assigns.locale, username))
    unverify_end = :erlang.system_time(:millisecond)
    Logger.info("✅ handle_event unverify_user completed in #{unverify_end - unverify_start}ms")
    {:noreply, socket}
  end

  defp assign_base_data(socket, session_uuid, current_user, user, post_changeset, user_changeset, privacy) do
    socket
    |> assign(session_uuid: session_uuid)
    |> assign(post_changeset: post_changeset)
    |> assign(user_changeset: user_changeset)
    |> assign(user: user)
    |> assign(current_user: current_user)
    |> assign(edit_action: false)
    |> assign(follower_action: false)
    |> assign(follows_action: false)
    |> assign(form: to_form(user_changeset))
    |> assign(privacy: privacy)
    |> assign(report_user_action: false)
    |> assign(verified_action: false)
    |> assign(admins: ["arvand"])
    |> assign(results: [])
    |> assign(followers: [])
    |> assign(page: 1)
    |> assign(has_more_posts: true)
  end

  defp assign_loading_states(socket) do
    socket
    |> assign(posts: [])
    |> assign(posts_loading: true)
  end

  defp assign_follow_data_optimistic(socket, user_id, target_id) do
    socket
    |> assign(:follow_event, "follow_user")
    |> assign(:follow_text, "Follow")
    |> assign(:followers, 0)
    |> assign(:followings, 0)
  end

  defp start_async_operations(username, current_user_id, target_user_id) do
    send(self(), {:load_posts, username, 1})
    send(self(), {:load_follow_data, current_user_id, target_user_id})
  end

  defp handle_user_not_found_error(error, socket, username) do
    Logger.error("❌ User not found: #{inspect(error)} for username #{username}")
    {:noreply, socket |> put_flash(:error, "User not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}
  end

  defp get_follow_data_with_timeout(user_id, target_id) do
    tasks = [
      Task.async(fn -> followers_cached(user_id) end),
      Task.async(fn -> followings_cached(user_id) end),
      Task.async(fn -> follow_event_cached(user_id, target_id) end),
      Task.async(fn -> follow_text_cached(user_id, target_id) end)
    ]

    results = tasks
    |> Task.yield_many(@task_timeout)
    |> Enum.map(fn {task, result} ->
      case result do
        {:ok, value} -> value
        nil ->
          Task.shutdown(task, :brutal_kill)
          nil
      end
    end)

    case results do
      [followers, followings, follow_event, follow_text] when not is_nil(followers) ->
        {followers, followings || 0, follow_event || "follow_user", follow_text || "Follow"}
      _ ->
        Logger.warn("⏰ Timeout loading follow data, using defaults")
        {0, 0, "follow_user", "Follow"}
    end
  end

  defp get_current_user_cached(session_uuid) do
    cache_key = {:session, session_uuid}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:session_cache, cache_key) do
      [{_, user, timestamp}] ->
        if current_time - timestamp < @user_cache_ttl do
          Logger.info("📦 Session Cache HIT for #{session_uuid}")
          {:ok, user}
        else
          Logger.info("📦 Session Cache EXPIRED for #{session_uuid}")
          fetch_and_cache_session_user(session_uuid, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Session Cache MISS for #{session_uuid}")
        fetch_and_cache_session_user(session_uuid, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      :ets.new(:session_cache, [:set, :public, :named_table])
      get_current_user_cached(session_uuid)
  end

  defp fetch_and_cache_session_user(session_uuid, cache_key, current_time) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} = result ->
        :ets.insert(:session_cache, {cache_key, user, current_time})
        result
      other -> other
    end
  end

  defp get_user_by_username_cached(username) do
    cache_key = {:user, username}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:user_cache, cache_key) do
      [{_, user, timestamp}] ->
        if current_time - timestamp < @user_cache_ttl do
          Logger.info("📦 User Cache HIT for #{username}")
          {:ok, user}
        else
          Logger.info("📦 User Cache EXPIRED for #{username}")
          fetch_and_cache_user(username, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 User Cache MISS for #{username}")
        fetch_and_cache_user(username, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      :ets.new(:user_cache, [:set, :public, :named_table])
      get_user_by_username_cached(username)
  end

  defp fetch_and_cache_user(username, cache_key, current_time) do
    case Users.one_by_username(username) do
      {:ok, user} = result ->
        :ets.insert(:user_cache, {cache_key, user, current_time})
        result
      other -> other
    end
  end

  defp get_posts_cached(username, page, limit) do
    cache_key = {:posts, username, page}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:posts_cache, cache_key) do
      [{_, {posts, has_more}, timestamp}] ->
        if current_time - timestamp < @posts_cache_ttl do
          Logger.info("📦 Posts Cache HIT for #{username} page #{page}")
          {posts, has_more}
        else
          Logger.info("📦 Posts Cache EXPIRED for #{username} page #{page}")
          fetch_and_cache_posts(username, page, limit, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Posts Cache MISS for #{username} page #{page}")
        fetch_and_cache_posts(username, page, limit, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      :ets.new(:posts_cache, [:set, :public, :named_table])
      get_posts_cached(username, page, limit)
  end

  defp fetch_and_cache_posts(username, page, limit, cache_key, current_time) do
    offset = (page - 1) * limit
    posts = Posts.get_posts_by_author(username, limit: limit + 1, offset: offset)

    has_more = length(posts) > limit
    posts = if has_more, do: Enum.take(posts, limit), else: posts

    result = {posts, has_more}
    :ets.insert(:posts_cache, {cache_key, result, current_time})
    result
  end

  defp search_user_by_username_cached(username) do
    cache_key = {:search, username}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:search_cache, cache_key) do
      [{_, result, timestamp}] ->
        if current_time - timestamp < @search_cache_ttl do
          Logger.info("📦 Search Cache HIT for #{username}")
          result
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
      :ets.new(:search_cache, [:set, :public, :named_table])
      search_user_by_username_cached(username)
  end

  defp fetch_and_cache_search(username, cache_key, current_time) do
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

    :ets.insert(:search_cache, {cache_key, result, current_time})

    fetch_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 search_user_by_username completed in #{fetch_end - fetch_start}ms")
    result
  end

  defp get_followers_with_details(follower_ids) do
    follower_ids
    |> Enum.take(50)
    |> Task.async_stream(fn user_id ->
      Account.Users.one_by_id(user_id)
    end, max_concurrency: @max_concurrent_tasks, timeout: @task_timeout)
    |> Stream.filter(&match?({:ok, {:ok, _}}, &1))
    |> Stream.map(&elem(elem(&1, 1), 1))
    |> Enum.to_list()
  end

  defp followers_cached(user_id) do
    cache_key = {:followers, user_id}
    current_time = :erlang.system_time(:millisecond)

    case :ets.lookup(:follow_cache, cache_key) do
      [{_, count, timestamp}] ->
        if current_time - timestamp < @follow_cache_ttl do
          Logger.info("📦 Followers Cache HIT for #{user_id}")
          count
        else
          Logger.info("📦 Followers Cache EXPIRED for #{user_id}")
          fetch_and_cache_followers(user_id, cache_key, current_time)
        end
      _ ->
        Logger.info("📦 Followers Cache MISS for #{user_id}")
        fetch_and_cache_followers(user_id, cache_key, current_time)
    end
  rescue
    ArgumentError ->
      :ets.new(:follow_cache, [:set, :public, :named_table])
      followers_cached(user_id)
  end
end
