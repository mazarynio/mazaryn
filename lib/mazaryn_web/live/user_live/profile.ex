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
  @posts_cache_ttl 15_000
  @search_cache_ttl 120_000

  @max_posts_per_page 20
  @max_concurrent_tasks 10
  @task_timeout 3_000
  @search_debounce_ms 300

  @fallback_posts_timeout 5_000
  @fresh_posts_delay 50

  @spec init_cache_tables() :: :ok
  defp init_cache_tables do
    tables = [:user_cache, :follow_cache, :posts_cache, :search_cache, :session_cache, :rate_limit_cache]

    Enum.each(tables, fn table ->
      case :ets.whereis(table) do
        :undefined -> :ets.new(table, [:set, :public, :named_table])
        _ -> :ok
      end
    end)
  end

  @impl true
  def mount(%{"username" => username} = _params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting MOUNT MazarynWeb.UserLive.Profile for username #{username}")

    init_cache_tables()
    Process.flag(:trap_exit, true)

    result = with {:ok, current_user} <- get_current_user_cached(session_uuid),
                  {:ok, user} <- get_user_by_username_cached(username) do
      post_changeset = Post.changeset(%Post{})
      user_changeset = User.changeset(user)
      privacy = if user.private, do: "private", else: "public"

      socket =
        socket
        |> assign_base_data(session_uuid, current_user, user, post_changeset, user_changeset, privacy)
        |> assign_optimistic_states()
        |> assign_follow_data_optimistic(current_user.id, user.id)

      load_posts_with_improved_fallback(username, current_user.id, user.id)

      {:ok, socket}
    else
      {:error, :user_not_found} ->
        handle_user_not_found_error(socket, username)
      {:error, reason} ->
        Logger.error("❌ Mount error: #{inspect(reason)}")
        {:ok, socket |> put_flash(:error, "Session not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}
    end

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 MOUNT MazarynWeb.UserLive.Profile completed in #{mount_end - mount_start}ms")
    result
  end

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting MOUNT MazarynWeb.UserLive.Profile (no username)")

    init_cache_tables()
    Process.flag(:trap_exit, true)

    result = case get_current_user_cached(session_uuid) do
      {:ok, current_user} ->
        post_changeset = Post.changeset(%Post{})
        user_changeset = User.changeset(%User{})

        socket =
          socket
          |> assign(session_uuid: session_uuid)
          |> assign(posts: [])
          |> assign(posts_loading: false)
          |> assign(post_changeset: post_changeset)
          |> assign(user_changeset: user_changeset)
          |> assign(current_user: current_user)
          |> assign(results: [])
          |> assign(page: 1)
          |> assign(has_more_posts: true)

        load_posts_with_improved_fallback(current_user.username, current_user.id, current_user.id)
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
          |> assign(posts_loading: false)
          |> assign(page: 1)
          |> assign(has_more_posts: true)
          |> assign_follow_data_optimistic(current_user.id, user.id)

        load_posts_with_improved_fallback(username, current_user.id, user.id)
        {:noreply, socket}

      {:error, _} = error ->
        handle_user_not_found_error(socket, username)
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

  defp load_posts_with_improved_fallback(username, current_user_id, target_user_id) do
    send(self(), {:load_cached_posts_immediate, username, 1})

    Process.send_after(self(), {:load_fresh_posts, username, 1}, @fresh_posts_delay)

    Process.send_after(self(), {:load_follow_data, current_user_id, target_user_id}, 15)

    Process.send_after(self(), {:posts_fallback_timeout, username}, @fallback_posts_timeout)
  end

  defp load_posts_with_fallback(username, current_user_id, target_user_id) do
    load_posts_with_improved_fallback(username, current_user_id, target_user_id)
  end

  @impl true
  def handle_info({:load_cached_posts_immediate, username, page}, socket) do
    Logger.info("📚 Loading cached posts IMMEDIATELY for #{username}, page #{page}")

    case get_posts_cached_with_extended_ttl(username, page, @max_posts_per_page) do
      {posts, has_more} when length(posts) > 0 ->
        Logger.info("📦 Found #{length(posts)} cached posts (immediate)")
        {:noreply, assign(socket, posts: posts, has_more_posts: has_more, page: page, posts_loading: false)}
      _ ->
        Logger.info("📦 No immediate cached posts found for #{username}")
        {:noreply, assign(socket, posts_loading: true)}
    end
  end

  def handle_info({:load_cached_posts, username, page}, socket) do
    Logger.info("📚 Loading cached posts for #{username}, page #{page}")

    case get_posts_cached_only(username, page, @max_posts_per_page) do
      {posts, has_more} when length(posts) > 0 ->
        Logger.info("📦 Found #{length(posts)} cached posts")
        {:noreply, assign(socket, posts: posts, has_more_posts: has_more, page: page, posts_loading: false)}
      _ ->
        Logger.info("📦 No cached posts found, will wait for fresh load")
        {:noreply, assign(socket, posts_loading: true)}
    end
  end

  def handle_info({:load_fresh_posts, username, page}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("📚 Starting fresh load_posts for #{username}, page #{page}")

    current_posts = socket.assigns.posts || []
    should_show_loading = length(current_posts) == 0

    socket = if should_show_loading do
      assign(socket, posts_loading: true)
    else
      socket
    end

    task = Task.async(fn ->
      try do
        get_posts_fresh(username, page, @max_posts_per_page)
      rescue
        error ->
          Logger.error("Error in load_fresh_posts task: #{inspect(error)}")
          case get_posts_cached_with_extended_ttl(username, page, @max_posts_per_page) do
            {cached_posts, cached_has_more} when length(cached_posts) > 0 ->
              {cached_posts, cached_has_more}
            _ ->
              {[], false}
          end
      end
    end)

    {posts, has_more} = case Task.yield(task, @task_timeout) do
      {:ok, result} ->
        Logger.info("📚 Successfully loaded fresh posts for #{username}")
        result
      nil ->
        Logger.warn("⏰ Timeout loading fresh posts for #{username}, using cached data")
        Task.shutdown(task, :brutal_kill)
        case get_posts_cached_with_extended_ttl(username, page, @max_posts_per_page) do
          {cached_posts, cached_has_more} when length(cached_posts) > 0 ->
            {cached_posts, cached_has_more}
          _ ->
            {[], false}
        end
    end

    all_posts = if page == 1 do
      posts
    else
      current_posts ++ posts
    end

    load_end = :erlang.system_time(:millisecond)
    Logger.info("📚 Fresh load_posts completed in #{load_end - load_start}ms with #{length(all_posts)} total posts")

    {:noreply, assign(socket, posts: all_posts, posts_loading: false, has_more_posts: has_more, page: page)}
  end

  def handle_info({:posts_fallback_timeout, username}, socket) do
    current_posts = socket.assigns.posts || []

    if socket.assigns.posts_loading and length(current_posts) == 0 do
      Logger.info("⏰ Posts fallback timeout for #{username}, showing empty state")
      {:noreply, assign(socket, posts: [], posts_loading: false, has_more_posts: true)}
    else
      Logger.info("⏰ Posts fallback timeout for #{username}, but posts already loaded or loading completed")
      {:noreply, socket}
    end
  end

  def handle_info({:load_posts, username, page}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("📚 Starting async load_posts for #{username}, page #{page}")

    task = Task.async(fn ->
      try do
        get_posts_cached(username, page, @max_posts_per_page)
      rescue
        error ->
          Logger.error("Error in load_posts task: #{inspect(error)}")
          {[], false}
      end
    end)

    {posts, has_more} = case Task.yield(task, @task_timeout) do
      {:ok, result} ->
        Logger.info("📚 Successfully loaded posts for #{username}")
        result
      nil ->
        Logger.warn("⏰ Timeout loading posts for #{username}, killing task")
        Task.shutdown(task, :brutal_kill)
        {[], false}
    end

    all_posts = if page == 1 do
      posts
    else
      current_posts = socket.assigns.posts || []
      current_posts ++ posts
    end

    load_end = :erlang.system_time(:millisecond)
    Logger.info("📚 Async load_posts completed in #{load_end - load_start}ms with #{length(all_posts)} total posts")

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
    username = case socket.assigns[:user] do
      %{username: username} -> username
      _ -> socket.assigns.current_user.username
    end

    clear_posts_cache(username)
    socket = assign(socket, posts: [], posts_loading: false, page: 1, has_more_posts: true)

    load_posts_with_improved_fallback(username, socket.assigns.current_user.id,
                            (socket.assigns[:user] || socket.assigns.current_user).id)
    {:noreply, socket}
  end

  def handle_info({:refresh_posts_cache, username}, socket) do
    Logger.info("🔄 Refreshing posts cache for #{username}")

    current_user_id = socket.assigns.current_user.id
    target_user_id = (socket.assigns[:user] || socket.assigns.current_user).id

    send(self(), {:load_fresh_posts, username, 1})
    Process.send_after(self(), {:clear_old_cache, username}, 1000)

    {:noreply, socket}
  end

  def handle_info({:clear_old_cache, username}, socket) do
    Logger.info("🗑️ Clearing old cache for #{username}")
    clear_posts_cache(username)
    {:noreply, socket}
  end

  def handle_info({:load_follow_data, user_id, target_id}, socket) do
    follow_start = :erlang.system_time(:millisecond)
    Logger.info("👥 Starting async load_follow_data for user_id #{user_id}")

    task = Task.async(fn ->
      try do
        get_follow_data_cached(user_id, target_id)
      rescue
        error ->
          Logger.error("Error in load_follow_data task: #{inspect(error)}")
          {0, 0, "follow_user", "Follow"}
      end
    end)

    {followers_count, followings_count, follow_event, follow_text} =
      case Task.yield(task, @task_timeout) do
        {:ok, result} -> result
        nil ->
          Logger.warn("⏰ Timeout loading follow data")
          Task.shutdown(task, :brutal_kill)
          {0, 0, "follow_user", "Follow"}
      end

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

        task = Task.async(fn ->
          try do
            search_user_by_username_cached(search_term)
          rescue
            error ->
              Logger.error("Error in search task: #{inspect(error)}")
              []
          end
        end)

        results = case Task.yield(task, @task_timeout) do
          {:ok, result} -> result || []
          nil ->
            Logger.warn("⏰ Search timeout")
            Task.shutdown(task, :brutal_kill)
            []
        end

        search_end = :erlang.system_time(:millisecond)
        Logger.info("🔍 Debounced search completed in #{search_end - search_start}ms")

        {:noreply, assign(socket, results: results)}

      _ ->
        {:noreply, socket}
    end
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

  defp get_current_user_cached(session_uuid) do
    cache_key = {:session, session_uuid}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:session_cache, cache_key) do
      [{_, user, timestamp}] when current_time - timestamp < @user_cache_ttl ->
        Logger.info("📦 Session Cache HIT for #{session_uuid}")
        {:ok, user}
      _ ->
        Logger.info("📦 Session Cache MISS/EXPIRED for #{session_uuid}")
        fetch_and_cache_session_user(session_uuid, cache_key, current_time)
    end
  end

  defp get_user_by_username_cached(username) do
    cache_key = {:user, username}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:user_cache, cache_key) do
      [{_, user, timestamp}] when current_time - timestamp < @user_cache_ttl ->
        Logger.info("📦 User Cache HIT for #{username}")
        {:ok, user}
      _ ->
        Logger.info("📦 User Cache MISS/EXPIRED for #{username}")
        fetch_and_cache_user(username, cache_key, current_time)
    end
  end

  defp get_posts_cached(username, page, limit) do
    cache_key = {:posts, username, page}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:posts_cache, cache_key) do
      [{_, {posts, has_more}, timestamp}] when current_time - timestamp < @posts_cache_ttl ->
        Logger.info("📦 Posts Cache HIT for #{username} page #{page}")
        {posts, has_more}
      _ ->
        Logger.info("📦 Posts Cache MISS/EXPIRED for #{username} page #{page}")
        fetch_and_cache_posts(username, page, limit, cache_key, current_time)
    end
  end

  defp get_posts_cached_only(username, page, limit) do
    cache_key = {:posts, username, page}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:posts_cache, cache_key) do
      [{_, {posts, has_more}, timestamp}] when current_time - timestamp < @posts_cache_ttl * 3 ->
        Logger.info("📦 Posts Cache (only) HIT for #{username} page #{page}")
        {posts, has_more}
      _ ->
        Logger.info("📦 No cached posts for #{username} page #{page}")
        {[], false}
    end
  end

  defp get_posts_cached_with_extended_ttl(username, page, limit) do
    cache_key = {:posts, username, page}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:posts_cache, cache_key) do
      [{_, {posts, has_more}, timestamp}] when current_time - timestamp < @posts_cache_ttl * 5 ->
        Logger.info("📦 Posts Cache (extended TTL) HIT for #{username} page #{page}")
        {posts, has_more}
      _ ->
        Logger.info("📦 No extended cached posts for #{username} page #{page}")
        {[], false}
    end
  end

  defp get_posts_fresh(username, page, limit) do
    cache_key = {:posts, username, page}
    current_time = :erlang.system_time(:millisecond)

    result = fetch_posts_from_source(username, page, limit)

    safe_ets_insert(:posts_cache, {cache_key, result, current_time})
    Logger.info("📦 Cached fresh posts for #{username} page #{page}")

    result
  end

  defp get_follow_data_cached(user_id, target_id) do
    followers_count = followers_cached(user_id)
    followings_count = followings_cached(user_id)
    follow_event = follow_event_cached(user_id, target_id)
    follow_text = follow_text_cached(user_id, target_id)

    {followers_count, followings_count, follow_event, follow_text}
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

  defp fetch_and_cache_user(username, cache_key, current_time) do
    case Users.one_by_username(username) do
      {:ok, user} = result ->
        safe_ets_insert(:user_cache, {cache_key, user, current_time})
        result
      {:error, _} = error -> error
      other -> {:error, :user_not_found}
    end
  end

  defp fetch_and_cache_posts(username, page, limit, cache_key, current_time) do
    result = fetch_posts_from_source(username, page, limit)
    safe_ets_insert(:posts_cache, {cache_key, result, current_time})
    result
  end

  defp fetch_posts_from_source(username, page, limit) do
    try do
      all_posts = Posts.get_posts_by_author(username)

      offset = (page - 1) * limit
      total_posts = length(all_posts)

      posts = all_posts
      |> Enum.drop(offset)
      |> Enum.take(limit)

      has_more = offset + limit < total_posts

      {posts, has_more}
    rescue
      error ->
        Logger.error("Error fetching posts for #{username}: #{inspect(error)}")
        {[], false}
    end
  end

  defp followers_cached(user_id) do
    cache_key = {:followers, user_id}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:follow_cache, cache_key) do
      [{_, count, timestamp}] when current_time - timestamp < @follow_cache_ttl ->
        Logger.info("📦 Followers Cache HIT for #{user_id}")
        count
      _ ->
        Logger.info("📦 Followers Cache MISS/EXPIRED for #{user_id}")
        fetch_and_cache_followers(user_id, cache_key, current_time)
    end
  end

  defp fetch_and_cache_followers(user_id, cache_key, current_time) do
    try do
      count = user_id |> UserClient.get_follower() |> Enum.count()
      safe_ets_insert(:follow_cache, {cache_key, count, current_time})
      count
    rescue
      error ->
        Logger.error("Error fetching followers for #{user_id}: #{inspect(error)}")
        0
    end
  end

  defp followings_cached(user_id) do
    cache_key = {:followings, user_id}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:follow_cache, cache_key) do
      [{_, count, timestamp}] when current_time - timestamp < @follow_cache_ttl ->
        Logger.info("📦 Followings Cache HIT for #{user_id}")
        count
      _ ->
        Logger.info("📦 Followings Cache MISS/EXPIRED for #{user_id}")
        fetch_and_cache_followings(user_id, cache_key, current_time)
    end
  end

  defp fetch_and_cache_followings(user_id, cache_key, current_time) do
    try do
      count = user_id |> UserClient.get_following() |> Enum.count()
      safe_ets_insert(:follow_cache, {cache_key, count, current_time})
      count
    rescue
      error ->
        Logger.error("Error fetching followings for #{user_id}: #{inspect(error)}")
        0
    end
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

    case safe_ets_lookup(:follow_cache, cache_key) do
      [{_, result, timestamp}] when current_time - timestamp < @follow_cache_ttl ->
        Logger.info("📦 Following Check Cache HIT for #{user_id} -> #{target_id}")
        result
      _ ->
        Logger.info("📦 Following Check Cache MISS/EXPIRED for #{user_id} -> #{target_id}")
        fetch_and_cache_following_check(user_id, target_id, cache_key, current_time)
    end
  end

  defp fetch_and_cache_following_check(user_id, target_id, cache_key, current_time) do
    try do
      result = user_id
      |> UserClient.get_following()
      |> Enum.any?(&(&1 == target_id))
      safe_ets_insert(:follow_cache, {cache_key, result, current_time})
      result
    rescue
      error ->
        Logger.error("Error checking following status for #{user_id} -> #{target_id}: #{inspect(error)}")
        false
    end
  end

  defp search_user_by_username_cached(username) do
    cache_key = {:search, username}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:search_cache, cache_key) do
      [{_, result, timestamp}] when current_time - timestamp < @search_cache_ttl ->
        Logger.info("📦 Search Cache HIT for #{username}")
        result
      _ ->
        Logger.info("📦 Search Cache MISS/EXPIRED for #{username}")
        fetch_and_cache_search(username, cache_key, current_time)
    end
  end

  defp fetch_and_cache_search(username, cache_key, current_time) do
    try do
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

      safe_ets_insert(:search_cache, {cache_key, result, current_time})

      fetch_end = :erlang.system_time(:millisecond)
      Logger.info("🔍 search_user_by_username completed in #{fetch_end - fetch_start}ms")
      result
    rescue
      error ->
        Logger.error("Error searching for user #{username}: #{inspect(error)}")
        nil
    end
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
      Logger.info("🗑️ Cleared posts cache for #{username}")
    rescue
      ArgumentError -> :ok
    end
  end

  defp clear_all_posts_cache do
    try do
      :ets.match_delete(:posts_cache, {{:posts, :_, :_}, :_, :_})
      Logger.info("🗑️ Cleared all posts cache")
    rescue
      ArgumentError -> :ok
    end
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

  defp assign_optimistic_states(socket) do
    socket
    |> assign(posts: [])
    |> assign(posts_loading: false)
  end

  defp assign_follow_data_optimistic(socket, user_id, target_id) do
    socket
    |> assign(:follow_event, "follow_user")
    |> assign(:follow_text, "Follow")
    |> assign(:followers, 0)
    |> assign(:followings, 0)
  end

  defp handle_user_not_found_error(socket, username) do
    Logger.error("❌ User not found for username #{username}")
    {:noreply, socket |> put_flash(:error, "User not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}
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
      try do
        UserClient.follow(user_id, id)
        Core.NotifEvent.follow(user_id, id)
        clear_follow_cache(user_id)
      rescue
        error -> Logger.error("Error in follow_user task: #{inspect(error)}")
      end
    end)

    Process.send_after(self(), {:load_follow_data, user_id, id}, 100)

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
      try do
        UserClient.unfollow(user_id, id)
        clear_follow_cache(user_id)
      rescue
        error -> Logger.error("Error in unfollow_user task: #{inspect(error)}")
      end
    end)

    Process.send_after(self(), {:load_follow_data, user_id, id}, 100)

    unfollow_end = :erlang.system_time(:millisecond)
    Logger.info("👥 handle_event unfollow_user completed in #{unfollow_end - unfollow_start}ms")
    {:noreply, socket}
  end

  def handle_event("open_modal", %{"action" => "follower"}, socket) do
    modal_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting handle_event open_modal follower")

    Task.start(fn ->
      try do
        followers = get_followers_with_details(socket.assigns.user.follower)
        send(self(), {:followers_loaded, followers})
      rescue
        error ->
          Logger.error("Error loading followers: #{inspect(error)}")
          send(self(), {:followers_loaded, []})
      end
    end)

    socket = socket
    |> assign(follower_action: true, followers: [], edit_action: false, follows_action: false)

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
      try do
        UserClient.delete_user(username)
        session_id = socket.assigns.session_uuid
        :ets.delete(:mazaryn_auth_table, :"#{session_id}")
        clear_all_posts_cache()
      rescue
        error -> Logger.error("Error in delete_user task: #{inspect(error)}")
      end
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
      try do
        if privacy == "private" do
          UserClient.make_private(socket.assigns.current_user.id)
        else
          UserClient.make_public(socket.assigns.current_user.id)
        end

        clear_user_cache(socket.assigns.current_user.username)
      rescue
        error -> Logger.error("Error in privacy task: #{inspect(error)}")
      end
    end)

    privacy_end = :erlang.system_time(:millisecond)
    Logger.info("🔒 handle_event privacy completed in #{privacy_end - privacy_start}ms")
    {:noreply, socket}
  end

  def handle_event("verify_user", %{"username" => username, "admin_username" => admin_username}, socket) do
    verify_start = :erlang.system_time(:millisecond)
    Logger.info("✅ Starting handle_event verify_user for username #{username}")

    Task.start(fn ->
      try do
        ManageUser.verify_user(username, admin_username)
        clear_user_cache(username)
      rescue
        error -> Logger.error("Error in verify_user task: #{inspect(error)}")
      end
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
      try do
        ManageUser.unverify_user(username, admin_username)
        clear_user_cache(username)
      rescue
        error -> Logger.error("Error in unverify_user task: #{inspect(error)}")
      end
    end)

    socket = socket |> put_flash(:info, "Unverification initiated") |> push_redirect(to: Routes.live_path(socket, __MODULE__, socket.assigns.locale, username))
    unverify_end = :erlang.system_time(:millisecond)
    Logger.info("✅ handle_event unverify_user completed in #{unverify_end - unverify_start}ms")
    {:noreply, socket}
  end

  def handle_event("post_created", %{"username" => username}, socket) do
    Logger.info("🆕 Post created for #{username}, refreshing cache with improved strategy")

    current_user_id = socket.assigns.current_user.id
    target_user_id = (socket.assigns[:user] || socket.assigns.current_user).id

    send(self(), {:refresh_posts_cache, username})

    {:noreply, socket}
  end

  def handle_event("comment_added", %{"username" => username}, socket) do
    Logger.info("💬 Comment added for #{username}, refreshing posts")

    current_user_id = socket.assigns.current_user.id
    target_user_id = (socket.assigns[:user] || socket.assigns.current_user).id

    send(self(), {:refresh_posts_cache, username})

    {:noreply, socket}
  end

  def handle_info({:followers_loaded, followers}, socket) do
    {:noreply, assign(socket, followers: followers)}
  end

  defp get_followers_with_details(follower_ids) do
    try do
      follower_ids
      |> Enum.take(50)
      |> Task.async_stream(fn user_id ->
        Account.Users.one_by_id(user_id)
      end, max_concurrency: @max_concurrent_tasks, timeout: @task_timeout)
      |> Stream.filter(&match?({:ok, {:ok, _}}, &1))
      |> Stream.map(&elem(elem(&1, 1), 1))
      |> Enum.to_list()
    rescue
      error ->
        Logger.error("Error getting followers with details: #{inspect(error)}")
        []
    end
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

  defp cleanup_old_cache_entries do
    current_time = :erlang.system_time(:millisecond)

    try do
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
    try do
      user_ids
      |> Enum.chunk_every(10)
      |> Task.async_stream(fn chunk ->
        Enum.map(chunk, &Account.Users.one_by_id/1)
      end, max_concurrency: 5, timeout: @task_timeout)
      |> Enum.flat_map(fn {:ok, results} -> results end)
      |> Enum.filter(&match?({:ok, _}, &1))
      |> Enum.map(&elem(&1, 1))
    rescue
      error ->
        Logger.error("Error in batch_load_users: #{inspect(error)}")
        []
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

  defp log_performance_metrics(operation, start_time) do
    end_time = :erlang.system_time(:millisecond)
    duration = end_time - start_time

    case duration do
      d when d > 1000 -> Logger.warn("⚠️  Slow operation: #{operation} took #{d}ms")
      d when d > 500 -> Logger.info("⏰ Operation: #{operation} took #{d}ms")
      _ -> Logger.debug("✅ Operation: #{operation} completed quickly")
    end
  end

  def clear_user_posts_cache(username) do
    clear_posts_cache(username)
  end

end
