defmodule MazarynWeb.HomeLive.Home do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Mazaryn.Schema.Post
  alias Mazaryn.Posts
  alias Account.Users
  alias Account.User
  alias MazarynWeb.HomeLive.WeatherHelper
  require Logger

  @user_cache_ttl 60_000
  @posts_cache_ttl 20_000
  @comments_cache_ttl 15_000
  @search_cache_ttl 120_000
  @session_cache_ttl 90_000

  @user_load_timeout 1_500
  @posts_load_timeout 2_000
  @comments_load_timeout 1_000
  @search_timeout 800
  @task_timeout 3_000

  @posts_per_page 20
  @initial_posts_limit 15
  @max_concurrent_tasks 6

  @fresh_cache_threshold 8_000
  @stale_cache_threshold 20_000

  @spec init_cache_tables() :: :ok
  defp init_cache_tables do
    tables = [
      :user_cache,
      :posts_cache,
      :comments_cache,
      :search_cache,
      :session_cache,
      :following_cache,
      :rate_limit_cache
    ]

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

    updated_posts =
      update_post_in_list(socket.assigns.posts, post_id, fn post ->
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

    result =
      with {:ok, user} <- get_user_by_session_cached(session_uuid) do
        {cached_posts, cache_status} = get_cached_posts_with_status(user.id)

        socket =
          socket
          |> assign_initial_state()
          |> assign_base_data(session_uuid, user)
          |> assign_posts_immediately(cached_posts, cache_status)
          |> assign_weather_state()

        schedule_immediate_load_if_needed(user.id, cache_status)

        {:ok, socket}
      else
        {:error, reason} ->
          Logger.error("❌ Mount error: #{inspect(reason)}")

          user_id = get_user_id_from_session(session_uuid)
          {cached_posts, cache_status} = get_cached_posts_with_status(user_id)

          socket =
            socket
            |> assign_initial_state()
            |> assign(session_uuid: session_uuid)
            |> assign_posts_immediately(cached_posts, cache_status)
            |> assign_weather_state()

          schedule_immediate_load_if_needed(user_id, cache_status)
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

    result =
      case get_user_cached_optimized(user_id) do
        {:ok, user} ->
          {cached_posts, cache_status} = get_cached_posts_with_status(user.id)

          socket =
            socket
            |> assign_initial_state()
            |> assign_base_data(user_id, user)
            |> assign_posts_immediately(cached_posts, cache_status)
            |> assign_weather_state()

          schedule_immediate_load_if_needed(user.id, cache_status)

          {:ok, socket}

        {:error, _reason} ->
          {cached_posts, cache_status} = get_cached_posts_with_status(user_id)

          socket =
            socket
            |> assign_initial_state()
            |> assign(user_id: user_id)
            |> assign_posts_immediately(cached_posts, cache_status)
            |> assign_weather_state()

          schedule_immediate_load_if_needed(user_id, cache_status)
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

    Logger.info(
      "🏠 HANDLE PARAMS MazarynWeb.HomeLive.Home completed in #{params_end - params_start}ms"
    )

    {:noreply, socket}
  end

  @impl true
  def handle_event("show-comments", %{"id" => post_id}, socket) do
    event_start = :erlang.system_time(:millisecond)
    Logger.info("💬 Starting handle_event show-comments for post #{post_id}")

    import Phoenix.LiveView.JS
    JS.toggle(to: "#test-toggle", in: "fade-in-scale", out: "fade-out-scale")

    comments = get_comments_immediately(post_id)

    event_end = :erlang.system_time(:millisecond)
    Logger.info("💬 handle_event show-comments completed in #{event_end - event_start}ms")
    {:noreply, socket |> assign(:comments, comments) |> assign(:comments_loading, false)}
  end

  @impl true
  def handle_event("open_weather", _params, socket) do
    Logger.info("🌤️ Opening weather modal")
    {:noreply, assign(socket, show_weather_modal: true)}
  end

  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("load-more-posts", _params, socket) do
    Logger.info("📚 Loading more posts")
    user = socket.assigns.user
    current_posts = socket.assigns.posts
    offset = length(current_posts)

    if user do
      send(self(), {:load_more_posts_immediate, user.id, offset})
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
      send(self(), {:load_posts_immediate, user.id})
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
    {:noreply, push_navigate(socket, to: random_id)}
  end

  def handle_event("do_search", %{"search" => search}, socket) do
    search_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting handle_event do_search for query #{search}")

    socket =
      socket
      |> assign(:search, search)
      |> assign(:last_search, search)
      |> assign(:search_loading, true)

    if socket.assigns[:search_timer] do
      Process.cancel_timer(socket.assigns.search_timer)
    end

    timer = Process.send_after(self(), {:search_debounce, search}, 200)
    socket = assign(socket, search_timer: timer)

    search_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 handle_event do_search completed in #{search_end - search_start}ms")
    {:noreply, socket}
  end

  def handle_event("clear_search_results", _params, socket) do
    Logger.info("🔍 Clearing search results")

    socket =
      socket
      |> assign(:search, "")
      |> assign(:results, [])
      |> assign(:search_loading, false)

    {:noreply, socket}
  end

  def handle_event("select_emoji", %{"name" => name}, socket) do
    Logger.info("😀 Selected emoji: #{name}")
    {:noreply, socket}
  end

  def handle_event("mark_notifications_read", _params, socket) do
    Logger.info("📬 Marking notifications as read")
    {:noreply, socket}
  end

  @impl true
  def handle_info({:fetch_weather, city}, socket) do
    Logger.info("🌤️ Fetching weather data for #{city}")

    case WeatherHelper.fetch_complete_weather(city) do
      {:ok, weather_data} ->
        Logger.info("🌤️ Successfully fetched weather data for #{city}")

        {:noreply,
         assign(socket, weather_data: weather_data, weather_loading: false, weather_error: nil)}

      {:error, error_message} ->
        Logger.error("❌ Failed to fetch weather data for #{city}: #{error_message}")

        {:noreply,
         assign(socket, weather_data: nil, weather_loading: false, weather_error: error_message)}
    end
  end

  def handle_info(:close_weather_modal, socket) do
    Logger.info("🌤️ Closing weather modal")
    {:noreply, assign(socket, show_weather_modal: false)}
  end

  @impl true
  def handle_info({:load_user_and_posts_immediate, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("👤📚 Starting immediate load_user_and_posts for user_id #{user_id}")

    user_result =
      case get_user_with_timeout(user_id, @user_load_timeout) do
        {:ok, user} ->
          Logger.info("👤 Successfully loaded user #{user.username}")
          user

        {:error, reason} ->
          Logger.error("❌ Failed to load user: #{inspect(reason)}")
          nil
      end

    posts_result =
      case get_posts_with_timeout(user_id, @initial_posts_limit, @posts_load_timeout) do
        {:ok, posts} ->
          Logger.info("Successfully loaded #{length(posts)} posts for home feed")
          posts

        {:error, _reason} ->
          Logger.warning("Failed to load posts, using cached fallback")
          get_cached_posts_fallback(user_id)
      end

    socket =
      if user_result do
        post_changeset = Post.changeset(%Post{})

        socket =
          socket
          |> assign(post_changeset: post_changeset)
          |> assign(user: user_result)
          |> assign(last_post_loaded_at: :erlang.system_time(:millisecond))

        socket =
          if length(posts_result) > 0 and posts_result != socket.assigns.posts do
            assign(socket, posts: posts_result)
          else
            socket
          end

        assign(socket, posts_loading: false)
      else
        socket
        |> assign(posts: posts_result)
        |> assign(posts_loading: false)
      end

    load_end = :erlang.system_time(:millisecond)
    Logger.info("Immediate load_user_and_posts completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:load_posts_immediate, user_id}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("📚 Starting immediate load_posts for user_id #{user_id}")

    posts_result =
      case get_posts_with_timeout(user_id, @initial_posts_limit, @posts_load_timeout) do
        {:ok, posts} ->
          Logger.info("Successfully loaded #{length(posts)} posts for home feed")
          posts

        {:error, _reason} ->
          Logger.warning("Failed to load posts, using cached fallback")
          get_cached_posts_fallback(user_id)
      end

    socket =
      socket
      |> assign(posts: posts_result)
      |> assign(posts_loading: false)
      |> assign(last_post_loaded_at: :erlang.system_time(:millisecond))

    load_end = :erlang.system_time(:millisecond)
    Logger.info("Immediate load_posts completed in #{load_end - load_start}ms")

    {:noreply, socket}
  end

  def handle_info({:load_more_posts_immediate, user_id, offset}, socket) do
    Logger.info("Loading more posts from offset #{offset}")

    new_posts =
      case get_posts_with_timeout(user_id, @posts_per_page, @posts_load_timeout, offset) do
        {:ok, posts} ->
          Logger.info("Successfully loaded #{length(posts)} more posts")
          posts

        {:error, _reason} ->
          Logger.warning("Failed to load more posts")
          []
      end

    current_posts = socket.assigns.posts
    all_posts = current_posts ++ new_posts

    {:noreply, assign(socket, posts: all_posts, posts_loading_more: false)}
  end

  def handle_info({:search_debounce, search_term}, socket) do
    case socket.assigns[:last_search] do
      ^search_term ->
        search_start = :erlang.system_time(:millisecond)
        Logger.info("Executing immediate search for #{search_term}")

        results =
          case search_user_with_timeout(search_term, @search_timeout) do
            {:ok, users} ->
              Logger.info(
                "Successfully found #{length(users || [])} users for query #{search_term}"
              )

              users || []

            {:error, _reason} ->
              Logger.warning("Search failed/timeout")
              []
          end

        search_end = :erlang.system_time(:millisecond)
        Logger.info("Immediate search completed in #{search_end - search_start}ms")

        {:noreply, assign(socket, results: results, search_loading: false)}

      _ ->
        {:noreply, socket}
    end
  end

  def handle_info(:reload_posts, socket) do
    reload_start = :erlang.system_time(:millisecond)
    Logger.info("Starting handle_info reload_posts")

    user = socket.assigns.user

    socket =
      if user do
        clear_posts_cache(user.id)
        send(self(), {:load_posts_immediate, user.id})
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

  def handle_info({:view_post, post_id}, socket) do
    locale = socket.assigns[:locale] || "en"
    post_path = Routes.live_path(socket, MazarynWeb.PostLive.Show, locale, post_id)
    {:noreply, push_navigate(socket, to: post_path)}
  end

  defp assign_weather_state(socket) do
    socket
    |> assign(show_weather_modal: false)
    |> assign(weather_data: nil)
    |> assign(weather_loading: false)
    |> assign(weather_error: nil)
  end

  defp get_cached_posts_with_status(user_id) do
    if user_id do
      cache_key = {:home_posts, user_id, @initial_posts_limit, 0}
      current_time = :erlang.system_time(:millisecond)

      case safe_ets_lookup(:posts_cache, cache_key) do
        [{_, posts, timestamp}] ->
          age = current_time - timestamp

          cache_status =
            cond do
              age < @fresh_cache_threshold -> :fresh
              age < @stale_cache_threshold -> :stale
              true -> :expired
            end

          {posts, cache_status}

        [] ->
          {[], :no_cache}
      end
    else
      {[], :no_cache}
    end
  end

  defp schedule_immediate_load_if_needed(user_id, cache_status) do
    if user_id do
      case cache_status do
        :fresh ->
          :ok

        :stale ->
          Process.send_after(self(), {:load_posts_immediate, user_id}, 100)

        :expired ->
          send(self(), {:load_user_and_posts_immediate, user_id})

        :no_cache ->
          send(self(), {:load_user_and_posts_immediate, user_id})
      end
    end
  end

  defp assign_posts_immediately(socket, cached_posts, cache_status) do
    posts_loading =
      case cache_status do
        :no_cache -> true
        :expired -> true
        _ -> false
      end

    socket
    |> assign(posts: cached_posts)
    |> assign(posts_loading: posts_loading)
  end

  defp get_user_with_timeout(user_id, timeout) do
    task =
      Task.async(fn ->
        get_user_cached_optimized(user_id)
      end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  defp get_posts_with_timeout(user_id, limit, timeout, offset \\ 0) do
    task =
      Task.async(fn ->
        get_user_and_following_posts_optimized(user_id, limit, offset)
      end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, posts} -> {:ok, posts}
      nil -> {:error, :timeout}
    end
  end

  defp search_user_with_timeout(search_term, timeout) do
    task =
      Task.async(fn ->
        search_user_by_username_cached(search_term)
      end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} ->
        if result do
          {:ok, [result]}
        else
          {:ok, []}
        end

      nil ->
        {:error, :timeout}
    end
  end

  defp get_comments_immediately(post_id) do
    try do
      case get_comments_cached(post_id) do
        comments when is_list(comments) and length(comments) > 0 ->
          comments

        _ ->
          task =
            Task.async(fn ->
              post_id |> to_charlist() |> Mazaryn.Posts.get_comment_by_post_id()
            end)

          case Task.yield(task, @comments_load_timeout) || Task.shutdown(task, :brutal_kill) do
            {:ok, comments} when is_list(comments) ->
              Logger.info("Successfully loaded #{length(comments)} comments immediately")
              comments

            _ ->
              Logger.warning("Failed to load comments immediately")
              []
          end
      end
    rescue
      e ->
        Logger.error("Error loading comments immediately: #{inspect(e)}")
        []
    end
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
        []
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

      {:error, _} = error ->
        error

      other ->
        {:error, other}
    end
  end

  defp fetch_and_cache_home_user_optimized(user_id, cache_key, current_time) do
    case Users.one_by_email(user_id) do
      {:ok, user} = result ->
        safe_ets_insert(:user_cache, {cache_key, user, current_time})
        result

      other ->
        other
    end
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

  defp fetch_and_cache_search(username, cache_key, current_time) do
    try do
      fetch_start = :erlang.system_time(:millisecond)

      result =
        case username |> String.downcase() |> Core.UserClient.search_user() do
          :username_not_exist ->
            nil

          erl_user ->
            case User.erl_changeset(erl_user) |> User.build() do
              {:ok, user} -> user
              {:error, _} -> nil
            end
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

      Logger.info(
        "Following user IDs: #{inspect(following_user_ids)} (#{length(following_user_ids)} users)"
      )

      all_posts =
        all_user_ids
        |> Task.async_stream(
          fn uid ->
            Posts.get_posts_by_user_id(uid)
          end,
          max_concurrency: @max_concurrent_tasks,
          timeout: 1000,
          on_timeout: :kill_task
        )
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
    |> assign(posts_loading: false)
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
end
