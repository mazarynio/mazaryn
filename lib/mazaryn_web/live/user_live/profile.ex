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

  @user_cache_ttl 30_000
  @follow_cache_ttl 10_000

  @impl true
  def mount(%{"username" => username} = _params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting MOUNT MazarynWeb.UserLive.Profile for username #{username}")

    result = case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->

        case get_user_by_username_cached(username) do
          {:ok, user} ->
            post_changeset = Post.changeset(%Post{})
            user_changeset = User.changeset(user)
            privacy = if user.private, do: "private", else: "public"

            socket =
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
              |> assign(posts: [])
              |> assign(posts_loading: true)
              |> assign_follow_data_async(current_user.id, user.id)

            send(self(), {:load_posts, username})

            {:ok, socket}

          {:error, reason} ->
            Logger.error("❌ User not found: #{inspect(reason)}")
            {:ok, socket |> put_flash(:error, "User not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}

          :username_not_exist ->
            Logger.error("❌ Username #{username} does not exist")
            {:ok, socket |> put_flash(:error, "User not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}

          :ok ->
            Logger.error("❌ Unexpected :ok response for username #{username}")
            {:ok, socket |> put_flash(:error, "User not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}
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

    result = case Users.get_by_session_uuid(session_uuid) do
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

        send(self(), {:load_posts, current_user.username})

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
          |> assign_follow_data_async(current_user.id, user.id)

        send(self(), {:load_posts, username})
        {:noreply, socket}

      {:error, reason} ->
        Logger.error("❌ User not found: #{inspect(reason)}")
        {:noreply, socket |> put_flash(:error, "User not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}

      :username_not_exist ->
        Logger.error("❌ Username #{username} does not exist")
        {:noreply, socket |> put_flash(:error, "User not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}

      :ok ->
        Logger.error("❌ Unexpected :ok response for username #{username}")
        {:noreply, socket |> put_flash(:error, "User not found") |> push_redirect(to: Routes.page_path(socket, :index, "en"))}
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

    params_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 HANDLE PARAMS MazarynWeb.UserLive.Profile (no username) completed in #{params_end - params_start}ms")
    {:noreply, socket}
  end

  @impl true
  def handle_info({:load_posts, username}, socket) do
    load_start = :erlang.system_time(:millisecond)
    Logger.info("📚 Starting async load_posts for #{username}")

    posts_task = Task.async(fn ->
      Posts.get_posts_by_author(username)
    end)

    posts = case Task.yield(posts_task, 5000) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, posts} ->
        Logger.info("📚 Successfully loaded #{length(posts)} posts for #{username}")
        posts
      nil ->
        Logger.warn("⏰ Timeout loading posts for #{username}, using empty list")
        []
    end

    load_end = :erlang.system_time(:millisecond)
    Logger.info("📚 Async load_posts completed in #{load_end - load_start}ms")

    {:noreply, assign(socket, posts: posts, posts_loading: false)}
  end

  def handle_info(:reload_posts, socket) do
    current_user = socket.assigns.current_user
    send(self(), {:load_posts, current_user.username})
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

  @impl true
  def handle_event("do_search", %{"search" => search}, socket) do
    search_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting handle_event do_search for query #{search}")

    user = search_user_by_username(search)
    search_end = :erlang.system_time(:millisecond)
    Logger.info("🔍 handle_event do_search completed in #{search_end - search_start}ms")
    {:noreply, assign(socket, search: search, results: user || [])}
  end

  def handle_event("follow_user", %{"userid" => id}, socket) do
    follow_start = :erlang.system_time(:millisecond)
    Logger.info("👥 Starting handle_event follow_user for userid #{id}")

    id = to_charlist(id)
    user_id = socket.assigns.current_user.id

    UserClient.follow(user_id, id)
    Core.NotifEvent.follow(user_id, id)

    clear_follow_cache(user_id)
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

    UserClient.unfollow(user_id, id)

    clear_follow_cache(user_id)
    send(self(), {:load_follow_data, user_id, id})

    unfollow_end = :erlang.system_time(:millisecond)
    Logger.info("👥 handle_event unfollow_user completed in #{unfollow_end - unfollow_start}ms")
    {:noreply, socket}
  end

  def handle_event("open_modal", %{"action" => "follower"}, socket) do
    modal_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting handle_event open_modal follower")


    followers = socket.assigns.user.follower
    |> Enum.take(50)
    |> Task.async_stream(fn user ->
      Account.Users.one_by_id(user)
    end, max_concurrency: 10, timeout: 1000)
    |> Enum.filter(&match?({:ok, {:ok, _}}, &1))
    |> Enum.map(&elem(elem(&1, 1), 1))

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

    UserClient.delete_user(username)
    session_id = socket.assigns.session_uuid
    :ets.delete(:mazaryn_auth_table, :"#{session_id}")

    socket = socket |> put_flash(:info, "Successfully deleted") |> push_redirect(to: Routes.page_path(socket, :index, "en"))
    delete_end = :erlang.system_time(:millisecond)
    Logger.info("🗑 handle_event delete_user completed in #{delete_end - delete_start}ms")
    {:noreply, socket}
  end

  def handle_event("privacy", %{"user" => %{"privacy" => privacy}}, socket) do
    privacy_start = :erlang.system_time(:millisecond)
    Logger.info("🔒 Starting handle_event privacy with value #{privacy}")

    if privacy == "private" do
      UserClient.make_private(socket.assigns.current_user.id)
    else
      UserClient.make_public(socket.assigns.current_user.id)
    end

    result = case get_user_by_username_cached(socket.assigns.current_user.username) do
      {:ok, user} ->
        user_changeset = User.changeset(user)
        socket = socket |> assign(form: to_form(user_changeset)) |> assign(privacy: privacy)
        {:noreply, socket}

      {:error, reason} ->
        Logger.error("❌ Error updating privacy: #{inspect(reason)}")
        {:noreply, socket |> put_flash(:error, "Error updating privacy")}

      :username_not_exist ->
        Logger.error("❌ Username not found for privacy update")
        {:noreply, socket |> put_flash(:error, "User not found")}

      :ok ->
        Logger.error("❌ Unexpected :ok response for privacy update")
        {:noreply, socket |> put_flash(:error, "User not found")}
    end

    privacy_end = :erlang.system_time(:millisecond)
    Logger.info("🔒 handle_event privacy completed in #{privacy_end - privacy_start}ms")
    result
  end

  def handle_event("verify_user", %{"username" => username, "admin_username" => admin_username}, socket) do
    verify_start = :erlang.system_time(:millisecond)
    Logger.info("✅ Starting handle_event verify_user for username #{username}")

    ManageUser.verify_user(username, admin_username)
    socket = socket |> put_flash(:info, "Update successful") |> push_redirect(to: Routes.live_path(socket, __MODULE__, socket.assigns.locale, username))
    verify_end = :erlang.system_time(:millisecond)
    Logger.info("✅ handle_event verify_user completed in #{verify_end - verify_start}ms")
    {:noreply, socket}
  end

  def handle_event("unverify_user", %{"username" => username, "admin_username" => admin_username}, socket) do
    unverify_start = :erlang.system_time(:millisecond)
    Logger.info("✅ Starting handle_event unverify_user for username #{username}")

    ManageUser.unverify_user(username, admin_username)
    socket = socket |> put_flash(:info, "Update successful") |> push_redirect(to: Routes.live_path(socket, __MODULE__, socket.assigns.locale, username))
    unverify_end = :erlang.system_time(:millisecond)
    Logger.info("✅ handle_event unverify_user completed in #{unverify_end - unverify_start}ms")
    {:noreply, socket}
  end


  defp assign_follow_data_async(socket, user_id, target_id) do

    socket = socket
    |> assign(:follow_event, "follow_user")
    |> assign(:follow_text, "Follow")
    |> assign(:followers, 0)
    |> assign(:followings, 0)


    send(self(), {:load_follow_data, user_id, target_id})
    socket
  end

  defp get_follow_data_with_timeout(user_id, target_id) do

    tasks = [
      Task.async(fn -> followers_cached(user_id) end),
      Task.async(fn -> followings_cached(user_id) end),
      Task.async(fn -> follow_event_cached(user_id, target_id) end),
      Task.async(fn -> follow_text_cached(user_id, target_id) end)
    ]

    results = tasks
    |> Enum.map(fn task ->
      case Task.yield(task, 1000) || Task.shutdown(task, :brutal_kill) do
        {:ok, result} -> result
        nil -> nil
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
