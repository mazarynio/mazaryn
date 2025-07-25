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

  @impl true
  def mount(%{"username" => username} = _params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🔍 Starting MOUNT MazarynWeb.UserLive.Profile for username #{username}")

    result = case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        user_start = :erlang.system_time(:millisecond)
        user_result = get_user_by_username(username)
        user_end = :erlang.system_time(:millisecond)
        Logger.info("📋 get_user_by_username for #{username} took #{user_end - user_start}ms")

        case user_result do
          {:ok, user} ->
            post_changeset = Post.changeset(%Post{})
            user_changeset = User.changeset(user)
            privacy = if user.private, do: "private", else: "public"

            assigns_start = :erlang.system_time(:millisecond)
            socket =
              socket
              |> assign(session_uuid: session_uuid)
              |> assign(post_changeset: post_changeset)
              |> assign(user_changeset: user_changeset)
              |> assign(user: user)
              |> assign(current_user: current_user)
              |> handle_assigns(current_user.id, username)
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
            assigns_end = :erlang.system_time(:millisecond)
            Logger.info("📋 handle_assigns for user #{username} took #{assigns_end - assigns_start}ms")

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

        posts_task = Task.async(fn ->
          fetch_start = :erlang.system_time(:millisecond)
          posts = Posts.get_posts_by_author(current_user.username)
          fetch_end = :erlang.system_time(:millisecond)
          Logger.info("📚 Posts.get_posts_by_author for #{current_user.username} took #{fetch_end - fetch_start}ms")
          posts
        end)

        posts = case Task.yield(posts_task, 1000) || Task.shutdown(posts_task, :brutal_kill) do
          {:ok, posts} -> posts
          nil ->
            Logger.warn("⏰ Timeout fetching posts for #{current_user.username}, using empty list")
            []
        end

        socket =
          socket
          |> assign(session_uuid: session_uuid)
          |> assign(posts: posts)
          |> assign(post_changeset: post_changeset)
          |> assign(user_changeset: user_changeset)
          |> assign(current_user: current_user)
          |> assign(results: [])

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

    posts_task = Task.async(fn ->
      fetch_start = :erlang.system_time(:millisecond)
      posts = Mazaryn.Posts.get_posts_by_author(username)
      fetch_end = :erlang.system_time(:millisecond)
      Logger.info("📚 Posts.get_posts_by_author for #{username} took #{fetch_end - fetch_start}ms")
      posts
    end)

    posts = case Task.yield(posts_task, 1000) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, posts} -> posts
      nil ->
        Logger.warn("⏰ Timeout fetching posts for #{username}, using empty list")
        []
    end

    result = case get_user_by_username(username) do
      {:ok, user} ->
        assigns_start = :erlang.system_time(:millisecond)
        socket =
          socket
          |> assign(post_changeset: post_changeset)
          |> assign(user_changeset: user_changeset)
          |> assign(user: user)
          |> assign(search: nil)
          |> assign(current_user: current_user)
          |> assign(posts: posts)
          |> handle_assigns(current_user.id, user.id)
        assigns_end = :erlang.system_time(:millisecond)
        Logger.info("📋 handle_assigns for user #{username} took #{assigns_end - assigns_start}ms")
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
  def handle_info(:reload_posts, socket) do
    reload_start = :erlang.system_time(:millisecond)
    Logger.info("🔄 Starting handle_info :reload_posts for user #{socket.assigns.current_user.username}")

    current_user = socket.assigns.current_user
    posts_task = Task.async(fn ->
      fetch_start = :erlang.system_time(:millisecond)
      posts = Posts.get_posts_by_author(current_user.username)
      fetch_end = :erlang.system_time(:millisecond)
      Logger.info("📚 Posts.get_posts_by_author for #{current_user.username} took #{fetch_end - fetch_start}ms")
      posts
    end)

    posts = case Task.yield(posts_task, 1000) || Task.shutdown(posts_task, :brutal_kill) do
      {:ok, posts} -> posts
      nil ->
        Logger.warn("⏰ Timeout fetching posts for #{current_user.username}, using empty list")
        []
    end

    reload_end = :erlang.system_time(:millisecond)
    Logger.info("🔄 handle_info :reload_posts completed in #{reload_end - reload_start}ms")
    {:noreply, assign(socket, posts: posts)}
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

    socket = handle_assigns(socket, user_id, id)
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

    socket = handle_assigns(socket, user_id, id)
    unfollow_end = :erlang.system_time(:millisecond)
    Logger.info("👥 handle_event unfollow_user completed in #{unfollow_end - unfollow_start}ms")
    {:noreply, socket}
  end

  def handle_event("open_modal", %{"action" => "follower"}, socket) do
    modal_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting handle_event open_modal follower")

    followers = socket.assigns.user.follower
    |> Enum.map(fn user ->
      fetch_start = :erlang.system_time(:millisecond)
      result = Account.Users.one_by_id(user)
      fetch_end = :erlang.system_time(:millisecond)
      Logger.info("📋 Account.Users.one_by_id for follower #{user} took #{fetch_end - fetch_start}ms")
      result
    end)
    |> Enum.filter(&match?({:ok, _}, &1))
    |> Enum.map(&elem(&1, 1))
    |> IO.inspect(label: "followerss --<>")

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

    result = case get_user_by_username(socket.assigns.current_user.username) do
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

  defp handle_assigns(socket, user_id, id) do
    assigns_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting handle_assigns for user_id #{user_id} and id #{id}")

    followers_task = Task.async(fn ->
      fetch_start = :erlang.system_time(:millisecond)
      count = followers(user_id)
      fetch_end = :erlang.system_time(:millisecond)
      Logger.info("📋 followers count for user_id #{user_id} took #{fetch_end - fetch_start}ms")
      count
    end)

    followings_task = Task.async(fn ->
      fetch_start = :erlang.system_time(:millisecond)
      count = followings(user_id)
      fetch_end = :erlang.system_time(:millisecond)
      Logger.info("📋 followings count for user_id #{user_id} took #{fetch_end - fetch_start}ms")
      count
    end)

    followers_count = case Task.yield(followers_task, 500) || Task.shutdown(followers_task, :brutal_kill) do
      {:ok, count} -> count
      nil ->
        Logger.warn("⏰ Timeout fetching followers for user_id #{user_id}, using 0")
        0
    end

    followings_count = case Task.yield(followings_task, 500) || Task.shutdown(followings_task, :brutal_kill) do
      {:ok, count} -> count
      nil ->
        Logger.warn("⏰ Timeout fetching followings for user_id #{user_id}, using 0")
        0
    end

    socket =
      socket
      |> assign(:follow_event, follow_event(user_id, id))
      |> assign(:follow_text, follow_text(user_id, id))
      |> assign(:followers, followers_count)
      |> assign(:followings, followings_count)

    assigns_end = :erlang.system_time(:millisecond)
    Logger.info("📋 handle_assigns completed in #{assigns_end - assigns_start}ms")
    socket
  end

  defp get_user_by_username(username) do
    fetch_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting get_user_by_username for #{username}")
    result = Users.one_by_username(username)
    fetch_end = :erlang.system_time(:millisecond)
    Logger.info("📋 get_user_by_username for #{username} completed in #{fetch_end - fetch_start}ms")
    result
  end

  defp one_of_following?(id, username) do
    fetch_start = :erlang.system_time(:millisecond)
    Logger.info("👥 Starting one_of_following? for id #{id} and username #{username}")

    result = id
    |> UserClient.get_following()
    |> Enum.any?(&(&1 == username))

    fetch_end = :erlang.system_time(:millisecond)
    Logger.info("👥 one_of_following? completed in #{fetch_end - fetch_start}ms")
    result
  end

  defp follow_text(id, username) do
    fetch_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting follow_text for id #{id} and username #{username}")

    result = if one_of_following?(id, username), do: "Unfollow", else: "Follow"
    fetch_end = :erlang.system_time(:millisecond)
    Logger.info("📋 follow_text completed in #{fetch_end - fetch_start}ms")
    result
  end

  defp follow_event(id, username) do
    fetch_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting follow_event for id #{id} and username #{username}")

    result = if one_of_following?(id, username), do: "unfollow_user", else: "follow_user"
    fetch_end = :erlang.system_time(:millisecond)
    Logger.info("📋 follow_event completed in #{fetch_end - fetch_start}ms")
    result
  end

  defp followers(user_id) do
    fetch_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting followers for user_id #{user_id}")

    count = user_id |> UserClient.get_follower() |> Enum.count()
    fetch_end = :erlang.system_time(:millisecond)
    Logger.info("📋 followers completed in #{fetch_end - fetch_start}ms")
    count
  end

  defp followings(user_id) do
    fetch_start = :erlang.system_time(:millisecond)
    Logger.info("📋 Starting followings for user_id #{user_id}")

    count = user_id |> UserClient.get_following() |> Enum.count()
    fetch_end = :erlang.system_time(:millisecond)
    Logger.info("📋 followings completed in #{fetch_end - fetch_start}ms")
    count
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
