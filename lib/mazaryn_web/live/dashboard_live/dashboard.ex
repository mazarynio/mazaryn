defmodule MazarynWeb.DashboardLive.Dashboard do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Core.UserClient
  alias Mazaryn.Posts
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info("🎯 Loading Dashboard for user_id #{user_id}")

    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "user_updates")
    Phoenix.PubSub.subscribe(Mazaryn.PubSub, "dashboard_updates_#{user_id}")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        socket = socket
        |> assign_user_data(user)
        |> assign_dashboard_metrics(user_id)
        |> assign_loading_states()

        {:ok, socket}

      {:error, _reason} ->
        {:ok, redirect(socket, to: "/")}
    end
  end

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("🎯 Loading Dashboard for session_uuid #{session_uuid}")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        Phoenix.PubSub.subscribe(Mazaryn.PubSub, "user_updates")
        Phoenix.PubSub.subscribe(Mazaryn.PubSub, "dashboard_updates_#{user.id}")

        socket = socket
        |> assign_user_data(user)
        |> assign_dashboard_metrics(user.id)
        |> assign_loading_states()

        {:ok, socket}

      {:error, _reason} ->
        {:ok, redirect(socket, to: "/")}
    end
  end

  @impl true
  def handle_params(_params, url, socket) do
    socket = assign(socket, current_path: URI.parse(url).path)
    {:noreply, socket}
  end

  @impl true
  def handle_event("refresh_dashboard", _params, socket) do
    user_id = socket.assigns.user.id

    socket = socket
    |> assign(:metrics_loading, true)
    |> assign_dashboard_metrics(user_id)

    {:noreply, socket}
  end

  def handle_event("view_analytics", _params, socket) do
    {:noreply, put_flash(socket, :info, "Analytics coming soon!")}
  end

  def handle_event("view_insights", _params, socket) do
    {:noreply, put_flash(socket, :info, "Insights coming soon!")}
  end

  @impl true
  def handle_info({:user_level_updated, user_id, new_level}, socket) do
    if socket.assigns.user.id == user_id do
      {:noreply, assign(socket, user_level: new_level)}
    else
      {:noreply, socket}
    end
  end

  def handle_info({:dashboard_metrics_updated, user_id, metrics}, socket) do
    if socket.assigns.user.id == user_id do
      {:noreply, assign(socket, metrics)}
    else
      {:noreply, socket}
    end
  end

  def handle_info(_msg, socket), do: {:noreply, socket}


  defp assign_user_data(socket, user) do
    socket
    |> assign(:user, user)
    |> assign(:page_title, "Dashboard - #{user.username}")
  end

  defp assign_dashboard_metrics(socket, user_id) do
    user_level = get_user_level_safe(user_id)

    stats = get_user_stats(user_id)

    recent_posts = get_recent_posts(user_id, 5)

    engagement = get_engagement_metrics(user_id)

    growth = get_growth_metrics(user_id)

    socket
    |> assign(:user_level, user_level)
    |> assign(:stats, stats)
    |> assign(:recent_posts, recent_posts)
    |> assign(:engagement, engagement)
    |> assign(:growth, growth)
    |> assign(:metrics_loading, false)
  end

  defp assign_loading_states(socket) do
    socket
    |> assign(:metrics_loading, true)
    |> assign(:posts_loading, true)
  end

  defp get_user_level_safe(user_id) do
    try do
      case UserClient.get_user_level(user_id) do
        level when is_integer(level) -> level
        level when is_binary(level) -> String.to_integer(level)
        level when is_list(level) ->
          level |> List.to_string() |> String.to_integer()
        _ -> 1
      end
    rescue
      _ -> 1
    end
  end

  defp get_user_stats(user_id) do
    try do
      posts_count = Posts.count_user_posts(user_id) || 0
      followers_count = length(UserClient.get_follower(user_id) || [])
      following_count = length(UserClient.get_following(user_id) || [])
      saved_posts_count = length(UserClient.get_save_posts(user_id) || [])

      %{
        posts: posts_count,
        followers: followers_count,
        following: following_count,
        saved_posts: saved_posts_count
      }
    rescue
      _ -> %{posts: 0, followers: 0, following: 0, saved_posts: 0}
    end
  end

  defp get_recent_posts(user_id, limit) do
    try do
      Posts.get_posts_by_user_id(user_id)
      |> Enum.take(limit)
    rescue
      _ -> []
    end
  end

  defp get_engagement_metrics(user_id) do
    try do
      recent_posts = Posts.get_posts_by_user_id(user_id) |> Enum.take(10)

      total_likes = recent_posts
      |> Enum.map(fn post ->
        try do
          Home.Like.get_likes_count(to_charlist(post.id)) || 0
        rescue
          _ -> 0
        end
      end)
      |> Enum.sum()

      total_comments = recent_posts
      |> Enum.map(fn post ->
        try do
          post.id |> to_charlist() |> Mazaryn.Posts.get_comment_by_post_id() |> length()
        rescue
          _ -> 0
        end
      end)
      |> Enum.sum()

      avg_engagement = if length(recent_posts) > 0 do
        (total_likes + total_comments) / length(recent_posts)
      else
        0
      end

      %{
        total_likes: total_likes,
        total_comments: total_comments,
        avg_engagement: Float.round(avg_engagement, 1),
        recent_posts_count: length(recent_posts)
      }
    rescue
      _ -> %{total_likes: 0, total_comments: 0, avg_engagement: 0, recent_posts_count: 0}
    end
  end

  defp get_growth_metrics(user_id) do
    %{
      followers_growth: "+12%",
      posts_growth: "+8%",
      engagement_growth: "+15%",
      level_progress: get_level_progress(get_user_level_safe(user_id))
    }
  end

  defp get_level_progress(level) do
    progress = rem(level * 15, 100)
    if progress < 20, do: progress + 20, else: progress
  end

  defp get_level_description(level) when level >= 50, do: "Legendary Influencer"
  defp get_level_description(level) when level >= 40, do: "Master Creator"
  defp get_level_description(level) when level >= 30, do: "Expert User"
  defp get_level_description(level) when level >= 20, do: "Advanced Member"
  defp get_level_description(level) when level >= 10, do: "Active Member"
  defp get_level_description(level) when level >= 5, do: "Growing User"
  defp get_level_description(_), do: "New Member"

  defp get_level_color(level) when level >= 50, do: "from-purple-600 to-pink-600"
  defp get_level_color(level) when level >= 40, do: "from-indigo-600 to-purple-600"
  defp get_level_color(level) when level >= 30, do: "from-blue-600 to-indigo-600"
  defp get_level_color(level) when level >= 20, do: "from-green-600 to-blue-600"
  defp get_level_color(level) when level >= 10, do: "from-yellow-600 to-green-600"
  defp get_level_color(level) when level >= 5, do: "from-orange-600 to-yellow-600"
  defp get_level_color(_), do: "from-gray-600 to-gray-700"

  defp format_number(num) when num >= 1_000_000 do
    "#{Float.round(num / 1_000_000, 1)}M"
  end

  defp format_number(num) when num >= 1_000 do
    "#{Float.round(num / 1_000, 1)}K"
  end

  defp format_number(num), do: to_string(num)
end
