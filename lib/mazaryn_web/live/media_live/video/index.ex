defmodule MazarynWeb.MediaLive.Video.Index do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Mazaryn.Schema.Post

  @user_cache_ttl 60_000
  @session_cache_ttl 90_000

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🎬 Starting MOUNT MazarynWeb.MediaLive.Video.Index")

    init_cache_tables()

    result =
      with {:ok, user} <- get_current_user_cached(session_uuid) do
        {:ok,
         socket
         |> assign(:page_title, "Videos")
         |> assign(:featured_videos, [])
         |> assign(:artist_videos, [])
         |> assign(:search_query, "")
         |> assign(:active_section, "home")
         |> assign(:user, user)
         |> assign(:current_user, user)
         |> assign(:locale, socket.assigns[:locale] || "en")
         |> load_videos()}
      else
        {:error, reason} ->
          Logger.error("❌ Mount error: #{inspect(reason)}")
          {:ok, redirect(socket, to: Routes.page_path(socket, :index, "en"))}
      end

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🎬 MOUNT completed in #{mount_end - mount_start}ms")
    result
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🎬 Starting MOUNT MazarynWeb.MediaLive.Video.Index for user_id")

    init_cache_tables()

    result =
      case get_user_cached_optimized(user_id) do
        {:ok, user} ->
          {:ok,
           socket
           |> assign(:page_title, "Videos")
           |> assign(:featured_videos, [])
           |> assign(:artist_videos, [])
           |> assign(:search_query, "")
           |> assign(:active_section, "home")
           |> assign(:user, user)
           |> assign(:current_user, user)
           |> assign(:locale, socket.assigns[:locale] || "en")
           |> load_videos()}

        {:error, reason} ->
          Logger.error("❌ Mount error: #{inspect(reason)}")
          {:ok, redirect(socket, to: Routes.page_path(socket, :index, "en"))}
      end

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🎬 MOUNT completed in #{mount_end - mount_start}ms")
    result
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action || :index, params)}
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Videos")
  end

  defp apply_action(socket, _action, _params) do
    socket
    |> assign(:page_title, "Videos")
  end

  @impl true
  def handle_event("upload_video", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_info({:search, query}, socket) do
    {:noreply, assign(socket, :search_query, query)}
  end

  @impl true
  def handle_info({:navigate_section, section}, socket) do
    {:noreply, assign(socket, :active_section, section)}
  end

  @impl true
  def handle_info({:play_video, id}, socket) do
    {:noreply, push_navigate(socket, to: Routes.live_path(socket, MazarynWeb.MediaLive.Video.Show, socket.assigns.locale, id))}
  end

  defp load_videos(socket) do
    socket
    |> assign(:featured_videos, mock_featured_videos())
    |> assign(:artist_videos, mock_artist_videos())
  end

  defp mock_featured_videos do
    [
      %{
        id: 1,
        title: "Over My Head - A Synthwave",
        creator: "Sketch & jam",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-1.jpg",
        duration: "05:43",
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-10.png"
      },
      %{
        id: 2,
        title: "John B Wells Live",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-2.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/ellipse-36-15.png"
      },
      %{
        id: 3,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-3.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/ellipse-36-9.png"
      },
      %{
        id: 4,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-4.jpg",
        duration: nil,
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-7.png"
      },
      %{
        id: 5,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-5.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/ellipse-36-6.png"
      },
      %{
        id: 6,
        title: "Trazo",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-6.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/ellipse-36-3.png"
      },
      %{
        id: 7,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-7.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/ellipse-36-4.png"
      },
      %{
        id: 8,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-8.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/ellipse-36-11.png"
      },
      %{
        id: 9,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-9.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/ellipse-36-5.png"
      },
      %{
        id: 10,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-10.jpg",
        duration: nil,
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-12.png"
      },
      %{
        id: 11,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-11.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/ellipse-36-14.png"
      },
      %{
        id: 12,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-12.jpg",
        duration: nil,
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36.png"
      }
    ]
  end

  defp mock_artist_videos do
    [
      %{
        id: 13,
        title: "Learn this amazing design",
        creator: "Juan",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-13.jpg",
        duration: nil,
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-13.png"
      },
      %{
        id: 14,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-14.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/image.png"
      },
      %{
        id: 15,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-15.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: nil,
        creator_avatar: "/images/ellipse-36-16.png"
      },
      %{
        id: 16,
        title: "Huzar",
        creator: "Huzar official",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-16.jpg",
        duration: nil,
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-2.png"
      }
    ]
  end

  defp get_current_user_cached(session_uuid) do
    cache_key = {:session, session_uuid}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:session_cache, cache_key) do
      [{_, user, timestamp}] when current_time - timestamp < @session_cache_ttl ->
        Logger.info("📦 Session Cache HIT for #{session_uuid}")
        {:ok, user}

      _ ->
        Logger.info("📦 Session Cache MISS/EXPIRED for #{session_uuid}")
        fetch_and_cache_session_user(session_uuid, cache_key, current_time)
    end
  end

  defp get_user_cached_optimized(user_id) do
    cache_key = {:user, user_id}
    current_time = :erlang.system_time(:millisecond)

    case safe_ets_lookup(:user_cache, cache_key) do
      [{_, user, timestamp}] when current_time - timestamp < @user_cache_ttl ->
        Logger.info("📦 User Cache HIT for #{user_id}")
        {:ok, user}

      _ ->
        Logger.info("📦 User Cache MISS/EXPIRED for #{user_id}")
        fetch_and_cache_user(user_id, cache_key, current_time)
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

  defp fetch_and_cache_user(user_id, cache_key, current_time) do
    case Users.one_by_email(user_id) do
      {:ok, user} = result ->
        safe_ets_insert(:user_cache, {cache_key, user, current_time})
        result

      other ->
        other
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

  defp init_cache_tables do
    tables = [:user_cache, :session_cache]

    Enum.each(tables, fn table ->
      case :ets.whereis(table) do
        :undefined -> :ets.new(table, [:set, :public, :named_table])
        _ -> :ok
      end
    end)
  end
end
