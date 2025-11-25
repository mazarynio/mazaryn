defmodule MazarynWeb.MediaLive.Video.Show do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users

  @user_cache_ttl 60_000
  @session_cache_ttl 90_000

  @impl true
  def mount(%{"id" => video_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🎬 Starting MOUNT MazarynWeb.MediaLive.Video.Show for video #{video_id}")

    init_cache_tables()

    result =
      with {:ok, user} <- get_current_user_cached(session_uuid),
           {:ok, video} <- get_video_by_id(video_id) do
        {:ok,
         socket
         |> assign(:page_title, video.title)
         |> assign(:video, video)
         |> assign(:user, user)
         |> assign(:current_user, user)
         |> assign(:locale, socket.assigns[:locale] || "en")
         |> assign(:video_id, video_id)
         |> assign(:comments, [])
         |> assign(:comment_text, "")
         |> assign(:related_videos, get_related_videos(video_id))
         |> assign(:is_subscribed, false)
         |> assign(:likes, video.likes || 18000)
         |> assign(:dislikes, video.dislikes || 200)
         |> assign(:is_liked, false)
         |> assign(:is_disliked, false)
         |> assign(:is_saved, false)}
      else
        {:error, reason} ->
          Logger.error("❌ Mount error: #{inspect(reason)}")
          {:ok, redirect(socket, to: Routes.live_path(socket, MazarynWeb.MediaLive.Video.Index, socket.assigns[:locale] || "en"))}
      end

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🎬 MOUNT completed in #{mount_end - mount_start}ms")
    result
  end

  @impl true
  def mount(%{"id" => video_id}, %{"user_id" => user_id} = _session, socket) do
    mount_start = :erlang.system_time(:millisecond)
    Logger.info("🎬 Starting MOUNT MazarynWeb.MediaLive.Video.Show for user_id")

    init_cache_tables()

    result =
      with {:ok, user} <- get_user_cached_optimized(user_id),
           {:ok, video} <- get_video_by_id(video_id) do
        {:ok,
         socket
         |> assign(:page_title, video.title)
         |> assign(:video, video)
         |> assign(:user, user)
         |> assign(:current_user, user)
         |> assign(:locale, socket.assigns[:locale] || "en")
         |> assign(:video_id, video_id)
         |> assign(:comments, [])
         |> assign(:comment_text, "")
         |> assign(:related_videos, get_related_videos(video_id))
         |> assign(:is_subscribed, false)
         |> assign(:likes, video.likes || 18000)
         |> assign(:dislikes, video.dislikes || 200)
         |> assign(:is_liked, false)
         |> assign(:is_disliked, false)
         |> assign(:is_saved, false)}
      else
        {:error, reason} ->
          Logger.error("❌ Mount error: #{inspect(reason)}")
          {:ok, redirect(socket, to: Routes.live_path(socket, MazarynWeb.MediaLive.Video.Index, socket.assigns[:locale] || "en"))}
      end

    mount_end = :erlang.system_time(:millisecond)
    Logger.info("🎬 MOUNT completed in #{mount_end - mount_start}ms")
    result
  end

  @impl true
  def handle_params(%{"id" => video_id}, _url, socket) do
    {:noreply, assign(socket, video_id: video_id)}
  end

  @impl true
  def handle_event("like_video", _params, socket) do
    new_likes = if socket.assigns.is_liked, do: socket.assigns.likes - 1, else: socket.assigns.likes + 1
    new_dislikes = if socket.assigns.is_disliked, do: socket.assigns.dislikes - 1, else: socket.assigns.dislikes

    {:noreply,
     socket
     |> assign(:is_liked, !socket.assigns.is_liked)
     |> assign(:is_disliked, false)
     |> assign(:likes, new_likes)
     |> assign(:dislikes, new_dislikes)}
  end

  @impl true
  def handle_event("dislike_video", _params, socket) do
    new_dislikes = if socket.assigns.is_disliked, do: socket.assigns.dislikes - 1, else: socket.assigns.dislikes + 1
    new_likes = if socket.assigns.is_liked, do: socket.assigns.likes - 1, else: socket.assigns.likes

    {:noreply,
     socket
     |> assign(:is_disliked, !socket.assigns.is_disliked)
     |> assign(:is_liked, false)
     |> assign(:likes, new_likes)
     |> assign(:dislikes, new_dislikes)}
  end

  @impl true
  def handle_event("save_video", _params, socket) do
    {:noreply, assign(socket, :is_saved, !socket.assigns.is_saved)}
  end

  @impl true
  def handle_event("subscribe", _params, socket) do
    {:noreply, assign(socket, :is_subscribed, !socket.assigns.is_subscribed)}
  end

  @impl true
  def handle_event("add_comment", %{"comment" => comment_text}, socket) do
    if String.trim(comment_text) != "" do
      new_comment = %{
        id: :erlang.system_time(:millisecond),
        author: socket.assigns.user.username,
        author_avatar: socket.assigns.user.avatar_url,
        text: comment_text,
        timestamp: "Just now",
        likes: 0,
        dislikes: 0
      }

      {:noreply,
       socket
       |> assign(:comments, [new_comment | socket.assigns.comments])
       |> assign(:comment_text, "")}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("update_comment", %{"value" => value}, socket) do
    {:noreply, assign(socket, :comment_text, value)}
  end

  @impl true
  def handle_info({:play_video, id}, socket) do
    {:noreply, push_navigate(socket, to: Routes.live_path(socket, __MODULE__, socket.assigns.locale, id))}
  end

  defp get_video_by_id(video_id) do
    video = %{
      id: video_id,
      title: "Over My Head - A Synthwave",
      thumbnail: "/images/thumbnails/video-1.jpg",
      duration: "11:55",
      views: "986k",
      uploaded_at: "10 months ago",
      creator: "The official Guy",
      creator_avatar: "/images/ellipse-36-10.png",
      subscribers: "65.6k",
      description: "what is your favorite video?\nthis channel is great to get useful infor",
      video_url: "https://example.com/video.mp4",
      likes: 18000,
      dislikes: 200
    }

    {:ok, video}
  end

  defp get_related_videos(current_video_id) do
    [
      %{
        id: 2,
        title: "Over My Head - A Synthwave",
        creator: "Sketch & jam",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-2.jpg",
        duration: "05:43",
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-15.png"
      },
      %{
        id: 3,
        title: "Over My Head - A Synthwave",
        creator: "Sketch & jam",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-3.jpg",
        duration: "05:43",
        is_live: true,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-9.png"
      },
      %{
        id: 4,
        title: "Over My Head - A Synthwave",
        creator: "Sketch & jam",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-4.jpg",
        duration: "05:43",
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-7.png"
      },
      %{
        id: 5,
        title: "Over My Head - A Synthwave",
        creator: "Sketch & jam",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-5.jpg",
        duration: "05:43",
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-6.png"
      },
      %{
        id: 6,
        title: "Over My Head - A Synthwave",
        creator: "Sketch & jam",
        views: "4.7M",
        time_ago: "3 years ago",
        thumbnail: "/images/thumbnails/video-6.jpg",
        duration: "05:43",
        is_live: false,
        chat_count: 8,
        creator_avatar: "/images/ellipse-36-3.png"
      }
    ]
    |> Enum.filter(&(&1.id != String.to_integer(current_video_id)))
    |> Enum.take(8)
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

  defp format_number(num) when num >= 1000 do
    "#{Float.round(num / 1000, 1)}K"
  end

  defp format_number(num), do: to_string(num)
end
