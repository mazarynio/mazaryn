defmodule MazarynWeb.MediaLive.Video.Index do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Videos")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:videos, [])
     |> load_videos()}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, socket |> assign(:search_query, query) |> search_videos(query)}
  end

  @impl true
  def handle_event("navigate_to_upload", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/upload")}
  end

  @impl true
  def handle_event("play_video", %{"id" => video_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id}")}
  end

  defp get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(%{"session_uuid" => _session_uuid, "user_id" => user_id})
       when user_id != nil do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(_), do: nil

  defp load_videos(socket) do
    case :videodb.get_public_videos() do
      videos when is_list(videos) ->
        formatted = Enum.map(videos, &format_video/1)
        assign(socket, :videos, formatted)

      _ ->
        assign(socket, :videos, [])
    end
  end

  defp search_videos(socket, query) when query != "" do
    case :videodb.search_videos(query) do
      videos when is_list(videos) ->
        formatted = Enum.map(videos, &format_video/1)
        assign(socket, :videos, formatted)

      _ ->
        assign(socket, :videos, [])
    end
  end

  defp search_videos(socket, _), do: load_videos(socket)

  defp format_video(video_tuple) do
    video = Mazaryn.Schema.Video.erl_changeset(video_tuple)

    %{
      id: video.changes.id,
      title: video.changes.title || "Untitled",
      thumbnail_url: get_thumbnail_url(video.changes),
      duration: format_duration(video.changes.duration_seconds),
      views: format_views(video.changes.views),
      created_at: format_time_ago(video.changes.date_created),
      creator: get_creator_name(video.changes.user_id),
      creator_avatar: get_creator_avatar(video.changes.user_id),
      is_live: video.changes.is_live || false
    }
  end

  defp get_thumbnail_url(video) do
    cond do
      video.thumbnail_url ->
        video.thumbnail_url

      is_list(video.thumbnail_cids) and length(video.thumbnail_cids) > 0 ->
        "https://ipfs.io/ipfs/#{List.first(video.thumbnail_cids)}"

      true ->
        "/images/default-thumbnail.jpg"
    end
  end

  defp format_duration(nil), do: nil

  defp format_duration(seconds) when is_integer(seconds) do
    minutes = div(seconds, 60)
    secs = rem(seconds, 60)

    "#{String.pad_leading(Integer.to_string(minutes), 2, "0")}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
  end

  defp format_duration(_), do: nil

  defp format_views(views) when is_integer(views) and views >= 1_000_000 do
    "#{Float.round(views / 1_000_000, 1)}M"
  end

  defp format_views(views) when is_integer(views) and views >= 1_000 do
    "#{Float.round(views / 1_000, 1)}K"
  end

  defp format_views(views) when is_integer(views), do: Integer.to_string(views)
  defp format_views(_), do: "0"

  defp format_time_ago(nil), do: "Just now"

  defp format_time_ago(datetime) do
    now = NaiveDateTime.utc_now()
    diff_seconds = NaiveDateTime.diff(now, datetime)

    cond do
      diff_seconds < 60 -> "Just now"
      diff_seconds < 3600 -> "#{div(diff_seconds, 60)} minutes ago"
      diff_seconds < 86400 -> "#{div(diff_seconds, 3600)} hours ago"
      diff_seconds < 604_800 -> "#{div(diff_seconds, 86400)} days ago"
      true -> "#{div(diff_seconds, 604_800)} weeks ago"
    end
  end

  defp get_creator_name(user_id) do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} ->
        "Unknown"

      user_tuple when is_tuple(user_tuple) ->
        case elem(user_tuple, 2) do
          username when is_binary(username) -> username
          username when is_list(username) -> List.to_string(username)
          _ -> "Unknown"
        end

      _ ->
        "Unknown"
    end
  end

  defp get_creator_avatar(user_id) do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} ->
        "/images/default-avatar.png"

      user_tuple when is_tuple(user_tuple) ->
        avatar_url = elem(user_tuple, 6)
        if avatar_url && avatar_url != "", do: avatar_url, else: "/images/default-avatar.png"

      _ ->
        "/images/default-avatar.png"
    end
  end
end
