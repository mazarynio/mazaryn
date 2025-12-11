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
    video_id_string = to_string(video_id)
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id_string}")}
  end

  @impl true
  def handle_info({:play_video, video_id}, socket) do
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
      views: format_views(Map.get(video.changes, :views, 0)),
      created_at: format_time_ago(video.changes.date_created),
      creator: get_creator_name(video.changes.user_id),
      creator_avatar: get_creator_avatar(video.changes.user_id),
      is_live: Map.get(video.changes, :is_live, false)
    }
  end

  defp get_thumbnail_url(video) do
    cond do
      Map.has_key?(video, :thumbnail_url) && video.thumbnail_url && video.thumbnail_url != "" ->
        video.thumbnail_url

      Map.has_key?(video, :thumbnail_cid) && video.thumbnail_cid && video.thumbnail_cid != "" ->
        cid =
          if is_list(video.thumbnail_cid),
            do: List.to_string(video.thumbnail_cid),
            else: video.thumbnail_cid

        "https://ipfs.io/ipfs/#{cid}"

      Map.has_key?(video, :thumbnail_cids) && is_list(video.thumbnail_cids) &&
          length(video.thumbnail_cids) > 0 ->
        first_cid = List.first(video.thumbnail_cids)
        cid_string = if is_list(first_cid), do: List.to_string(first_cid), else: first_cid
        "https://ipfs.io/ipfs/#{cid_string}"

      true ->
        "/images/default-video-thumbnail.svg"
    end
  end

  defp format_duration(nil), do: "00:00"
  defp format_duration(0), do: "00:00"
  defp format_duration(0.0), do: "00:00"

  defp format_duration(seconds) when is_float(seconds) do
    format_duration(round(seconds))
  end

  defp format_duration(seconds) when is_integer(seconds) do
    minutes = div(seconds, 60)
    secs = rem(seconds, 60)

    "#{String.pad_leading(Integer.to_string(minutes), 2, "0")}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
  end

  defp format_duration(_), do: "00:00"

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

  defp get_creator_name(user_id) when is_binary(user_id) do
    get_creator_name(String.to_charlist(user_id))
  end

  defp get_creator_name(user_id) do
    Logger.info("===== GET_CREATOR_NAME: Getting name for user: #{inspect(user_id)} =====")

    case Core.UserClient.get_user_by_id(user_id) do
      {:error, reason} ->
        "Unknown"

      user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 35 ->
        username = elem(user_tuple, 8)

        username_str =
          case username do
            u when is_list(u) and length(u) > 0 ->
              str = List.to_string(u)
              if String.starts_with?(str, ["/ip4", "/ip6"]), do: "Unknown", else: str

            u when is_binary(u) and u != "" ->
              if String.starts_with?(u, ["/ip4", "/ip6"]), do: "Unknown", else: u

            _ ->
              "Unknown"
          end

        username_str

      _ ->
        "Unknown"
    end
  end

  defp get_creator_avatar(user_id) when is_binary(user_id) do
    get_creator_avatar(String.to_charlist(user_id))
  end

  defp get_creator_avatar(user_id) do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, reason} ->
        "/images/default-avatar.png"

      user_tuple when is_tuple(user_tuple) and tuple_size(user_tuple) >= 35 ->
        avatar = elem(user_tuple, 26)
        avatar_str = safe_process_avatar(avatar)

        avatar_str

      _ ->
        "/images/default-avatar.png"
    end
  end

  defp safe_list_to_string(list) when is_list(list) do
    try do
      if Enum.all?(list, &is_integer/1) do
        List.to_string(list)
      else
        list
        |> Enum.filter(&is_integer/1)
        |> List.to_string()
      end
    rescue
      _ -> ""
    end
  end

  defp safe_list_to_string(_), do: ""

  defp safe_process_avatar(avatar) do
    try do
      case avatar do
        :undefined ->
          "/images/default-avatar.png"

        a when is_list(a) and length(a) > 0 ->
          str = safe_list_to_string(a)

          if str == "" do
            "/images/default-avatar.png"
          else
            process_avatar_string(str)
          end

        a when is_binary(a) and a != "" ->
          process_avatar_string(a)

        _ ->
          "/images/default-avatar.png"
      end
    rescue
      error ->
        "/images/default-avatar.png"
    end
  end

  defp process_avatar_string(str) when is_binary(str) do
    cond do
      str == "" ->
        "/images/default-avatar.png"

      String.starts_with?(str, ["/ip4", "/ip6"]) ->
        "/images/default-avatar.png"

      String.contains?(str, "ipfs.io/ipfs/") ->
        str

      String.starts_with?(str, "Qm") or String.starts_with?(str, "bafy") ->
        full_url = "https://ipfs.io/ipfs/#{str}"
        full_url

      String.starts_with?(str, ["http://", "https://"]) and not String.contains?(str, "ipfs") ->
        "/images/default-avatar.png"

      String.starts_with?(str, "/") ->
        str

      true ->
        "/images/default-avatar.png"
    end
  end

  defp process_avatar_string(_), do: "/images/default-avatar.png"
end
