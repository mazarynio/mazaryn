defmodule MazarynWeb.MediaLive.Video.Show do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(%{"id" => video_id}, session, socket) do
    user = get_user_from_session(session)

    case load_video(video_id) do
      {:ok, video} ->
        {:ok,
         socket
         |> assign(:page_title, video.title)
         |> assign(:user, user)
         |> assign(:current_user, user)
         |> assign(:locale, socket.assigns[:locale] || "en")
         |> assign(:video, video)
         |> assign(:video_id, video_id)
         |> assign(:related_videos, [])
         |> assign(:comments, [])
         |> assign(:comment_text, "")
         |> assign(:is_liked, false)
         |> assign(:is_disliked, false)
         |> assign(:is_saved, false)
         |> assign(:is_subscribed, false)
         |> load_related_videos()
         |> load_comments()
         |> track_view()}

      {:error, _} ->
        {:ok,
         socket
         |> put_flash(:error, "Video not found")
         |> push_navigate(to: ~p"/#{socket.assigns[:locale] || "en"}/videos")}
    end
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("like_video", _params, socket) do
    video_id = socket.assigns.video_id
    user_id = socket.assigns.user.id

    if socket.assigns.is_liked do
      :videodb.remove_reaction(video_id, user_id)
      {:noreply, assign(socket, :is_liked, false)}
    else
      :videodb.react_to_video(video_id, user_id, :like)
      {:noreply, socket |> assign(:is_liked, true) |> assign(:is_disliked, false)}
    end
  end

  @impl true
  def handle_event("dislike_video", _params, socket) do
    video_id = socket.assigns.video_id
    user_id = socket.assigns.user.id

    if socket.assigns.is_disliked do
      :videodb.remove_reaction(video_id, user_id)
      {:noreply, assign(socket, :is_disliked, false)}
    else
      :videodb.react_to_video(video_id, user_id, :dislike)
      {:noreply, socket |> assign(:is_disliked, true) |> assign(:is_liked, false)}
    end
  end

  @impl true
  def handle_event("save_video", _params, socket) do
    video_id = socket.assigns.video_id
    user_id = socket.assigns.user.id

    if socket.assigns.is_saved do
      :videodb.unsave_video(video_id, user_id)
      {:noreply, assign(socket, :is_saved, false)}
    else
      :videodb.save_video(video_id, user_id)
      {:noreply, assign(socket, :is_saved, true)}
    end
  end

  @impl true
  def handle_event("share_video", _params, socket) do
    video_id = socket.assigns.video_id
    user_id = socket.assigns.user.id
    :videodb.share_video(video_id, user_id)
    {:noreply, put_flash(socket, :info, "Video shared!")}
  end

  @impl true
  def handle_event("subscribe", _params, socket) do
    creator_id = socket.assigns.video.creator_id
    user_id = socket.assigns.user.id

    if socket.assigns.is_subscribed do
      :userdb.unsubscribe(user_id, creator_id)
      {:noreply, assign(socket, :is_subscribed, false)}
    else
      :userdb.subscribe(user_id, creator_id)
      {:noreply, assign(socket, :is_subscribed, true)}
    end
  end

  @impl true
  def handle_event("add_comment", %{"comment" => comment_text}, socket) do
    if String.trim(comment_text) != "" do
      video_id = socket.assigns.video_id
      user_id = socket.assigns.user.id

      case :videodb.add_comment(video_id, user_id, comment_text) do
        {:ok, _comment_id} ->
          {:noreply,
           socket
           |> assign(:comment_text, "")
           |> load_comments()
           |> put_flash(:info, "Comment added!")}

        _ ->
          {:noreply, put_flash(socket, :error, "Failed to add comment")}
      end
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("play_related_video", %{"id" => video_id}, socket) do
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

  defp load_video(video_id) do
    case :videodb.get_video(video_id) do
      {:ok, video_tuple} ->
        video = Mazaryn.Schema.Video.erl_changeset(video_tuple)

        {:ok,
         %{
           id: video.changes.id,
           title: video.changes.title || "Untitled",
           description: video.changes.description || "",
           thumbnail_url: get_thumbnail_url(video.changes),
           video_url: get_video_url(video.changes),
           duration: format_duration(video.changes.duration_seconds),
           views: format_views(video.changes.views),
           created_at: format_time_ago(video.changes.date_created),
           creator_id: video.changes.user_id,
           creator: get_creator_name(video.changes.user_id),
           creator_avatar: get_creator_avatar(video.changes.user_id),
           subscribers: get_subscriber_count(video.changes.user_id),
           likes: get_reaction_count(video.changes, :like),
           dislikes: get_reaction_count(video.changes, :dislike)
         }}

      _ ->
        {:error, :not_found}
    end
  end

  defp load_related_videos(socket) do
    video_id = socket.assigns.video_id

    case :videodb.get_public_videos() do
      videos when is_list(videos) ->
        related =
          videos
          |> Enum.reject(fn v -> elem(v, 1) == video_id end)
          |> Enum.take(8)
          |> Enum.map(&format_related_video/1)

        assign(socket, :related_videos, related)

      _ ->
        assign(socket, :related_videos, [])
    end
  end

  defp load_comments(socket) do
    video_id = socket.assigns.video_id

    case :videodb.get_comments(video_id) do
      comments when is_list(comments) ->
        formatted = Enum.map(comments, &format_comment/1)
        assign(socket, :comments, formatted)

      _ ->
        assign(socket, :comments, [])
    end
  end

  defp track_view(socket) do
    if socket.assigns.user do
      video_id = socket.assigns.video_id
      user_id = socket.assigns.user.id
      :videodb.increment_unique_view(video_id, user_id)
    end

    socket
  end

  defp format_related_video(video_tuple) do
    video = Mazaryn.Schema.Video.erl_changeset(video_tuple)

    %{
      id: video.changes.id,
      title: video.changes.title || "Untitled",
      thumbnail_url: get_thumbnail_url(video.changes),
      duration: format_duration(video.changes.duration_seconds),
      views: format_views(video.changes.views),
      created_at: format_time_ago(video.changes.date_created),
      creator: get_creator_name(video.changes.user_id)
    }
  end

  defp format_comment(comment_tuple) do
    %{
      id: elem(comment_tuple, 1),
      author_id: elem(comment_tuple, 3),
      author: get_creator_name(elem(comment_tuple, 3)),
      author_avatar: get_creator_avatar(elem(comment_tuple, 3)),
      text: elem(comment_tuple, 4),
      created_at: format_time_ago(elem(comment_tuple, 6)),
      likes: 0
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

  defp get_video_url(video) do
    cond do
      video.file_url -> video.file_url
      video.ipfs_cid -> "https://ipfs.io/ipfs/#{video.ipfs_cid}"
      true -> nil
    end
  end

  defp format_duration(nil), do: "0:00"

  defp format_duration(seconds) when is_integer(seconds) do
    hours = div(seconds, 3600)
    minutes = div(rem(seconds, 3600), 60)
    secs = rem(seconds, 60)

    if hours > 0 do
      "#{hours}:#{String.pad_leading(Integer.to_string(minutes), 2, "0")}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
    else
      "#{minutes}:#{String.pad_leading(Integer.to_string(secs), 2, "0")}"
    end
  end

  defp format_duration(_), do: "0:00"

  defp format_views(views) when is_integer(views) and views >= 1_000_000 do
    "#{Float.round(views / 1_000_000, 1)}M"
  end

  defp format_views(views) when is_integer(views) and views >= 1_000 do
    "#{Float.round(views / 1_000, 1)}K"
  end

  defp format_views(views) when is_integer(views), do: Integer.to_string(views)
  defp format_views(_), do: "0"

  defp format_number(number) when is_integer(number) and number >= 1_000_000 do
    "#{Float.round(number / 1_000_000, 1)}M"
  end

  defp format_number(number) when is_integer(number) and number >= 1_000 do
    "#{Float.round(number / 1_000, 1)}K"
  end

  defp format_number(number) when is_integer(number), do: Integer.to_string(number)
  defp format_number(_), do: "0"

  defp format_time_ago(nil), do: "Just now"

  defp format_time_ago(datetime) do
    now = NaiveDateTime.utc_now()
    diff_seconds = NaiveDateTime.diff(now, datetime)

    cond do
      diff_seconds < 60 -> "Just now"
      diff_seconds < 3600 -> "#{div(diff_seconds, 60)} minutes ago"
      diff_seconds < 86400 -> "#{div(diff_seconds, 3600)} hours ago"
      diff_seconds < 604_800 -> "#{div(diff_seconds, 86400)} days ago"
      diff_seconds < 2_592_000 -> "#{div(diff_seconds, 604_800)} weeks ago"
      diff_seconds < 31_536_000 -> "#{div(diff_seconds, 2_592_000)} months ago"
      true -> "#{div(diff_seconds, 31_536_000)} years ago"
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

  defp get_subscriber_count(user_id) do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} ->
        "0"

      user_tuple when is_tuple(user_tuple) ->
        followers = elem(user_tuple, 10)
        if is_list(followers), do: format_views(length(followers)), else: "0"

      _ ->
        "0"
    end
  end

  defp get_reaction_count(video, type) do
    if is_map(video.reaction_counts) do
      Map.get(video.reaction_counts, Atom.to_string(type), 0)
    else
      0
    end
  end
end
