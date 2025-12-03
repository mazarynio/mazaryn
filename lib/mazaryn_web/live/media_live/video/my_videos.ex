defmodule MazarynWeb.MediaLive.Video.MyVideos do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "My Videos")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:active_tab, "all")
     |> assign(:videos, [])
     |> assign(:uploaded_videos, [])
     |> assign(:live_streams, [])
     |> assign(:scheduled, [])
     |> load_user_videos()}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, :active_tab, tab)}
  end

  @impl true
  def handle_event("delete_video", %{"id" => video_id}, socket) do
    user_id = get_user_id(socket.assigns.user)

    case :videodb.delete_video(video_id, user_id) do
      {:ok, _} ->
        {:noreply,
         socket
         |> put_flash(:info, "Video deleted successfully")
         |> load_user_videos()}

      _ ->
        {:noreply, put_flash(socket, :error, "Failed to delete video")}
    end
  end

  @impl true
  def handle_event("edit_video", %{"id" => video_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id}/edit")}
  end

  @impl true
  def handle_event("view_video", %{"id" => video_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/#{video_id}")}
  end

  @impl true
  def handle_event("navigate_to_upload", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/videos/upload")}
  end

  defp video_card(assigns) do
    ~H"""
    <div class="p-5 bg-[#323340] rounded-[20px] shadow-lg">
      <div class="flex gap-5">
        <div class="relative">
          <img
            src={@video.thumbnail_url}
            class="w-64 h-44 rounded-md object-cover cursor-pointer hover:opacity-80 transition-opacity"
            alt={@video.title}
            phx-click="view_video"
            phx-value-id={@video.id}
          />
          <%= if @video.is_live do %>
            <span class="absolute bottom-2 right-2 px-2 py-1 bg-red-600 text-white text-xs font-bold rounded">
              LIVE
            </span>
          <% else %>
            <%= if @video.status == "processing" do %>
              <span class="absolute bottom-2 right-2 px-2 py-1 bg-yellow-600 text-white text-xs font-bold rounded">
                PROCESSING
              </span>
            <% end %>
            <%= if @video.status == "failed" do %>
              <span class="absolute bottom-2 right-2 px-2 py-1 bg-red-600 text-white text-xs font-bold rounded">
                FAILED
              </span>
            <% end %>
          <% end %>
        </div>

        <div class="flex-1 space-y-3">
          <div>
            <h3
              class="text-white text-lg font-semibold font-['Poppins'] cursor-pointer hover:text-blue-500 transition-colors"
              phx-click="view_video"
              phx-value-id={@video.id}
            >
              <%= @video.title %>
            </h3>
            <p class="text-neutral-400 text-sm font-['Poppins'] mt-1"><%= @video.url_slug %></p>
          </div>

          <div class="p-3 bg-[#27282e] rounded-lg">
            <p class="text-gray-300 text-sm font-['Poppins'] line-clamp-2">
              <%= @video.description %>
            </p>
          </div>

          <div class="flex items-center justify-between">
            <div class="flex items-center gap-4 text-gray-400 text-sm">
              <span><%= @video.views %> views</span>
              <span>•</span>
              <span><%= @video.created_at %></span>
            </div>

            <div class="flex gap-2">
              <button
                phx-click="edit_video"
                phx-value-id={@video.id}
                class="p-2 rounded-lg bg-[#27282e] hover:bg-blue-500 transition-colors group"
                title="Edit video"
              >
                <Heroicons.pencil class="w-5 h-5 text-gray-400 group-hover:text-white" />
              </button>

              <button
                phx-click="delete_video"
                phx-value-id={@video.id}
                data-confirm="Are you sure you want to delete this video?"
                class="p-2 rounded-lg bg-[#27282e] hover:bg-red-500 transition-colors group"
                title="Delete video"
              >
                <Heroicons.trash class="w-5 h-5 text-gray-400 group-hover:text-white" />
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
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

  defp get_user_id(user_tuple) when is_tuple(user_tuple) do
    elem(user_tuple, 1)
  end

  defp get_user_id(_), do: nil

  defp load_user_videos(socket) do
    user_id = get_user_id(socket.assigns.user)

    case :videodb.get_videos_by_creator(user_id) do
      videos when is_list(videos) ->
        formatted_videos = Enum.map(videos, &format_video/1)

        all_videos = formatted_videos

        uploaded_videos =
          Enum.filter(formatted_videos, &(&1.status in ["ready", "processing", "failed"]))

        live_streams = Enum.filter(formatted_videos, & &1.is_live)
        scheduled = Enum.filter(formatted_videos, &(&1.visibility == "scheduled"))

        socket
        |> assign(:videos, all_videos)
        |> assign(:uploaded_videos, uploaded_videos)
        |> assign(:live_streams, live_streams)
        |> assign(:scheduled, scheduled)

      _ ->
        socket
        |> assign(:videos, [])
        |> assign(:uploaded_videos, [])
        |> assign(:live_streams, [])
        |> assign(:scheduled, [])
    end
  end

  defp format_video(video_tuple) do
    video = Mazaryn.Schema.Video.erl_changeset(video_tuple)

    %{
      id: video.changes.id,
      title: video.changes.title || "Untitled",
      description: video.changes.description || "No description available",
      thumbnail_url: get_thumbnail_url(video.changes),
      url_slug: video.changes.file_url || "mazaryn.xyz://#{video.changes.id}",
      duration: format_duration(video.changes.duration_seconds),
      views: video.changes.views || 0,
      status: Atom.to_string(video.changes.status || :ready),
      visibility: Atom.to_string(video.changes.privacy || :public),
      created_at: format_time_ago(video.changes.date_created),
      is_live: video.changes.is_live || false,
      upload_progress: get_upload_progress(video.changes)
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

  defp get_upload_progress(video) do
    case video.status do
      :processing -> 50
      :ready -> 100
      :failed -> 0
      _ -> 0
    end
  end
end
