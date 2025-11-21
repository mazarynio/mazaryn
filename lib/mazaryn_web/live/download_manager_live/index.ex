defmodule MazarynWeb.DownloadManagerLive.Index do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      if connected?(socket) do
        :timer.send_interval(2000, self(), :refresh_downloads)
      end

      socket =
        socket
        |> assign(user: user)
        |> assign(session_uuid: session_uuid)
        |> assign(downloads: load_user_downloads(user))
        |> assign(active_filter: "all")
        |> assign(search_query: "")
        |> assign(sort_by: "recent")
        |> assign(selected_downloads: [])
        |> assign(show_details_modal: false)
        |> assign(selected_download: nil)
        |> assign(locale: "en")

      {:ok, socket}
    else
      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        if connected?(socket) do
          :timer.send_interval(2000, self(), :refresh_downloads)
        end

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(downloads: load_user_downloads(user))
          |> assign(active_filter: "all")
          |> assign(search_query: "")
          |> assign(sort_by: "recent")
          |> assign(selected_downloads: [])
          |> assign(show_details_modal: false)
          |> assign(selected_download: nil)
          |> assign(locale: "en")

        {:ok, socket}

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_params(_params, url, socket) do
    {:noreply, assign(socket, current_path: URI.parse(url).path)}
  end

  @impl true
  def handle_event("filter_change", %{"filter" => filter}, socket) do
    filtered = filter_downloads(socket.assigns.downloads, filter, socket.assigns.search_query)
    sorted = sort_downloads(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, active_filter: filter, downloads: sorted)}
  end

  @impl true
  def handle_event("search", %{"value" => query}, socket) do
    {:noreply, assign(socket, search_query: query)}
  end

  @impl true
  def handle_event("sort_change", %{"sort" => sort}, socket) do
    sorted = sort_downloads(socket.assigns.downloads, sort)
    {:noreply, assign(socket, sort_by: sort, downloads: sorted)}
  end

  @impl true
  def handle_event("pause_download", %{"id" => download_id}, socket) do
    case :download_manager_client.pause_download(download_id) do
      :ok ->
        downloads = refresh_download_status(socket.assigns.downloads, download_id)
        {:noreply, assign(socket, downloads: downloads) |> put_flash(:info, "Download paused")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to pause: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("resume_download", %{"id" => download_id}, socket) do
    case :download_manager_client.resume_download(download_id) do
      :ok ->
        downloads = refresh_download_status(socket.assigns.downloads, download_id)
        {:noreply, assign(socket, downloads: downloads) |> put_flash(:info, "Download resumed")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to resume: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("cancel_download", %{"id" => download_id}, socket) do
    case :download_manager_client.cancel_download(download_id) do
      :ok ->
        downloads = Enum.reject(socket.assigns.downloads, fn d -> d.id == download_id end)
        {:noreply, assign(socket, downloads: downloads) |> put_flash(:info, "Download cancelled")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to cancel: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("delete_download", %{"id" => download_id}, socket) do
    case :download_manager_client.delete_download(download_id) do
      :ok ->
        downloads = Enum.reject(socket.assigns.downloads, fn d -> d.id == download_id end)
        {:noreply, assign(socket, downloads: downloads) |> put_flash(:info, "Download deleted")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to delete: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("toggle_select", %{"id" => download_id}, socket) do
    selected = socket.assigns.selected_downloads

    updated_selected =
      if Enum.member?(selected, download_id) do
        List.delete(selected, download_id)
      else
        [download_id | selected]
      end

    {:noreply, assign(socket, selected_downloads: updated_selected)}
  end

  @impl true
  def handle_event("select_all", _params, socket) do
    all_ids = Enum.map(socket.assigns.downloads, & &1.id)
    {:noreply, assign(socket, selected_downloads: all_ids)}
  end

  @impl true
  def handle_event("deselect_all", _params, socket) do
    {:noreply, assign(socket, selected_downloads: [])}
  end

  @impl true
  def handle_event("bulk_pause", _params, socket) do
    Enum.each(socket.assigns.selected_downloads, fn id ->
      :download_manager_client.pause_download(id)
    end)

    {:noreply, assign(socket, selected_downloads: []) |> put_flash(:info, "Downloads paused")}
  end

  @impl true
  def handle_event("bulk_cancel", _params, socket) do
    Enum.each(socket.assigns.selected_downloads, fn id ->
      :download_manager_client.cancel_download(id)
    end)

    downloads =
      Enum.reject(socket.assigns.downloads, fn d ->
        Enum.member?(socket.assigns.selected_downloads, d.id)
      end)

    {:noreply,
     assign(socket, downloads: downloads, selected_downloads: [])
     |> put_flash(:info, "Downloads cancelled")}
  end

  @impl true
  def handle_event("bulk_delete", _params, socket) do
    Enum.each(socket.assigns.selected_downloads, fn id ->
      :download_manager_client.delete_download(id)
    end)

    downloads =
      Enum.reject(socket.assigns.downloads, fn d ->
        Enum.member?(socket.assigns.selected_downloads, d.id)
      end)

    {:noreply,
     assign(socket, downloads: downloads, selected_downloads: [])
     |> put_flash(:info, "Downloads deleted")}
  end

  @impl true
  def handle_event("show_details", %{"id" => download_id}, socket) do
    download = Enum.find(socket.assigns.downloads, &(&1.id == download_id))
    {:noreply, assign(socket, show_details_modal: true, selected_download: download)}
  end

  @impl true
  def handle_event("close_details_modal", _params, socket) do
    {:noreply, assign(socket, show_details_modal: false, selected_download: nil)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_info(:refresh_downloads, socket) do
    downloads = load_user_downloads(socket.assigns.user)
    {:noreply, assign(socket, downloads: downloads)}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Received message: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp load_user_downloads(user) do
    user_id = to_string(user.id)

    case :download_manager_client.list_user_downloads(user_id) do
      {:ok, downloads} when is_list(downloads) ->
        Enum.map(downloads, &parse_download/1)

      _ ->
        []
    end
  rescue
    _ -> []
  end

  defp parse_download(download_map) when is_map(download_map) do
    %{
      id: binary_to_string(Map.get(download_map, :id)),
      url: binary_to_string(Map.get(download_map, :url)),
      destination: binary_to_string(Map.get(download_map, :destination)),
      status: binary_to_string(Map.get(download_map, :status)),
      progress_percentage: Map.get(download_map, :progress_percentage, 0.0),
      speed_bps: Map.get(download_map, :speed_bps, 0),
      eta_seconds: Map.get(download_map, :eta_seconds),
      downloaded_size: Map.get(download_map, :downloaded_size, 0),
      total_size: Map.get(download_map, :total_size),
      user_id: binary_to_string(Map.get(download_map, :user_id)),
      dataset_id: binary_to_string(Map.get(download_map, :dataset_id)),
      competition_id: binary_to_string(Map.get(download_map, :competition_id)),
      error: binary_to_string(Map.get(download_map, :error))
    }
  end

  defp binary_to_string(nil), do: nil
  defp binary_to_string(:null), do: nil
  defp binary_to_string(value) when is_binary(value), do: value
  defp binary_to_string(value) when is_list(value), do: to_string(value)
  defp binary_to_string(value), do: inspect(value)

  defp filter_downloads(downloads, "all", query), do: search_downloads(downloads, query)

  defp filter_downloads(downloads, "active", query) do
    downloads
    |> Enum.filter(fn d -> d.status in ["Downloading", "Pending", "Queued"] end)
    |> search_downloads(query)
  end

  defp filter_downloads(downloads, "completed", query) do
    downloads
    |> Enum.filter(fn d -> d.status == "Completed" end)
    |> search_downloads(query)
  end

  defp filter_downloads(downloads, "paused", query) do
    downloads
    |> Enum.filter(fn d -> d.status == "Paused" end)
    |> search_downloads(query)
  end

  defp filter_downloads(downloads, "failed", query) do
    downloads
    |> Enum.filter(fn d -> d.status == "Failed" end)
    |> search_downloads(query)
  end

  defp filter_downloads(downloads, _, query), do: search_downloads(downloads, query)

  defp search_downloads(downloads, ""), do: downloads

  defp search_downloads(downloads, query) do
    query_lower = String.downcase(query)

    Enum.filter(downloads, fn download ->
      destination_match =
        download.destination && String.contains?(String.downcase(download.destination), query_lower)

      url_match = download.url && String.contains?(String.downcase(download.url), query_lower)

      dataset_match =
        download.dataset_id && String.contains?(String.downcase(download.dataset_id || ""), query_lower)

      destination_match || url_match || dataset_match
    end)
  end

  defp sort_downloads(downloads, "recent") do
    Enum.sort_by(downloads, & &1.id, :desc)
  end

  defp sort_downloads(downloads, "progress") do
    Enum.sort_by(downloads, & &1.progress_percentage, :desc)
  end

  defp sort_downloads(downloads, "size") do
    Enum.sort_by(downloads, & &1.total_size || 0, :desc)
  end

  defp sort_downloads(downloads, "speed") do
    Enum.sort_by(downloads, & &1.speed_bps, :desc)
  end

  defp sort_downloads(downloads, _), do: downloads

  defp refresh_download_status(downloads, download_id) do
    case :download_manager_client.get_download_status(download_id) do
      {:ok, updated_info} ->
        Enum.map(downloads, fn d ->
          if d.id == download_id do
            parse_download(updated_info)
          else
            d
          end
        end)

      _ ->
        downloads
    end
  end

  def format_bytes(nil), do: "N/A"
  def format_bytes(bytes) when is_integer(bytes) and bytes >= 0 do
    cond do
      bytes >= 1_073_741_824 ->
        "#{Float.round(bytes / 1_073_741_824, 2)} GB"

      bytes >= 1_048_576 ->
        "#{Float.round(bytes / 1_048_576, 2)} MB"

      bytes >= 1024 ->
        "#{Float.round(bytes / 1024, 2)} KB"

      true ->
        "#{bytes} B"
    end
  end

  def format_bytes(_), do: "N/A"

  def format_speed(speed_bps) when is_integer(speed_bps) and speed_bps > 0 do
    format_bytes(speed_bps) <> "/s"
  end

  def format_speed(_), do: "N/A"

  def format_eta(nil), do: "N/A"
  def format_eta(:null), do: "N/A"

  def format_eta(seconds) when is_integer(seconds) do
    cond do
      seconds >= 3600 ->
        hours = div(seconds, 3600)
        minutes = div(rem(seconds, 3600), 60)
        "#{hours}h #{minutes}m"

      seconds >= 60 ->
        minutes = div(seconds, 60)
        secs = rem(seconds, 60)
        "#{minutes}m #{secs}s"

      true ->
        "#{seconds}s"
    end
  end

  def format_eta(_), do: "N/A"

  def status_color("Downloading"), do: "blue"
  def status_color("Completed"), do: "green"
  def status_color("Failed"), do: "red"
  def status_color("Paused"), do: "yellow"
  def status_color("Pending"), do: "purple"
  def status_color("Queued"), do: "indigo"
  def status_color("Cancelled"), do: "slate"
  def status_color(_), do: "slate"

  def status_icon("Downloading") do
    ~s(<svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"/></svg>)
  end

  def status_icon("Completed") do
    ~s(<svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/></svg>)
  end

  def status_icon("Failed") do
    ~s(<svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"/></svg>)
  end

  def status_icon("Paused") do
    ~s(<svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 9v6m4-6v6m7-3a9 9 0 11-18 0 9 9 0 0118 0z"/></svg>)
  end

  def status_icon(_) do
    ~s(<svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"/></svg>)
  end

  def filename_from_destination(destination) when is_binary(destination) do
    Path.basename(destination)
  end

  def filename_from_destination(_), do: "Unknown"
end
