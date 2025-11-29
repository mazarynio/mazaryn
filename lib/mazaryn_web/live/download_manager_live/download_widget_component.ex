defmodule MazarynWeb.DownloadWidgetComponent do
  use MazarynWeb, :live_component
  require Logger

  @impl true
  def mount(socket) do
    if connected?(socket) do
      :timer.send_interval(2000, self(), :refresh_widget)
    end
    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(assigns)
      |> assign_new(:show_widget, fn -> true end)
      |> assign_new(:position, fn -> "bottom-right" end)
      |> assign_new(:downloads, fn -> load_active_downloads(assigns[:user]) end)

    {:ok, socket}
  end

  @impl true
  def handle_event("toggle_widget", _params, socket) do
    {:noreply, assign(socket, show_widget: !socket.assigns.show_widget)}
  end

  @impl true
  def handle_event("pause_download", %{"id" => download_id}, socket) do
    :download_manager_client.pause_download(download_id)
    downloads = load_active_downloads(socket.assigns.user)
    {:noreply, assign(socket, downloads: downloads)}
  end

  @impl true
  def handle_event("resume_download", %{"id" => download_id}, socket) do
    :download_manager_client.resume_download(download_id)
    downloads = load_active_downloads(socket.assigns.user)
    {:noreply, assign(socket, downloads: downloads)}
  end

  @impl true
  def handle_event("cancel_download", %{"id" => download_id}, socket) do
    :download_manager_client.cancel_download(download_id)
    downloads = load_active_downloads(socket.assigns.user)
    {:noreply, assign(socket, downloads: downloads)}
  end

  @impl true
  def handle_event("view_all_downloads", _params, socket) do
    send(self(), {:redirect, "/#{socket.assigns.locale}/downloads"})
    {:noreply, socket}
  end

  defp load_active_downloads(user) do
    user_id = to_string(user.id)

    case :download_manager_client.list_user_downloads(user_id) do
      {:ok, downloads} when is_list(downloads) ->
        downloads
        |> Enum.map(&parse_download/1)
        |> Enum.filter(fn d -> d.status in ["Downloading", "Pending", "Queued", "Paused"] end)
        |> Enum.take(5)

      _ ->
        []
    end
  rescue
    _ -> []
  end

  @impl true
  def handle_info(:refresh_widget, socket) do
    downloads = load_active_downloads(socket.assigns.user)
    {:noreply, assign(socket, downloads: downloads)}
  end

  defp parse_download(download_map) when is_map(download_map) do
    %{
      id: binary_to_string(Map.get(download_map, :id)),
      destination: binary_to_string(Map.get(download_map, :destination)),
      status: binary_to_string(Map.get(download_map, :status)),
      progress_percentage: Map.get(download_map, :progress_percentage, 0.0),
      speed_bps: Map.get(download_map, :speed_bps, 0),
      downloaded_size: Map.get(download_map, :downloaded_size, 0),
      total_size: Map.get(download_map, :total_size),
      dataset_id: binary_to_string(Map.get(download_map, :dataset_id))
    }
  end

  defp binary_to_string(nil), do: nil
  defp binary_to_string(:null), do: nil
  defp binary_to_string(value) when is_binary(value), do: value
  defp binary_to_string(value) when is_list(value), do: to_string(value)
  defp binary_to_string(value), do: inspect(value)

  defp filename_from_destination(destination) when is_binary(destination) do
    Path.basename(destination)
  end

  defp filename_from_destination(_), do: "Unknown"

  defp format_bytes(nil), do: "N/A"

  defp format_bytes(bytes) when is_integer(bytes) and bytes >= 0 do
    cond do
      bytes >= 1_073_741_824 ->
        "#{Float.round(bytes / 1_073_741_824, 1)} GB"

      bytes >= 1_048_576 ->
        "#{Float.round(bytes / 1_048_576, 1)} MB"

      bytes >= 1024 ->
        "#{Float.round(bytes / 1024, 1)} KB"

      true ->
        "#{bytes} B"
    end
  end

  defp format_bytes(_), do: "N/A"

  defp format_speed(speed_bps) when is_integer(speed_bps) and speed_bps > 0 do
    format_bytes(speed_bps) <> "/s"
  end

  defp format_speed(_), do: "N/A"

  defp position_classes("bottom-right"), do: "bottom-6 right-6"
  defp position_classes("bottom-left"), do: "bottom-6 left-6"
  defp position_classes("top-right"), do: "top-6 right-6"
  defp position_classes("top-left"), do: "top-6 left-6"
  defp position_classes(_), do: "bottom-6 right-6"

  defp download_status_class("Completed"), do: "bg-green-100 text-green-700"
  defp download_status_class("Downloading"), do: "bg-blue-100 text-blue-700"
  defp download_status_class("Paused"), do: "bg-yellow-100 text-yellow-700"
  defp download_status_class("Failed"), do: "bg-red-100 text-red-700"
  defp download_status_class("Queued"), do: "bg-gray-100 text-gray-700"
  defp download_status_class(_), do: "bg-gray-100 text-gray-700"

  defp format_eta(nil), do: ""
  defp format_eta(seconds) when seconds < 60, do: "#{seconds}s"
  defp format_eta(seconds) when seconds < 3600 do
    minutes = div(seconds, 60)
    "#{minutes}m"
  end
  defp format_eta(seconds) do
    hours = div(seconds, 3600)
    minutes = div(rem(seconds, 3600), 60)
    "#{hours}h #{minutes}m"
  end

  defp format_eta(nil), do: ""
  defp format_eta(seconds) when is_integer(seconds) and seconds < 60, do: "#{seconds}s"
  defp format_eta(seconds) when is_integer(seconds) and seconds < 3600 do
    minutes = div(seconds, 60)
    "#{minutes}m"
  end
  defp format_eta(seconds) when is_integer(seconds) do
    hours = div(seconds, 3600)
    minutes = div(rem(seconds, 3600), 60)
    "#{hours}h #{minutes}m"
  end
  defp format_eta(_), do: ""
end
