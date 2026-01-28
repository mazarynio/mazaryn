defmodule MazarynWeb.AiLive.DatasetShow do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.DatasetClient
  alias Mazaryn.Schema.Dataset
  require Logger

  @impl true
  def mount(%{"id" => dataset_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      case load_dataset(dataset_id) do
        {:ok, dataset} ->
          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(dataset: dataset)
            |> assign(dataset_id: dataset_id)
            |> assign(loading: false)
            |> assign(show_delete_modal: false)
            |> assign(user_rating: get_user_rating(dataset, user.id))
            |> assign(active_tab: "overview")
            |> assign(locale: "en")
            |> assign(search_query: "")
            |> assign(filter: "all")
            |> assign(sort_by: "recent")

          {:ok, socket}

        {:error, reason} ->
          Logger.error("Failed to load dataset #{dataset_id}: #{inspect(reason)}")

          {:ok,
           socket
           |> put_flash(:error, "Dataset not found")
           |> redirect(to: "/en/ai/datasets")}
      end
    else
      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(%{"id" => dataset_id}, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        case load_dataset(dataset_id) do
          {:ok, dataset} ->
            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(dataset: dataset)
              |> assign(dataset_id: dataset_id)
              |> assign(loading: false)
              |> assign(show_delete_modal: false)
              |> assign(user_rating: get_user_rating(dataset, user.id))
              |> assign(active_tab: "overview")
              |> assign(locale: "en")
              |> assign(search_query: "")
              |> assign(filter: "all")
              |> assign(sort_by: "recent")

            {:ok, socket}

          {:error, reason} ->
            Logger.error("Failed to load dataset #{dataset_id}: #{inspect(reason)}")

            {:ok,
             socket
             |> put_flash(:error, "Dataset not found")
             |> redirect(to: "/en/ai/datasets")}
        end

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
  def handle_event("change_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, active_tab: tab)}
  end

  @impl true
  def handle_event("rate_dataset", %{"rating" => rating}, socket) do
    user_id = to_string(socket.assigns.user.id)
    dataset_id = socket.assigns.dataset_id
    rating_int = String.to_integer(rating)

    user_id_charlist = String.to_charlist(user_id)
    dataset_id_charlist = String.to_charlist(dataset_id)

    case DatasetClient.rate_dataset(dataset_id_charlist, user_id_charlist, rating_int) do
      :ok ->
        case load_dataset(dataset_id) do
          {:ok, dataset} ->
            {:noreply,
             socket
             |> assign(dataset: dataset)
             |> assign(user_rating: rating_int)
             |> put_flash(:info, "Rating submitted successfully")}

          _ ->
            {:noreply,
             socket
             |> assign(user_rating: rating_int)
             |> put_flash(:info, "Rating submitted successfully")}
        end

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to rate: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("download_dataset", _params, socket) do
    user_id = to_string(socket.assigns.user.id)
    dataset_id = socket.assigns.dataset_id

    IO.puts("=== DOWNLOAD DATASET DEBUG (SHOW PAGE) ===")
    IO.puts("Dataset ID: #{dataset_id}")
    IO.puts("User ID: #{user_id}")

    destination_dir = "/tmp/mazaryn_downloads/#{user_id}"

    case File.mkdir_p(destination_dir) do
      :ok ->
        IO.puts("Created destination directory: #{destination_dir}")

      {:error, reason} ->
        IO.puts("Failed to create directory: #{inspect(reason)}")
    end

    dataset_id_charlist = String.to_charlist(dataset_id)
    IO.puts("Dataset ID as charlist: #{inspect(dataset_id_charlist)}")

    case :datasetdb.get_dataset_by_id(dataset_id_charlist) do
      {:error, :dataset_not_found} ->
        IO.puts("ERROR: Dataset not found")
        {:noreply, put_flash(socket, :error, "Dataset not found")}

      dataset when is_tuple(dataset) ->
        IO.puts("Dataset found: #{inspect(elem(dataset, 0))}")
        dataset_title = extract_dataset_title(dataset)
        IO.puts("Dataset title: #{dataset_title}")

        filename = sanitize_filename(dataset_title) <> ".zip"
        destination = Path.join(destination_dir, filename)
        IO.puts("Destination path: #{destination}")

        user_id_charlist = String.to_charlist(user_id)
        dataset_id_charlist = String.to_charlist(dataset_id)
        destination_charlist = String.to_charlist(destination)

        IO.puts("Calling download_manager_client with:")
        IO.puts("  dataset_id: #{inspect(dataset_id_charlist)}")
        IO.puts("  destination: #{inspect(destination_charlist)}")
        IO.puts("  user_id: #{inspect(user_id_charlist)}")

        case :download_manager_client.start_erlang_binary_download(
               dataset_id_charlist,
               destination_charlist,
               user_id_charlist,
               :high,
               %{}
             ) do
          {:ok, download_id} ->
            download_id_str =
              if is_list(download_id), do: to_string(download_id), else: download_id

            IO.puts("Download started successfully with ID: #{download_id_str}")

            Process.send_after(self(), {:check_download_result, download_id_str}, 2000)

            {:noreply,
             socket
             |> put_flash(:info, "Download started with ID: #{download_id_str}")
             |> assign(:downloading_dataset, dataset_id)}

          {:error, reason} ->
            IO.puts("ERROR: Download failed to start: #{inspect(reason)}")
            {:noreply, put_flash(socket, :error, "Download failed: #{inspect(reason)}")}
        end

      error ->
        IO.puts("ERROR: Unexpected result from get_dataset_by_id: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to get dataset: #{inspect(error)}")}
    end
  end

  @impl true
  def handle_event("open_delete_modal", _params, socket) do
    {:noreply, assign(socket, show_delete_modal: true)}
  end

  @impl true
  def handle_event("close_delete_modal", _params, socket) do
    {:noreply, assign(socket, show_delete_modal: false)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("confirm_delete", _params, socket) do
    user_id = to_string(socket.assigns.user.id)
    dataset_id = socket.assigns.dataset_id

    user_id_charlist = String.to_charlist(user_id)
    dataset_id_charlist = String.to_charlist(dataset_id)

    case DatasetClient.delete_dataset(dataset_id_charlist, user_id_charlist) do
      :ok ->
        {:noreply,
         socket
         |> put_flash(:info, "Dataset deleted successfully")
         |> redirect(to: "/en/ai/datasets")}

      {:error, reason} ->
        {:noreply,
         socket
         |> assign(show_delete_modal: false)
         |> put_flash(:error, "Failed to delete: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event(event, params, socket) do
    Logger.warning("Unhandled event: #{event} with params: #{inspect(params)}")
    {:noreply, socket}
  end

  def handle_info({:check_download_result, download_id}, socket) do
    download_id_str = if is_list(download_id), do: to_string(download_id), else: download_id

    IO.puts("=== CHECK DOWNLOAD RESULT ===")
    IO.puts("Download ID: #{download_id_str}")

    case :download_manager_client.get_download_status(download_id_str) do
      {:ok, info} when is_map(info) ->
        status = Map.get(info, :status) |> to_string()
        progress = Map.get(info, :progress_percentage, 0.0)

        IO.puts("Status: #{status}")
        IO.puts("Progress: #{progress}%")

        cond do
          status == "Completed" ->
            IO.puts("Download completed! Redirecting to file...")
            locale = socket.assigns.locale

            {:noreply,
             socket
             |> put_flash(:success, "Download complete! Your file will download shortly...")
             |> redirect(to: "/#{locale}/downloads/#{download_id_str}/file")
             |> assign(:downloading_dataset, nil)}

          status == "Failed" ->
            error_msg = Map.get(info, :error, "Unknown error")
            IO.puts("Download failed: #{error_msg}")

            {:noreply,
             socket
             |> put_flash(:error, "Download failed: #{error_msg}")
             |> assign(:downloading_dataset, nil)}

          progress < 100 ->
            IO.puts("Download in progress (#{progress}%), checking again in 2 seconds...")

            if Map.get(socket.assigns, :downloading_dataset) do
              Process.send_after(self(), {:check_download_result, download_id_str}, 2000)
            end

            {:noreply,
             socket
             |> put_flash(:info, "Downloading... #{Float.round(progress, 1)}%")}

          true ->
            IO.puts("Download status unclear, checking again...")
            Process.send_after(self(), {:check_download_result, download_id_str}, 2000)
            {:noreply, socket}
        end

      {:error, reason} ->
        IO.puts("ERROR: Failed to get download status: #{inspect(reason)}")

        if Map.get(socket.assigns, :downloading_dataset) do
          IO.puts("Retrying status check in 2 seconds...")
          Process.send_after(self(), {:check_download_result, download_id_str}, 2000)
        end

        {:noreply, socket}
    end
  end

  defp extract_dataset_title(dataset_tuple) do
    case dataset_tuple do
      {:dataset, _, title, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
       _, _, _, _, _, _}
      when is_binary(title) ->
        title

      {:dataset, _, title, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
       _, _, _, _, _, _}
      when is_list(title) ->
        to_string(title)

      _ ->
        "dataset"
    end
  end

  defp sanitize_filename(filename) do
    filename
    |> String.replace(~r/[^a-zA-Z0-9._-]/, "_")
    |> String.slice(0, 200)
  end

  defp load_dataset(dataset_id) do
    dataset_id_charlist = String.to_charlist(dataset_id)

    case DatasetClient.get_dataset_by_id(dataset_id_charlist) do
      {:error, reason} ->
        {:error, reason}

      dataset_tuple ->
        case Dataset.erl_changeset(dataset_tuple) do
          %Ecto.Changeset{valid?: true} = changeset ->
            {:ok, Ecto.Changeset.apply_changes(changeset)}

          changeset ->
            Logger.error("Invalid dataset changeset: #{inspect(changeset.errors)}")
            {:error, :invalid_dataset}
        end
    end
  rescue
    error ->
      Logger.error("Error loading dataset: #{inspect(error)}")
      {:error, :load_failed}
  end

  defp get_user_rating(dataset, user_id) do
    user_id_str = to_string(user_id)

    Enum.find_value(dataset.ratings || [], 0, fn rating_map ->
      if rating_map.user_id == user_id_str do
        rating_map.rating
      else
        nil
      end
    end)
  end

  def is_owner?(dataset, user) do
    dataset.creator_id == to_string(user.id)
  end

  def format_metadata_value(value) when is_map(value) do
    case Jason.encode(value, pretty: true) do
      {:ok, json} -> json
      _ -> inspect(value)
    end
  end

  def format_metadata_value(value) when is_list(value) do
    cond do
      Enum.all?(value, &is_integer/1) ->
        try do
          List.to_string(value)
        rescue
          _ -> inspect(value)
        end

      true ->
        inspect(value)
    end
  end

  def format_metadata_value(value) when is_tuple(value) do
    case value do
      {{_year, _month, _day}, {_hour, _min, _sec}} = datetime ->
        format_datetime(datetime)

      _ ->
        inspect(value)
    end
  end

  def format_metadata_value(value), do: to_string(value)

  def format_datetime({{year, month, day}, {hour, min, sec}}) do
    "#{year}-#{pad(month)}-#{pad(day)} #{pad(hour)}:#{pad(min)}:#{pad(sec)}"
  end

  def format_datetime(_), do: "Invalid date"

  defp pad(num) when num < 10, do: "0#{num}"
  defp pad(num), do: to_string(num)

  def error_to_string(:too_large), do: "File is too large"
  def error_to_string(:not_accepted), do: "File type not accepted"
  def error_to_string(:too_many_files), do: "Too many files"
  def error_to_string(error), do: "Upload error: #{inspect(error)}"

  def get_ipfs_content(cid) when is_binary(cid) or is_list(cid) do
    cid_string = if is_binary(cid), do: cid, else: to_string(cid)

    case fetch_from_ipfs(cid_string) do
      {:ok, content} -> content
      {:error, _reason} -> cid_string
    end
  end

  def get_ipfs_content(cid), do: inspect(cid)

  defp fetch_from_ipfs(cid) do
    cid_charlist = String.to_charlist(cid)

    try do
      case :ipfs_content.get_text_content(cid_charlist) do
        content when is_list(content) -> {:ok, to_string(content)}
        content when is_binary(content) -> {:ok, content}
        _ -> {:error, :invalid_content}
      end
    catch
      _, error -> {:error, error}
    end
  end
end
