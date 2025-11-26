defmodule MazarynWeb.AiLive.Datasets do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.DatasetClient
  alias Mazaryn.Schema.Dataset
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      datasets = load_datasets(user, "all")

      socket =
        socket
        |> assign(user: user)
        |> assign(session_uuid: session_uuid)
        |> assign(datasets: datasets)
        |> assign(filtered_datasets: datasets)
        |> assign(loading: false)
        |> assign(search_query: "")
        |> assign(filter: "all")
        |> assign(sort_by: "recent")
        |> assign(show_create_modal: false)
        |> assign(show_delete_modal: false)
        |> assign(dataset_to_delete: nil)
        |> assign(page: 1)
        |> assign(per_page: 12)
        |> allow_upload(:dataset_file,
          accept: ~w(.zip),
          max_entries: 1,
          max_file_size: 10_737_418_240
        )

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
        datasets = load_datasets(user, "all")

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(datasets: datasets)
          |> assign(filtered_datasets: datasets)
          |> assign(loading: false)
          |> assign(search_query: "")
          |> assign(filter: "all")
          |> assign(sort_by: "recent")
          |> assign(show_create_modal: false)
          |> assign(show_delete_modal: false)
          |> assign(dataset_to_delete: nil)
          |> assign(page: 1)
          |> assign(per_page: 12)
          |> allow_upload(:dataset_file,
            accept: ~w(.zip),
            max_entries: 1,
            max_file_size: 10_737_418_240
          )

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
  def handle_event("search", %{"value" => query}, socket) do
    filtered = filter_datasets(socket.assigns.datasets, query, socket.assigns.filter)
    sorted = sort_datasets(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, search_query: query, filtered_datasets: sorted)}
  end

  @impl true
  def handle_event("filter_change", %{"filter" => filter}, socket) do
    user = socket.assigns.user
    datasets = load_datasets(user, filter)
    filtered = filter_datasets(datasets, socket.assigns.search_query, filter)
    sorted = sort_datasets(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, filter: filter, datasets: datasets, filtered_datasets: sorted)}
  end

  @impl true
  def handle_event("sort_change", %{"sort" => sort}, socket) do
    sorted = sort_datasets(socket.assigns.filtered_datasets, sort)
    {:noreply, assign(socket, sort_by: sort, filtered_datasets: sorted)}
  end

  @impl true
  def handle_event("open_create_modal", _params, socket) do
    {:noreply, assign(socket, show_create_modal: true)}
  end

  @impl true
  def handle_event("close_create_modal", _params, socket) do
    {:noreply, assign(socket, show_create_modal: false)}
  end

  @impl true
  def handle_event("open_delete_modal", %{"id" => dataset_id}, socket) do
    dataset = Enum.find(socket.assigns.datasets, &(&1.id == dataset_id))
    {:noreply, assign(socket, show_delete_modal: true, dataset_to_delete: dataset)}
  end

  @impl true
  def handle_event("close_delete_modal", _params, socket) do
    {:noreply, assign(socket, show_delete_modal: false, dataset_to_delete: nil)}
  end

  @impl true
  def handle_event("confirm_delete_dataset", _params, socket) do
    dataset = socket.assigns.dataset_to_delete
    user = socket.assigns.user
    user_id = to_string(user.id)

    dataset_id_charlist = String.to_charlist(dataset.id)
    user_id_charlist = String.to_charlist(user_id)

    case DatasetClient.delete_dataset(dataset_id_charlist, user_id_charlist) do
      :ok ->
        datasets = load_datasets(user, socket.assigns.filter)
        filtered = filter_datasets(datasets, socket.assigns.search_query, socket.assigns.filter)
        sorted = sort_datasets(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(show_delete_modal: false)
         |> assign(dataset_to_delete: nil)
         |> assign(datasets: datasets)
         |> assign(filtered_datasets: sorted)
         |> put_flash(:info, "Dataset deleted successfully")}

      {:error, reason} ->
        {:noreply,
         socket
         |> assign(show_delete_modal: false)
         |> assign(dataset_to_delete: nil)
         |> put_flash(:error, "Failed to delete dataset: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("validate_upload", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("cancel_upload", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :dataset_file, ref)}
  end

  @impl true
  def handle_event("create_dataset", %{"dataset" => dataset_params}, socket) do
    user = socket.assigns.user
    user_id = to_string(user.id)

    title = Map.get(dataset_params, "title", "Untitled Dataset")
    description = Map.get(dataset_params, "description", "")
    license = Map.get(dataset_params, "license", "MIT")
    tags = parse_tags(Map.get(dataset_params, "tags", ""))
    visibility = String.to_atom(Map.get(dataset_params, "visibility", "public"))

    uploaded_files =
      consume_uploaded_entries(socket, :dataset_file, fn %{path: path}, entry ->
        dest = Path.join(System.tmp_dir(), entry.client_name)
        File.cp!(path, dest)
        {:ok, dest}
      end)

    case uploaded_files do
      [file_path] ->
        file_path_charlist = String.to_charlist(file_path)

        Logger.info("Creating dataset from ZIP file: #{file_path}")
        Logger.info("File path as charlist: #{inspect(file_path_charlist)}")

        result =
          DatasetClient.create_dataset_from_file(
            user_id,
            title,
            description,
            file_path_charlist,
            license,
            tags,
            visibility
          )

        File.rm(file_path)

        case result do
          {:ok, dataset_id} when is_binary(dataset_id) or is_list(dataset_id) ->
            Logger.info("Dataset created successfully with ID: #{inspect(dataset_id)}")
            datasets = load_datasets(user, socket.assigns.filter)
            filtered = filter_datasets(datasets, socket.assigns.search_query, socket.assigns.filter)
            sorted = sort_datasets(filtered, socket.assigns.sort_by)

            {:noreply,
             socket
             |> assign(show_create_modal: false)
             |> assign(datasets: datasets)
             |> assign(filtered_datasets: sorted)
             |> put_flash(:info, "Dataset created successfully!")}

          dataset_id when is_binary(dataset_id) or is_list(dataset_id) ->
            Logger.info("Dataset created successfully with ID: #{inspect(dataset_id)}")
            datasets = load_datasets(user, socket.assigns.filter)
            filtered = filter_datasets(datasets, socket.assigns.search_query, socket.assigns.filter)
            sorted = sort_datasets(filtered, socket.assigns.sort_by)

            {:noreply,
             socket
             |> assign(show_create_modal: false)
             |> assign(datasets: datasets)
             |> assign(filtered_datasets: sorted)
             |> put_flash(:info, "Dataset created successfully!")}

          {:error, reason} ->
            Logger.error("Failed to create dataset: #{inspect(reason)}")

            {:noreply,
             socket
             |> put_flash(:error, "Failed to create dataset: #{inspect(reason)}")}
        end

      [] ->
        {:noreply, put_flash(socket, :error, "Please upload a ZIP file")}
    end
  end

  @impl true
  def handle_event("rate_dataset", %{"id" => dataset_id, "rating" => rating}, socket) do
    user_id = to_string(socket.assigns.user.id)
    rating_int = String.to_integer(rating)

    case DatasetClient.rate_dataset(dataset_id, user_id, rating_int) do
      :ok ->
        {:noreply, put_flash(socket, :info, "Rating submitted successfully")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to rate dataset: #{inspect(reason)}")}
    end
  end

  def handle_event("download_dataset", %{"id" => dataset_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    IO.puts("=== DOWNLOAD DATASET DEBUG ===")
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
            download_id_str = if is_list(download_id), do: to_string(download_id), else: download_id
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
      {:dataset, _, title, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}
        when is_binary(title) -> title
      {:dataset, _, title, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _}
        when is_list(title) -> to_string(title)
      _ -> "dataset"
    end
  end

  defp sanitize_filename(filename) do
    filename
    |> String.replace(~r/[^a-zA-Z0-9._-]/, "_")
    |> String.slice(0, 200)
  end

  @impl true
  def handle_event(event, params, socket) do
    Logger.warning("Unhandled event in Datasets: #{event} with params: #{inspect(params)}")
    {:noreply, socket}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Received message in Datasets: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp load_datasets(user, filter) do
    user_id = to_string(user.id)

    case filter do
      "my" ->
        load_user_datasets(user)

      "public" ->
        DatasetClient.get_public_datasets()
        |> convert_datasets()

      "trending" ->
        DatasetClient.get_trending_datasets(20)
        |> convert_datasets()

      "shared" ->
        load_shared_datasets(user_id)

      _ ->
        user_datasets = load_user_datasets(user)
        public_datasets = DatasetClient.get_public_datasets() |> convert_datasets()

        (user_datasets ++ public_datasets)
        |> Enum.uniq_by(& &1.id)
    end
  rescue
    error ->
      Logger.error("Error loading datasets: #{inspect(error)}")
      []
  end

  defp load_user_datasets(user) do
    case user.datasets do
      :undefined ->
        []

      nil ->
        []

      datasets when is_list(datasets) ->
        Enum.map(datasets, fn dataset_id ->
          case DatasetClient.get_dataset_by_id(dataset_id) do
            {:error, _} -> nil
            dataset -> convert_single_dataset(dataset)
          end
        end)
        |> Enum.reject(&is_nil/1)

      _ ->
        []
    end
  rescue
    _ -> []
  end

  defp load_shared_datasets(user_id) do
    DatasetClient.get_public_datasets()
    |> Enum.filter(fn dataset ->
      case dataset do
        {:dataset, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, collaborators, _, _} ->
          Enum.member?(collaborators, user_id)

        _ ->
          false
      end
    end)
    |> convert_datasets()
  rescue
    _ -> []
  end

  defp convert_datasets(datasets) when is_list(datasets) do
    Enum.map(datasets, &convert_single_dataset/1)
    |> Enum.reject(&is_nil/1)
  end

  defp convert_datasets(_), do: []

  defp convert_single_dataset(dataset) do
    case Dataset.erl_changeset(dataset) do
      %Ecto.Changeset{valid?: true} = changeset ->
        Ecto.Changeset.apply_changes(changeset)

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  defp filter_datasets(datasets, query, _filter) do
    if query == "" do
      datasets
    else
      query_lower = String.downcase(query)

      Enum.filter(datasets, fn dataset ->
        title_match =
          dataset.title && String.contains?(String.downcase(dataset.title), query_lower)

        desc_match =
          dataset.description &&
            String.contains?(String.downcase(dataset.description), query_lower)

        tag_match =
          Enum.any?(dataset.tags || [], fn tag ->
            String.contains?(String.downcase(tag), query_lower)
          end)

        title_match || desc_match || tag_match
      end)
    end
  end

  defp sort_datasets(datasets, sort_by) do
    case sort_by do
      "recent" ->
        Enum.sort_by(datasets, & &1.date_updated, {:desc, NaiveDateTime})

      "popular" ->
        Enum.sort_by(datasets, & &1.downloads, :desc)

      "votes" ->
        Enum.sort_by(datasets, &Dataset.average_rating/1, :desc)

      "downloads" ->
        Enum.sort_by(datasets, & &1.downloads, :desc)

      _ ->
        datasets
    end
  rescue
    _ -> datasets
  end

  defp parse_tags(tags_string) when is_binary(tags_string) do
    tags_string
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp parse_tags(_), do: []

  defp error_to_string(:too_large), do: "File is too large (max 10GB)"
  defp error_to_string(:not_accepted), do: "Only ZIP files are accepted"
  defp error_to_string(:too_many_files), do: "Only one file allowed"
  defp error_to_string(err), do: "Upload error: #{inspect(err)}"

  defp check_dataset_ready_for_download(dataset_id) do
    case :datasetdb.get_dataset_by_id(String.to_charlist(dataset_id)) do
      {:error, _} ->
        {:error, :not_found}
      dataset_tuple ->
        case dataset_tuple do
          {:dataset, _, _, _, _, content_cid, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} ->
            case content_cid do
              {:pending, _} -> {:error, :pending}
              {:pending_update, _} -> {:error, :pending}
              {:pending_version, _} -> {:error, :pending}
              :undefined -> {:error, :no_content}
              cid when is_list(cid) -> {:ok, cid}
              cid when is_binary(cid) -> {:ok, cid}
              _ -> {:error, :invalid_format}
            end
          _ -> {:error, :invalid_dataset}
        end
    end
  end
end
