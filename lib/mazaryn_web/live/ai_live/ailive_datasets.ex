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
  def handle_event("delete_dataset", %{"id" => dataset_id}, socket) do
    user = socket.assigns.user
    user_id = to_string(user.id)

    dataset_id_charlist = String.to_charlist(dataset_id)
    user_id_charlist = String.to_charlist(user_id)

    case DatasetClient.delete_dataset(dataset_id_charlist, user_id_charlist) do
      :ok ->
        datasets = load_datasets(user, socket.assigns.filter)
        filtered = filter_datasets(datasets, socket.assigns.search_query, socket.assigns.filter)
        sorted = sort_datasets(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(datasets: datasets)
         |> assign(filtered_datasets: sorted)
         |> put_flash(:info, "Dataset deleted successfully")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete dataset: #{inspect(reason)}")}
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

  @impl true
  def handle_event("download_dataset", %{"id" => dataset_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case DatasetClient.download_dataset(dataset_id, user_id) do
      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to download: #{inspect(reason)}")}

      _content ->
        {:noreply, put_flash(socket, :info, "Download started")}
    end
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
end
