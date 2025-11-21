defmodule MazarynWeb.AiLive.Models do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.ModelClient
  alias Mazaryn.Schema.Model
  require Logger

  def framework_color("tensorflow"), do: "bg-orange-100 text-orange-700"
  def framework_color("pytorch"), do: "bg-red-100 text-red-700"
  def framework_color("keras"), do: "bg-pink-100 text-pink-700"
  def framework_color("scikit_learn"), do: "bg-blue-100 text-blue-700"
  def framework_color("xgboost"), do: "bg-green-100 text-green-700"
  def framework_color("onnx"), do: "bg-purple-100 text-purple-700"
  def framework_color("huggingface"), do: "bg-yellow-100 text-yellow-700"
  def framework_color(_), do: "bg-slate-100 text-slate-700"

  def task_color("classification"), do: "bg-blue-100 text-blue-700"
  def task_color("regression"), do: "bg-green-100 text-green-700"
  def task_color("object_detection"), do: "bg-purple-100 text-purple-700"
  def task_color("segmentation"), do: "bg-pink-100 text-pink-700"
  def task_color("nlp"), do: "bg-indigo-100 text-indigo-700"
  def task_color("generation"), do: "bg-amber-100 text-amber-700"
  def task_color("reinforcement_learning"), do: "bg-red-100 text-red-700"
  def task_color(_), do: "bg-slate-100 text-slate-700"

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      models = load_models(user, "all")

      socket =
        socket
        |> assign(user: user)
        |> assign(session_uuid: session_uuid)
        |> assign(models: models)
        |> assign(filtered_models: models)
        |> assign(loading: false)
        |> assign(search_query: "")
        |> assign(filter: "all")
        |> assign(sort_by: "recent")
        |> assign(show_create_modal: false)
        |> assign(page: 1)
        |> assign(per_page: 12)
        |> allow_upload(:model_file,
          accept: ~w(.h5 .pt .pth .pb .onnx .pkl .joblib .zip),
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
        models = load_models(user, "all")

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(models: models)
          |> assign(filtered_models: models)
          |> assign(loading: false)
          |> assign(search_query: "")
          |> assign(filter: "all")
          |> assign(sort_by: "recent")
          |> assign(show_create_modal: false)
          |> assign(page: 1)
          |> assign(per_page: 12)
          |> allow_upload(:model_file,
            accept: ~w(.h5 .pt .pth .pb .onnx .pkl .joblib .zip),
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
    filtered = filter_models(socket.assigns.models, query, socket.assigns.filter)
    sorted = sort_models(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, search_query: query, filtered_models: sorted)}
  end

  @impl true
  def handle_event("filter_change", %{"filter" => filter}, socket) do
    user = socket.assigns.user
    models = load_models(user, filter)
    filtered = filter_models(models, socket.assigns.search_query, filter)
    sorted = sort_models(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, filter: filter, models: models, filtered_models: sorted)}
  end

  @impl true
  def handle_event("sort_change", %{"sort" => sort}, socket) do
    sorted = sort_models(socket.assigns.filtered_models, sort)
    {:noreply, assign(socket, sort_by: sort, filtered_models: sorted)}
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
    {:noreply, cancel_upload(socket, :model_file, ref)}
  end

  @impl true
  def handle_event("create_model", %{"model" => model_params}, socket) do
    user = socket.assigns.user
    user_id = to_string(user.id)

    title = Map.get(model_params, "title", "Untitled Model")
    description = Map.get(model_params, "description", "")
    framework = String.to_atom(Map.get(model_params, "framework", "pytorch"))
    task_type = String.to_atom(Map.get(model_params, "task_type", "classification"))
    license = Map.get(model_params, "license", "MIT")
    tags = parse_tags(Map.get(model_params, "tags", ""))
    visibility = String.to_atom(Map.get(model_params, "visibility", "public"))

    uploaded_files =
      consume_uploaded_entries(socket, :model_file, fn %{path: path}, entry ->
        dest = Path.join(System.tmp_dir(), entry.client_name)
        File.cp!(path, dest)
        {:ok, dest}
      end)

    case uploaded_files do
      [file_path] ->
        file_path_charlist = String.to_charlist(file_path)

        Logger.info("Creating model from file: #{file_path}")

        result =
          ModelClient.create_model_from_file(
            user_id,
            title,
            description,
            framework,
            task_type,
            file_path_charlist,
            license,
            tags,
            visibility
          )

        File.rm(file_path)

        case result do
          {:ok, model_id} when is_binary(model_id) or is_list(model_id) ->
            Logger.info("Model created successfully with ID: #{inspect(model_id)}")
            models = load_models(user, socket.assigns.filter)
            filtered = filter_models(models, socket.assigns.search_query, socket.assigns.filter)
            sorted = sort_models(filtered, socket.assigns.sort_by)

            {:noreply,
             socket
             |> assign(show_create_modal: false)
             |> assign(models: models)
             |> assign(filtered_models: sorted)
             |> put_flash(:info, "Model created successfully!")}

          model_id when is_binary(model_id) or is_list(model_id) ->
            Logger.info("Model created successfully with ID: #{inspect(model_id)}")
            models = load_models(user, socket.assigns.filter)
            filtered = filter_models(models, socket.assigns.search_query, socket.assigns.filter)
            sorted = sort_models(filtered, socket.assigns.sort_by)

            {:noreply,
             socket
             |> assign(show_create_modal: false)
             |> assign(models: models)
             |> assign(filtered_models: sorted)
             |> put_flash(:info, "Model created successfully!")}

          {:error, reason} ->
            Logger.error("Failed to create model: #{inspect(reason)}")

            {:noreply,
             socket
             |> put_flash(:error, "Failed to create model: #{inspect(reason)}")}
        end

      [] ->
        {:noreply, put_flash(socket, :error, "Please upload a model file")}
    end
  end

  @impl true
  def handle_event("delete_model", %{"id" => model_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case ModelClient.delete_model(model_id, user_id) do
      :ok ->
        models = load_models(socket.assigns.user, socket.assigns.filter)
        filtered = filter_models(models, socket.assigns.search_query, socket.assigns.filter)
        sorted = sort_models(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(models: models)
         |> assign(filtered_models: sorted)
         |> put_flash(:info, "Model deleted successfully")}

      {:error, reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete model: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("rate_model", %{"id" => model_id, "rating" => rating}, socket) do
    user_id = to_string(socket.assigns.user.id)
    rating_int = String.to_integer(rating)

    case ModelClient.rate_model(model_id, user_id, rating_int) do
      :ok ->
        {:noreply, put_flash(socket, :info, "Rating submitted successfully")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to rate model: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("download_model", %{"id" => model_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case ModelClient.download_model(model_id, user_id) do
      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to download: #{inspect(reason)}")}

      _content ->
        {:noreply, put_flash(socket, :info, "Download started")}
    end
  end

  @impl true
  def handle_event(event, params, socket) do
    Logger.warning("Unhandled event in Models: #{event} with params: #{inspect(params)}")
    {:noreply, socket}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Received message in Models: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp load_models(user, filter) do
    user_id = to_string(user.id)

    case filter do
      "my" ->
        ModelClient.get_models_by_creator(user_id)
        |> convert_models()

      "public" ->
        ModelClient.get_public_models()
        |> convert_models()

      "trending" ->
        ModelClient.get_trending_models(20)
        |> convert_models()

      "featured" ->
        ModelClient.get_featured_models()
        |> convert_models()

      framework
      when framework in [
             "tensorflow",
             "pytorch",
             "keras",
             "scikit_learn",
             "xgboost",
             "onnx",
             "huggingface"
           ] ->
        ModelClient.get_models_by_framework(String.to_atom(framework))
        |> convert_models()

      task
      when task in [
             "classification",
             "regression",
             "object_detection",
             "segmentation",
             "nlp",
             "generation",
             "reinforcement_learning"
           ] ->
        ModelClient.get_models_by_task(String.to_atom(task))
        |> convert_models()

      _ ->
        ModelClient.get_public_models()
        |> convert_models()
    end
  rescue
    error ->
      Logger.error("Error loading models: #{inspect(error)}")
      []
  end

  defp convert_models(models) when is_list(models) do
    Enum.map(models, &convert_single_model/1)
    |> Enum.reject(&is_nil/1)
  end

  defp convert_models(_), do: []

  defp convert_single_model(model) do
    case Model.erl_changeset(model) do
      %Ecto.Changeset{valid?: true} = changeset ->
        Ecto.Changeset.apply_changes(changeset)

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  defp filter_models(models, query, _filter) do
    if query == "" do
      models
    else
      query_lower = String.downcase(query)

      Enum.filter(models, fn model ->
        title_match =
          model.title && String.contains?(String.downcase(model.title), query_lower)

        desc_match =
          model.description &&
            String.contains?(String.downcase(model.description), query_lower)

        tag_match =
          Enum.any?(model.tags || [], fn tag ->
            String.contains?(String.downcase(tag), query_lower)
          end)

        title_match || desc_match || tag_match
      end)
    end
  end

  defp sort_models(models, sort_by) do
    case sort_by do
      "recent" ->
        Enum.sort_by(models, & &1.date_updated, {:desc, NaiveDateTime})

      "popular" ->
        Enum.sort_by(models, & &1.downloads, :desc)

      "rating" ->
        Enum.sort_by(models, &Model.average_rating/1, :desc)

      "downloads" ->
        Enum.sort_by(models, & &1.downloads, :desc)

      _ ->
        models
    end
  rescue
    _ -> models
  end

  defp parse_tags(tags_string) when is_binary(tags_string) do
    tags_string
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp parse_tags(_), do: []

  defp error_to_string(:too_large), do: "File is too large (max 10GB)"
  defp error_to_string(:not_accepted), do: "File format not accepted"
  defp error_to_string(:too_many_files), do: "Only one file allowed"
  defp error_to_string(err), do: "Upload error: #{inspect(err)}"
end
