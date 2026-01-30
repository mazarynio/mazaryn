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
    Logger.info("Models mount started – session_uuid present")
    Logger.info("session_uuid = #{inspect(session_uuid)}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      Logger.info("User successfully fetched by session_uuid")
      Logger.info("User id = #{inspect(user.id)}")
      Logger.info("User email = #{inspect(user.email)}")

      models = load_models(user, "all")

      Logger.info("load_models(\"all\") returned #{length(models)} models")

      if length(models) > 0 do
        sample_ids = models |> Enum.take(3) |> Enum.map(& &1.id) |> inspect()
        Logger.info("First few model IDs: #{sample_ids}")
      else
        Logger.warning("No models returned from load_models")
      end

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
        |> assign(show_delete_modal: false)
        |> assign(model_to_delete: nil)
        |> assign(page: 1)
        |> assign(per_page: 12)
        |> assign(locale: "en")
        |> allow_upload(:model_file,
          accept: ~w(.h5 .pt .pth .pb .onnx .pkl .joblib .zip),
          max_entries: 1,
          max_file_size: 10_737_418_240
        )

      Logger.info("Models mounted successfully")
      Logger.info("socket.assigns.locale = #{inspect(socket.assigns.locale)}")
      Logger.info("socket.assigns.models count = #{length(socket.assigns.models)}")

      Logger.info(
        "socket.assigns.filtered_models count = #{length(socket.assigns.filtered_models)}"
      )

      {:ok, socket}
    else
      {:error, reason} ->
        Logger.error("Failed to get user by session_uuid: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info("Models mount with user_id = #{inspect(user_id)}")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        Logger.info("User found via one_by_email")
        Logger.info("User id = #{inspect(user.id)}")

        models = load_models(user, "all")

        Logger.info("load_models returned #{length(models)} models")

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
          |> assign(show_delete_modal: false)
          |> assign(model_to_delete: nil)
          |> assign(page: 1)
          |> assign(per_page: 12)
          |> assign(locale: "en")
          |> allow_upload(:model_file,
            accept: ~w(.h5 .pt .pth .pb .onnx .pkl .joblib .zip),
            max_entries: 1,
            max_file_size: 10_737_418_240
          )

        Logger.info("Models mounted successfully with user_id")
        {:ok, socket}

      {:error, reason} ->
        Logger.error("User not found by email/user_id: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_params(_params, url, socket) do
    Logger.info("handle_params called – current URL path: #{URI.parse(url).path}")
    {:noreply, assign(socket, current_path: URI.parse(url).path)}
  end

  @impl true
  def handle_event("search", %{"value" => query}, socket) do
    Logger.info("Search event – query: #{inspect(query)}")
    filtered = filter_models(socket.assigns.models, query, socket.assigns.filter)
    sorted = sort_models(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, search_query: query, filtered_models: sorted)}
  end

  @impl true
  def handle_event("filter_change", %{"filter" => filter}, socket) do
    Logger.info("Filter changed to: #{inspect(filter)}")
    user = socket.assigns.user
    models = load_models(user, filter)
    Logger.info("After filter change – loaded #{length(models)} models")
    filtered = filter_models(models, socket.assigns.search_query, filter)
    sorted = sort_models(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, filter: filter, models: models, filtered_models: sorted)}
  end

  @impl true
  def handle_event("sort_change", %{"sort" => sort}, socket) do
    Logger.info("Sort changed to: #{inspect(sort)}")
    sorted = sort_models(socket.assigns.filtered_models, sort)
    {:noreply, assign(socket, sort_by: sort, filtered_models: sorted)}
  end

  @impl true
  def handle_event("open_create_modal", _params, socket) do
    Logger.info("open_create_modal triggered")
    {:noreply, assign(socket, show_create_modal: true)}
  end

  @impl true
  def handle_event("close_create_modal", _params, socket) do
    Logger.info("close_create_modal triggered")
    {:noreply, assign(socket, show_create_modal: false)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    Logger.debug("stop_propagation event")
    {:noreply, socket}
  end

  @impl true
  def handle_event("validate_upload", _params, socket) do
    Logger.debug("validate_upload called")
    {:noreply, socket}
  end

  @impl true
  def handle_event("cancel_upload", %{"ref" => ref}, socket) do
    Logger.info("cancel_upload – ref: #{inspect(ref)}")
    {:noreply, cancel_upload(socket, :model_file, ref)}
  end

  @impl true
  def handle_event("create_model", %{"model" => model_params}, socket) do
    Logger.info("create_model event triggered")
    Logger.info("Model params received: #{inspect(model_params, limit: 100)}")

    user = socket.assigns.user
    user_id = to_string(user.id)
    Logger.info("Creating model for user_id: #{user_id}")

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
        Logger.info("Copying uploaded file to: #{dest}")
        File.cp!(path, dest)
        {:ok, dest}
      end)

    case uploaded_files do
      [file_path] ->
        file_path_charlist = String.to_charlist(file_path)
        Logger.info("Creating model from file: #{file_path}")
        Logger.info("File path as charlist: #{inspect(file_path_charlist)}")

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
        Logger.info("Temporary file removed: #{file_path}")

        case result do
          {:ok, model_id} when is_binary(model_id) or is_list(model_id) ->
            model_id_str = if is_list(model_id), do: List.to_string(model_id), else: model_id
            Logger.info("Model created successfully!")
            Logger.info("Model ID received: #{inspect(model_id)}")
            Logger.info("Current locale: #{inspect(socket.assigns.locale)}")
            nav_path = "/#{socket.assigns.locale}/ai/models/#{model_id_str}"
            Logger.info("Navigation path prepared: #{nav_path}")

            {:noreply,
             socket
             |> assign(show_create_modal: false)
             |> put_flash(:info, "Model created successfully!")
             |> push_navigate(to: nav_path)}

          model_id when is_binary(model_id) or is_list(model_id) ->
            model_id_str = if is_list(model_id), do: List.to_string(model_id), else: model_id
            Logger.info("Model created (no :ok tuple) – ID: #{inspect(model_id)}")
            Logger.info("Model ID as string: #{model_id_str}")
            nav_path = "/#{socket.assigns.locale}/ai/models/#{model_id_str}"
            Logger.info("Navigation path prepared: #{nav_path}")

            {:noreply,
             socket
             |> assign(show_create_modal: false)
             |> put_flash(:info, "Model created successfully!")
             |> push_navigate(to: nav_path)}

          {:error, reason} ->
            Logger.error("Failed to create model: #{inspect(reason)}")

            {:noreply,
             socket
             |> put_flash(:error, "Failed to create model: #{inspect(reason)}")}
        end

      [] ->
        Logger.warning("No file uploaded for model creation")
        {:noreply, put_flash(socket, :error, "Please upload a model file")}
    end
  end

  @impl true
  def handle_event("open_delete_modal", %{"id" => model_id, "title" => model_title}, socket) do
    Logger.info(
      "open_delete_modal triggered – model_id: #{model_id}, model_title: #{model_title}"
    )

    {:noreply,
     socket
     |> assign(show_delete_modal: true)
     |> assign(model_to_delete: %{id: model_id, title: model_title})}
  end

  @impl true
  def handle_event("cancel_delete", _params, socket) do
    Logger.info("cancel_delete triggered")

    {:noreply,
     socket
     |> assign(show_delete_modal: false)
     |> assign(model_to_delete: nil)}
  end

  @impl true
  def handle_event("confirm_delete", %{"id" => model_id}, socket) do
    user_id = to_string(socket.assigns.user.id)
    Logger.info("confirm_delete requested – model_id: #{inspect(model_id)}, user_id: #{user_id}")

    case ModelClient.delete_model(model_id, user_id) do
      :ok ->
        Logger.info("Model deleted successfully – reloading list")

        models = load_models(socket.assigns.user, socket.assigns.filter)
        filtered = filter_models(models, socket.assigns.search_query, socket.assigns.filter)
        sorted = sort_models(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(models: models)
         |> assign(filtered_models: sorted)
         |> assign(show_delete_modal: false)
         |> assign(model_to_delete: nil)
         |> put_flash(:info, "Model deleted successfully")}

      {:error, :model_not_found} ->
        Logger.error("Model not found for deletion – model_id: #{model_id}")

        {:noreply,
         socket
         |> assign(show_delete_modal: false)
         |> assign(model_to_delete: nil)
         |> put_flash(:error, "Model not found or already deleted")}

      {:error, :not_authorized} ->
        Logger.error("Not authorized to delete model – user_id: #{user_id}")

        {:noreply,
         socket
         |> assign(show_delete_modal: false)
         |> assign(model_to_delete: nil)
         |> put_flash(:error, "You are not authorized to delete this model")}

      {:error, reason} ->
        Logger.error("Failed to delete model: #{inspect(reason)}")

        {:noreply,
         socket
         |> assign(show_delete_modal: false)
         |> assign(model_to_delete: nil)
         |> put_flash(:error, "Failed to delete model: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("rate_model", %{"id" => model_id, "rating" => rating}, socket) do
    user_id = to_string(socket.assigns.user.id)
    rating_int = String.to_integer(rating)
    Logger.info("rate_model – model_id: #{model_id}, rating: #{rating_int}, user: #{user_id}")

    case ModelClient.rate_model(model_id, user_id, rating_int) do
      :ok ->
        Logger.info("Rating submitted successfully")
        {:noreply, put_flash(socket, :info, "Rating submitted successfully")}

      {:error, reason} ->
        Logger.error("Failed to submit rating: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to rate model: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("download_model", %{"id" => model_id}, socket) do
    user_id = to_string(socket.assigns.user.id)
    Logger.info("download_model requested – model_id: #{model_id}, user_id: #{user_id}")

    case ModelClient.download_model(model_id, user_id) do
      {:error, reason} ->
        Logger.error("Download failed: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to download: #{inspect(reason)}")}

      _content ->
        Logger.info("Download started successfully")
        {:noreply, put_flash(socket, :info, "Download started")}
    end
  end

  @impl true
  def handle_event(event, params, socket) do
    Logger.warning("Unhandled event in Models live view")
    Logger.warning("   Event: #{inspect(event)}")
    Logger.warning("   Params: #{inspect(params)}")
    {:noreply, socket}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Received message in Models live view: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp load_models(user, filter) do
    user_id = to_string(user.id)
    Logger.info("load_models called – filter: #{inspect(filter)}, user_id: #{user_id}")

    models =
      case filter do
        "my" ->
          Logger.info("Loading 'my' models (get_models_by_creator)")

          ModelClient.get_models_by_creator(user_id)
          |> convert_models()

        "public" ->
          Logger.info("Loading public models")

          ModelClient.get_public_models()
          |> convert_models()

        "trending" ->
          Logger.info("Loading trending models")

          ModelClient.get_trending_models(20)
          |> convert_models()

        "featured" ->
          Logger.info("Loading featured models")

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
          Logger.info("Loading models by framework: #{framework}")

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
          Logger.info("Loading models by task: #{task}")

          ModelClient.get_models_by_task(String.to_atom(task))
          |> convert_models()

        _ ->
          Logger.info("Loading default (public) models")

          ModelClient.get_public_models()
          |> convert_models()
      end

    Logger.info("load_models returned #{length(models)} models for filter #{filter}")

    if length(models) > 0 do
      sample = models |> Enum.take(2) |> Enum.map(& &1.id) |> inspect()
      Logger.info("Sample model IDs: #{sample}")
    end

    models
  rescue
    error ->
      Logger.error("Error in load_models: #{inspect(error)}")
      Logger.error("Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
      []
  end

  defp convert_models(models) when is_list(models) do
    Logger.debug("Converting #{length(models)} raw models")

    converted =
      Enum.map(models, &convert_single_model/1)
      |> Enum.reject(&is_nil/1)

    Logger.debug("Converted to #{length(converted)} valid models")
    converted
  end

  defp convert_models(non_list) do
    Logger.warning("convert_models received non-list: #{inspect(non_list)}")
    []
  end

  defp convert_single_model(model) do
    case Model.erl_changeset(model) do
      %Ecto.Changeset{valid?: true} = changeset ->
        converted = Ecto.Changeset.apply_changes(changeset)
        Logger.debug("Successfully converted model: #{inspect(converted.id)}")
        converted

      invalid_changeset ->
        Logger.warning("Invalid changeset for model: #{inspect(invalid_changeset.errors)}")
        nil
    end
  rescue
    e ->
      Logger.error("Exception in convert_single_model: #{inspect(e)}")
      nil
  end

  defp filter_models(models, query, _filter) do
    if query == "" do
      Logger.debug("No search query – returning all models")
      models
    else
      query_lower = String.downcase(query)
      Logger.info("Filtering models with query: #{inspect(query)} (lower: #{query_lower})")

      filtered =
        Enum.filter(models, fn model ->
          title_match =
            model.title && String.contains?(String.downcase(model.title || ""), query_lower)

          desc_match =
            model.description &&
              String.contains?(String.downcase(model.description || ""), query_lower)

          tag_match =
            Enum.any?(model.tags || [], &String.contains?(String.downcase(&1 || ""), query_lower))

          title_match || desc_match || tag_match
        end)

      Logger.info("Filtered down to #{length(filtered)} models")
      filtered
    end
  end

  defp sort_models(models, sort_by) do
    Logger.info("Sorting models by: #{sort_by} (count before: #{length(models)})")

    sorted =
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
          Logger.warning("Unknown sort key: #{sort_by} – keeping original order")
          models
      end

    Logger.info("Sorted – new count: #{length(sorted)}")
    sorted
  rescue
    e ->
      Logger.error("Error during sort_models: #{inspect(e)}")
      models
  end

  defp parse_tags(tags_string) when is_binary(tags_string) do
    tags =
      tags_string
      |> String.split(",")
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))

    Logger.debug("Parsed tags: #{inspect(tags)}")
    tags
  end

  defp parse_tags(_), do: []

  defp error_to_string(:too_large), do: "File is too large (max 10GB)"
  defp error_to_string(:not_accepted), do: "File format not accepted"
  defp error_to_string(:too_many_files), do: "Only one file allowed"
  defp error_to_string(err), do: "Upload error: #{inspect(err)}"
end
