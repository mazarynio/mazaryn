defmodule MazarynWeb.AiLive.ModelShow do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.ModelClient
  alias Mazaryn.Schema.Model
  require Logger

  def framework_color("tensorflow") do
    "px-3 py-1 text-sm font-medium rounded-full bg-orange-100 text-orange-700 border border-orange-200"
  end

  def framework_color("pytorch") do
    "px-3 py-1 text-sm font-medium rounded-full bg-red-100 text-red-700 border border-red-200"
  end

  def framework_color("keras") do
    "px-3 py-1 text-sm font-medium rounded-full bg-pink-100 text-pink-700 border border-pink-200"
  end

  def framework_color("scikit_learn") do
    "px-3 py-1 text-sm font-medium rounded-full bg-blue-100 text-blue-700 border border-blue-200"
  end

  def framework_color("xgboost") do
    "px-3 py-1 text-sm font-medium rounded-full bg-green-100 text-green-700 border border-green-200"
  end

  def framework_color("onnx") do
    "px-3 py-1 text-sm font-medium rounded-full bg-purple-100 text-purple-700 border border-purple-200"
  end

  def framework_color("huggingface") do
    "px-3 py-1 text-sm font-medium rounded-full bg-yellow-100 text-yellow-700 border border-yellow-200"
  end

  def framework_color(_) do
    "px-3 py-1 text-sm font-medium rounded-full bg-slate-100 text-slate-700 border border-slate-200"
  end

  def task_color("classification") do
    "px-3 py-1 text-sm font-medium rounded-full bg-blue-100 text-blue-700 border border-blue-200"
  end

  def task_color("regression") do
    "px-3 py-1 text-sm font-medium rounded-full bg-green-100 text-green-700 border border-green-200"
  end

  def task_color("object_detection") do
    "px-3 py-1 text-sm font-medium rounded-full bg-purple-100 text-purple-700 border border-purple-200"
  end

  def task_color("segmentation") do
    "px-3 py-1 text-sm font-medium rounded-full bg-pink-100 text-pink-700 border border-pink-200"
  end

  def task_color("nlp") do
    "px-3 py-1 text-sm font-medium rounded-full bg-indigo-100 text-indigo-700 border border-indigo-200"
  end

  def task_color("generation") do
    "px-3 py-1 text-sm font-medium rounded-full bg-amber-100 text-amber-700 border border-amber-200"
  end

  def task_color("reinforcement_learning") do
    "px-3 py-1 text-sm font-medium rounded-full bg-red-100 text-red-700 border border-red-200"
  end

  def task_color(_) do
    "px-3 py-1 text-sm font-medium rounded-full bg-slate-100 text-slate-700 border border-slate-200"
  end

  def framework_display_name("tensorflow"), do: "TensorFlow"
  def framework_display_name("pytorch"), do: "PyTorch"
  def framework_display_name("keras"), do: "Keras"
  def framework_display_name("scikit_learn"), do: "Scikit-learn"
  def framework_display_name("xgboost"), do: "XGBoost"
  def framework_display_name("onnx"), do: "ONNX"
  def framework_display_name("huggingface"), do: "HuggingFace"

  def framework_display_name(framework) when is_binary(framework) do
    String.capitalize(framework)
  end

  def framework_display_name(framework) when is_atom(framework) do
    framework_display_name(Atom.to_string(framework))
  end

  def framework_display_name(_), do: "Unknown"

  def task_display_name("classification"), do: "Classification"
  def task_display_name("regression"), do: "Regression"
  def task_display_name("object_detection"), do: "Object Detection"
  def task_display_name("segmentation"), do: "Segmentation"
  def task_display_name("nlp"), do: "Natural Language Processing"
  def task_display_name("generation"), do: "Generation"
  def task_display_name("reinforcement_learning"), do: "Reinforcement Learning"

  def task_display_name(task) when is_binary(task) do
    String.capitalize(String.replace(task, "_", " "))
  end

  def task_display_name(task) when is_atom(task) do
    task_display_name(Atom.to_string(task))
  end

  def task_display_name(_), do: "Unknown"

  defp normalize_id(id) when is_binary(id) do
    id
    |> String.trim()
    |> String.replace_prefix("id:", "")
    |> String.trim()
  end

  defp normalize_id(id) when is_list(id) do
    id |> IO.iodata_to_binary() |> normalize_id()
  end

  defp normalize_id(id) when is_atom(id) do
    id |> Atom.to_string() |> normalize_id()
  end

  defp normalize_id(nil), do: ""
  defp normalize_id(:undefined), do: ""
  defp normalize_id(other), do: to_string(other) |> normalize_id()

  defp id_equal?(a, b) do
    a_str = clean_id_for_comparison(a)
    b_str = clean_id_for_comparison(b)
    a_str != "" and b_str != "" and a_str == b_str
  end

  defp clean_id_for_comparison(id) when is_binary(id) do
    id
    |> String.trim("\"")
    |> String.replace_prefix("id:", "")
    |> String.trim()
  end

  defp clean_id_for_comparison(id) when is_list(id) do
    id
    |> List.to_string()
    |> clean_id_for_comparison()
  end

  defp clean_id_for_comparison(:undefined), do: ""
  defp clean_id_for_comparison(nil), do: ""
  defp clean_id_for_comparison(other), do: to_string(other) |> clean_id_for_comparison()

  @impl true
  def mount(%{"id" => model_id} = _params, session, socket) do
    Logger.info("ModelShow mount called with model_id: #{inspect(model_id)}")
    locale = socket.assigns[:locale] || "en"

    user_result =
      cond do
        uuid = session["session_uuid"] ->
          Users.get_by_session_uuid(uuid)

        uid = session["user_id"] ->
          Users.one_by_email(uid)

        true ->
          {:error, :no_auth}
      end

    case user_result do
      {:ok, user} ->
        model_result = try_get_model(model_id)

        case model_result do
          {:ok, model_tuple} ->
            case convert_model(model_tuple) do
              nil ->
                {:ok,
                 socket
                 |> put_flash(:error, "Failed to load model data")
                 |> redirect(to: "/#{locale}/ai/models")}

              model ->
                user_id_string =
                  case user.id do
                    id when is_list(id) -> List.to_string(id)
                    id -> to_string(id)
                  end

                can_delete = id_equal?(model.creator_id, user_id_string)

                socket =
                  socket
                  |> assign(user: user)
                  |> assign(model: model)
                  |> assign(can_delete: can_delete)
                  |> assign(active_tab: "overview")
                  |> assign(locale: locale)
                  |> assign(show_delete_modal: false)
                  |> assign(download_loading: false)
                  |> assign(confirmation_text: "")
                  |> assign(can_confirm_delete: false)

                {:ok, socket}
            end

          {:error, reason} ->
            {:ok,
             socket
             |> put_flash(:error, "Model not found")
             |> redirect(to: "/#{locale}/ai/models")}
        end

      {:error, reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired or invalid")
         |> redirect(to: "/#{locale}/login")}
    end
  end

  defp try_get_model(model_id) do
    attempts = [
      fn ->
        ModelClient.get_model_by_id(model_id)
      end,
      fn ->
        charlist_id = String.to_charlist(model_id)
        ModelClient.get_model_by_id(charlist_id)
      end,
      fn ->
        charlist_id = :erlang.binary_to_list(model_id)
        ModelClient.get_model_by_id(charlist_id)
      end,
      fn ->
        normalized_id = normalize_id(model_id)
        ModelClient.get_model_by_id(normalized_id)
      end
    ]

    Enum.reduce_while(attempts, {:error, :not_found}, fn attempt_func, _acc ->
      case attempt_func.() do
        {:ok, model_tuple} when is_tuple(model_tuple) ->
          {:halt, {:ok, model_tuple}}

        model_tuple
        when is_tuple(model_tuple) and tuple_size(model_tuple) >= 1 and
               elem(model_tuple, 0) == :model ->
          {:halt, {:ok, model_tuple}}

        {:error, reason} ->
          {:cont, {:error, reason}}

        other ->
          {:cont, {:error, :unexpected_result}}
      end
    end)
  end

  defp convert_model(model) do
    case Model.erl_changeset(model) do
      %Ecto.Changeset{valid?: true} = changeset ->
        Ecto.Changeset.apply_changes(changeset)

      _changeset ->
        nil
    end
  rescue
    _e ->
      nil
  end

  defp load_model(model_id) do
    case try_get_model(model_id) do
      {:ok, model_tuple} ->
        case convert_model(model_tuple) do
          nil -> {:error, :conversion_failed}
          model -> {:ok, model}
        end

      error ->
        error
    end
  end

  @impl true
  def handle_event("download_model", _params, socket) do
    model = socket.assigns.model
    model_id = model.id
    user = socket.assigns.user

    Logger.info("DEBUG: Starting download_model event")
    Logger.info("DEBUG: Model ID: #{inspect(model_id)}")
    Logger.info("DEBUG: User: #{inspect(user.id)}")

    user_id_string =
      case user.id do
        id when is_list(id) ->
          Logger.info("DEBUG: User ID is list: #{inspect(id)}")
          List.to_string(id)

        id ->
          Logger.info("DEBUG: User ID is other type: #{inspect(id)}")
          to_string(id)
      end

    Logger.info("DEBUG: User ID string: #{inspect(user_id_string)}")
    socket = assign(socket, download_loading: true)

    case load_model(model_id) do
      {:ok, model} ->
        Logger.info("DEBUG: Successfully loaded model: #{inspect(model.title)}")

        case ModelClient.download_model(model_id, user_id_string) do
          {:error, :badarg} ->
            Logger.error("DEBUG: Badarg error in download_model")

            {:noreply,
             socket
             |> assign(download_loading: false)
             |> put_flash(:error, "Download failed: Invalid model ID format")}

          {:error, reason} ->
            Logger.error("DEBUG: download_model error: #{inspect(reason)}")

            {:noreply,
             socket
             |> assign(download_loading: false)
             |> put_flash(:error, "Download failed: #{inspect(reason)}")}

          file_content when is_binary(file_content) ->
            Logger.info(
              "DEBUG: Got binary content successfully, size: #{byte_size(file_content)}"
            )

            handle_successful_download(socket, model, file_content)

          file_content when is_list(file_content) ->
            Logger.info("DEBUG: Got list content, converting to binary")
            handle_successful_download(socket, model, :erlang.list_to_binary(file_content))

          other ->
            Logger.warn("DEBUG: Unexpected download result: #{inspect(other)}")

            {:noreply,
             socket
             |> assign(download_loading: false)
             |> put_flash(:error, "Download failed: Unexpected error")}
        end

      {:error, reason} ->
        Logger.error("DEBUG: Failed to load model: #{inspect(reason)}")

        {:noreply,
         socket
         |> assign(download_loading: false)
         |> put_flash(:error, "Failed to load model: #{inspect(reason)}")}
    end
  end

  defp handle_successful_download(socket, model, file_content) do
    original_filename = get_original_filename(model)

    Logger.info("DEBUG: Preparing download for file: #{original_filename}")
    Logger.info("DEBUG: File content size: #{byte_size(file_content)} bytes")

    if byte_size(file_content) == 0 do
      Logger.error("DEBUG: File content is empty!")

      {:noreply,
       socket
       |> assign(download_loading: false)
       |> put_flash(:error, "Download failed: File is empty")}
    else
      Logger.info("DEBUG: Encoding file content to base64")

      try do
        encoded_content = Base.encode64(file_content)

        Logger.info(
          "DEBUG: Base64 encoding successful, length: #{String.length(encoded_content)}"
        )

        socket =
          socket
          |> assign(download_loading: false)
          |> push_event("trigger_download", %{
            filename: original_filename,
            content_type: get_content_type(model.framework),
            content: encoded_content
          })
          |> put_flash(:info, "Download started: #{original_filename}")

        Logger.info("DEBUG: Download event pushed successfully")

        case load_model(model.id) do
          {:ok, updated_model} ->
            Logger.info("DEBUG: Model reloaded after download")
            {:noreply, assign(socket, model: updated_model)}

          _ ->
            Logger.warn("DEBUG: Failed to reload model after download")
            {:noreply, socket}
        end
      rescue
        error ->
          Logger.error("DEBUG: Error encoding or pushing download: #{inspect(error)}")

          {:noreply,
           socket
           |> assign(download_loading: false)
           |> put_flash(:error, "Download processing failed")}
      end
    end
  end

  defp get_original_filename(model) do
    case model.file_name do
      nil ->
        "#{String.replace(model.title, " ", "_")}_#{model.id}.#{get_file_extension(model.framework)}"

      file_name ->
        file_name
    end
  end

  defp get_content_type(framework) do
    case framework do
      "tensorflow" -> "application/x-hdf5"
      "pytorch" -> "application/octet-stream"
      "keras" -> "application/x-hdf5"
      "scikit_learn" -> "application/octet-stream"
      "xgboost" -> "application/json"
      "onnx" -> "application/octet-stream"
      "huggingface" -> "application/octet-stream"
      _ -> "application/octet-stream"
    end
  end

  defp get_file_extension(framework) when is_binary(framework) do
    case String.downcase(framework) do
      "tensorflow" -> "h5"
      "pytorch" -> "pt"
      "keras" -> "h5"
      "scikit_learn" -> "pkl"
      "xgboost" -> "json"
      "onnx" -> "onnx"
      "huggingface" -> "bin"
      _ -> "model"
    end
  end

  defp get_file_extension(framework) when is_atom(framework) do
    get_file_extension(Atom.to_string(framework))
  end

  defp get_file_extension(_), do: "model"

  @impl true
  def handle_event("rate_model", %{"rating" => rating}, socket) do
    model_id = socket.assigns.model.id
    user = socket.assigns.user

    user_id_string =
      case user.id do
        id when is_list(id) -> List.to_string(id)
        id -> to_string(id)
      end

    rating_int = String.to_integer(rating)

    case ModelClient.rate_model(model_id, user_id_string, rating_int) do
      :ok ->
        case load_model(model_id) do
          {:ok, updated_model} ->
            {:noreply,
             socket
             |> assign(model: updated_model)
             |> put_flash(:info, "Rating submitted successfully")}

          _ ->
            {:noreply, put_flash(socket, :info, "Rating submitted successfully")}
        end

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to rate model: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("open_delete_modal", _params, socket) do
    {:noreply, assign(socket, show_delete_modal: true)}
  end

  @impl true
  def handle_event("cancel_delete", _params, socket) do
    {:noreply,
     assign(socket, show_delete_modal: false, confirmation_text: "", can_confirm_delete: false)}
  end

  @impl true
  def handle_event("update_confirmation", %{"value" => text}, socket) do
    expected = "delete " <> socket.assigns.model.title
    trimmed_text = String.trim(text)
    trimmed_expected = String.trim(expected)
    can_confirm = String.downcase(trimmed_text) == String.downcase(trimmed_expected)

    {:noreply,
     socket
     |> assign(confirmation_text: text)
     |> assign(can_confirm_delete: can_confirm)}
  end

  @impl true
  def handle_event("confirm_delete", _params, socket) do
    if socket.assigns.can_confirm_delete do
      model_id = socket.assigns.model.id
      user = socket.assigns.user

      user_id_string =
        case user.id do
          id when is_list(id) -> List.to_string(id)
          id -> to_string(id)
        end

      case ModelClient.delete_model(model_id, user_id_string) do
        :ok ->
          {:noreply,
           socket
           |> put_flash(:info, "Model deleted successfully")
           |> assign(show_delete_modal: false, confirmation_text: "", can_confirm_delete: false)
           |> redirect(to: "/#{socket.assigns.locale}/ai/models")}

        {:error, reason} ->
          {:noreply,
           socket
           |> put_flash(:error, "Failed to delete model: #{inspect(reason)}")
           |> assign(show_delete_modal: false, confirmation_text: "", can_confirm_delete: false)}
      end
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, active_tab: tab)}
  end

  @impl true
  def handle_event("deploy_model", _params, socket) do
    {:noreply, put_flash(socket, :info, "Deployment feature coming soon!")}
  end

  @impl true
  def handle_event(event, params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_info(msg, socket) do
    {:noreply, socket}
  end
end
