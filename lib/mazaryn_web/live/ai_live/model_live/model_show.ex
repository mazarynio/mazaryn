defmodule MazarynWeb.AiLive.ModelShow do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.ModelClient
  alias Mazaryn.Schema.Model
  require Logger

  def framework_color("tensorflow"), do: "px-3 py-1 text-sm font-medium rounded-full bg-orange-100 text-orange-700 border border-orange-200"
  def framework_color("pytorch"), do: "px-3 py-1 text-sm font-medium rounded-full bg-red-100 text-red-700 border border-red-200"
  def framework_color("keras"), do: "px-3 py-1 text-sm font-medium rounded-full bg-pink-100 text-pink-700 border border-pink-200"
  def framework_color("scikit_learn"), do: "px-3 py-1 text-sm font-medium rounded-full bg-blue-100 text-blue-700 border border-blue-200"
  def framework_color("xgboost"), do: "px-3 py-1 text-sm font-medium rounded-full bg-green-100 text-green-700 border border-green-200"
  def framework_color("onnx"), do: "px-3 py-1 text-sm font-medium rounded-full bg-purple-100 text-purple-700 border border-purple-200"
  def framework_color("huggingface"), do: "px-3 py-1 text-sm font-medium rounded-full bg-yellow-100 text-yellow-700 border border-yellow-200"
  def framework_color(_), do: "px-3 py-1 text-sm font-medium rounded-full bg-slate-100 text-slate-700 border border-slate-200"

  def task_color("classification"), do: "px-3 py-1 text-sm font-medium rounded-full bg-blue-100 text-blue-700 border border-blue-200"
  def task_color("regression"), do: "px-3 py-1 text-sm font-medium rounded-full bg-green-100 text-green-700 border border-green-200"
  def task_color("object_detection"), do: "px-3 py-1 text-sm font-medium rounded-full bg-purple-100 text-purple-700 border border-purple-200"
  def task_color("segmentation"), do: "px-3 py-1 text-sm font-medium rounded-full bg-pink-100 text-pink-700 border border-pink-200"
  def task_color("nlp"), do: "px-3 py-1 text-sm font-medium rounded-full bg-indigo-100 text-indigo-700 border border-indigo-200"
  def task_color("generation"), do: "px-3 py-1 text-sm font-medium rounded-full bg-amber-100 text-amber-700 border border-amber-200"
  def task_color("reinforcement_learning"), do: "px-3 py-1 text-sm font-medium rounded-full bg-red-100 text-red-700 border border-red-200"
  def task_color(_), do: "px-3 py-1 text-sm font-medium rounded-full bg-slate-100 text-slate-700 border border-slate-200"

  def framework_display_name("tensorflow"), do: "TensorFlow"
  def framework_display_name("pytorch"), do: "PyTorch"
  def framework_display_name("keras"), do: "Keras"
  def framework_display_name("scikit_learn"), do: "Scikit-learn"
  def framework_display_name("xgboost"), do: "XGBoost"
  def framework_display_name("onnx"), do: "ONNX"
  def framework_display_name("huggingface"), do: "HuggingFace"
  def framework_display_name(framework) when is_binary(framework), do: String.capitalize(framework)
  def framework_display_name(framework) when is_atom(framework), do: framework_display_name(Atom.to_string(framework))
  def framework_display_name(_), do: "Unknown"

  def task_display_name("classification"), do: "Classification"
  def task_display_name("regression"), do: "Regression"
  def task_display_name("object_detection"), do: "Object Detection"
  def task_display_name("segmentation"), do: "Segmentation"
  def task_display_name("nlp"), do: "Natural Language Processing"
  def task_display_name("generation"), do: "Generation"
  def task_display_name("reinforcement_learning"), do: "Reinforcement Learning"
  def task_display_name(task) when is_binary(task), do: String.capitalize(String.replace(task, "_", " "))
  def task_display_name(task) when is_atom(task), do: task_display_name(Atom.to_string(task))
  def task_display_name(_), do: "Unknown"

  @impl true
  def mount(%{"id" => model_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      case load_model(model_id) do
        {:ok, model} ->
          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(model: model)
            |> assign(model_id: model_id)
            |> assign(loading: false)
            |> assign(show_delete_modal: false)
            |> assign(delete_confirmation_text: "")
            |> assign(active_tab: "overview")
            |> assign(locale: "en")

          {:ok, socket}

        {:error, :not_found} ->
          {:ok,
           socket
           |> put_flash(:error, "Model not found")
           |> redirect(to: "/en/ai/models")}

        {:error, reason} ->
          Logger.error("Failed to load model: #{inspect(reason)}")

          {:ok,
           socket
           |> put_flash(:error, "Failed to load model")
           |> redirect(to: "/en/ai/models")}
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
  def mount(%{"id" => model_id}, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        case load_model(model_id) do
          {:ok, model} ->
            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(model: model)
              |> assign(model_id: model_id)
              |> assign(loading: false)
              |> assign(show_delete_modal: false)
              |> assign(delete_confirmation_text: "")
              |> assign(active_tab: "overview")
              |> assign(locale: "en")

            {:ok, socket}

          {:error, :not_found} ->
            {:ok,
             socket
             |> put_flash(:error, "Model not found")
             |> redirect(to: "/en/ai/models")}

          {:error, reason} ->
            Logger.error("Failed to load model: #{inspect(reason)}")

            {:ok,
             socket
             |> put_flash(:error, "Failed to load model")
             |> redirect(to: "/en/ai/models")}
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

  defp load_model(model_id) do
    model_id_str = to_string(model_id)

    case ModelClient.get_model_by_id(model_id_str) do
      {:error, :model_not_found} ->
        {:error, :not_found}

      {:error, reason} ->
        {:error, reason}

      model when is_tuple(model) ->
        case convert_model(model) do
          nil -> {:error, :conversion_failed}
          converted -> {:ok, converted}
        end

      _ ->
        {:error, :invalid_response}
    end
  rescue
    error ->
      Logger.error("Error loading model: #{inspect(error)}")
      {:error, :load_failed}
  end

  defp convert_model(model) do
    case Model.erl_changeset(model) do
      %Ecto.Changeset{valid?: true} = changeset ->
        Ecto.Changeset.apply_changes(changeset)

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  @impl true
  def handle_event("open_delete_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(show_delete_modal: true)
     |> assign(delete_confirmation_text: "")}
  end

  @impl true
  def handle_event("close_delete_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(show_delete_modal: false)
     |> assign(delete_confirmation_text: "")}
  end

  @impl true
  def handle_event("update_delete_confirmation", %{"value" => value}, socket) do
    {:noreply, assign(socket, delete_confirmation_text: value)}
  end

  @impl true
  def handle_event("confirm_delete_model", _params, socket) do
    model = socket.assigns.model
    user_id = to_string(socket.assigns.user.id)
    expected_text = "delete #{model.title}"
    actual_text = String.downcase(socket.assigns.delete_confirmation_text)

    if String.downcase(expected_text) == actual_text do
      case ModelClient.delete_model(model.id, user_id) do
        :ok ->
          {:noreply,
           socket
           |> put_flash(:info, "Model deleted successfully")
           |> redirect(to: "/#{socket.assigns.locale}/ai/models")}

        {:error, :unauthorized} ->
          {:noreply,
           socket
           |> assign(show_delete_modal: false)
           |> put_flash(:error, "You don't have permission to delete this model")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(show_delete_modal: false)
           |> put_flash(:error, "Failed to delete model: #{inspect(reason)}")}
      end
    else
      {:noreply,
       socket
       |> put_flash(:error, "Confirmation text doesn't match")}
    end
  end

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, active_tab: tab)}
  end

  @impl true
  def handle_event("download_model", _params, socket) do
    model_id = socket.assigns.model.id
    user_id = to_string(socket.assigns.user.id)

    case ModelClient.download_model(model_id, user_id) do
      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to download: #{inspect(reason)}")}

      _content ->
        case load_model(model_id) do
          {:ok, updated_model} ->
            {:noreply,
             socket
             |> assign(model: updated_model)
             |> put_flash(:info, "Download started")}

          _ ->
            {:noreply, put_flash(socket, :info, "Download started")}
        end
    end
  end

  @impl true
  def handle_event("rate_model", %{"rating" => rating}, socket) do
    model_id = socket.assigns.model.id
    user_id = to_string(socket.assigns.user.id)
    rating_int = String.to_integer(rating)

    case ModelClient.rate_model(model_id, user_id, rating_int) do
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
  def handle_event("deploy_model", _params, socket) do
    {:noreply,
     socket
     |> put_flash(:info, "Deployment feature coming soon!")}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event(event, params, socket) do
    Logger.warning("Unhandled event in ModelShow: #{event} with params: #{inspect(params)}")
    {:noreply, socket}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Received message in ModelShow: #{inspect(msg)}")
    {:noreply, socket}
  end
end
