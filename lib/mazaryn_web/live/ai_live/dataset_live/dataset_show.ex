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

    user_id_charlist = String.to_charlist(user_id)
    dataset_id_charlist = String.to_charlist(dataset_id)

    case DatasetClient.download_dataset(dataset_id_charlist, user_id_charlist) do
      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to download: #{inspect(reason)}")}

      _content ->
        case load_dataset(dataset_id) do
          {:ok, dataset} ->
            {:noreply,
             socket
             |> assign(dataset: dataset)
             |> put_flash(:info, "Download started")}

          _ ->
            {:noreply, put_flash(socket, :info, "Download started")}
        end
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

  def format_metadata_value(value), do: to_string(value)

  def error_to_string(:too_large), do: "File is too large"
  def error_to_string(:not_accepted), do: "File type not accepted"
  def error_to_string(:too_many_files), do: "Too many files"
  def error_to_string(error), do: "Upload error: #{inspect(error)}"

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
