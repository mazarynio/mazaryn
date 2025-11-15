defmodule MazarynWeb.AiLive.Index do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      socket =
        socket
        |> assign(user: user)
        |> assign(session_uuid: session_uuid)
        |> assign(active_tab: :datasets)
        |> assign(datasets: load_user_datasets(user))
        |> assign(competitions: load_user_competitions(user))
        |> assign(notebooks: load_user_notebooks(user))
        |> assign(loading: false)
        |> assign(search_query: "")
        |> assign(filter: "all")
        |> assign(sort_by: "recent")
        |> assign(show_create_modal: false)
        |> assign(create_type: nil)

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
        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(active_tab: :datasets)
          |> assign(datasets: load_user_datasets(user))
          |> assign(competitions: load_user_competitions(user))
          |> assign(notebooks: load_user_notebooks(user))
          |> assign(loading: false)
          |> assign(search_query: "")
          |> assign(filter: "all")
          |> assign(sort_by: "recent")
          |> assign(show_create_modal: false)
          |> assign(create_type: nil)

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
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    tab_atom = String.to_existing_atom(tab)
    {:noreply, assign(socket, active_tab: tab_atom)}
  end

  @impl true
  def handle_event("search", %{"value" => query}, socket) do
    {:noreply, assign(socket, search_query: query)}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, assign(socket, search_query: query)}
  end

  @impl true
  def handle_event("filter_change", %{"filter" => filter}, socket) do
    {:noreply, assign(socket, filter: filter)}
  end

  @impl true
  def handle_event("sort_change", %{"sort" => sort}, socket) do
    {:noreply, assign(socket, sort_by: sort)}
  end

  @impl true
  def handle_event("open_create_modal", %{"type" => type}, socket) do
    {:noreply, assign(socket, show_create_modal: true, create_type: type)}
  end

  @impl true
  def handle_event("close_create_modal", _params, socket) do
    {:noreply, assign(socket, show_create_modal: false, create_type: nil)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("create_dataset", params, socket) do
    Logger.info("Creating dataset with params: #{inspect(params)}")

    {:noreply,
     socket
     |> assign(show_create_modal: false)
     |> put_flash(:info, "Dataset created successfully")}
  end

  @impl true
  def handle_event(event, params, socket) do
    Logger.warning("Unhandled event: #{event} with params: #{inspect(params)}")
    {:noreply, socket}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Received message: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp load_user_datasets(user) do
    case user.datasets do
      :undefined ->
        []

      nil ->
        []

      datasets when is_list(datasets) ->
        Enum.map(datasets, fn dataset_id ->
          case Core.DatasetClient.get_dataset_by_id(dataset_id) do
            {:error, _} ->
              nil

            dataset ->
              Mazaryn.Schema.Dataset.erl_changeset(dataset) |> Ecto.Changeset.apply_changes()
          end
        end)
        |> Enum.reject(&is_nil/1)

      _ ->
        []
    end
  rescue
    _ -> []
  end

  defp load_user_competitions(_user) do
    []
  end

  defp load_user_notebooks(_user) do
    []
  end
end
