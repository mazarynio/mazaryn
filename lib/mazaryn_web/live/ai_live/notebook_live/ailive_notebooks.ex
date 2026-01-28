defmodule MazarynWeb.AiLive.Notebooks do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.NotebookClient
  alias Mazaryn.Schema.Notebook
  require Logger

  def language_badge_color("python"), do: "px-2 py-1 text-xs font-medium rounded-full bg-blue-100 text-blue-700 border border-blue-200"
  def language_badge_color("julia"), do: "px-2 py-1 text-xs font-medium rounded-full bg-purple-100 text-purple-700 border border-purple-200"
  def language_badge_color("elixir"), do: "px-2 py-1 text-xs font-medium rounded-full bg-purple-100 text-purple-700 border border-purple-200"
  def language_badge_color("rust"), do: "px-2 py-1 text-xs font-medium rounded-full bg-orange-100 text-orange-700 border border-orange-200"
  def language_badge_color("r"), do: "px-2 py-1 text-xs font-medium rounded-full bg-blue-100 text-blue-700 border border-blue-200"
  def language_badge_color(_), do: "px-2 py-1 text-xs font-medium rounded-full bg-slate-100 text-slate-700 border border-slate-200"

  def language_display_name("python"), do: "🐍 Python"
  def language_display_name("julia"), do: "🔬 Julia"
  def language_display_name("elixir"), do: "💧 Elixir"
  def language_display_name("rust"), do: "🦀 Rust"
  def language_display_name("r"), do: "📊 R"
  def language_display_name(lang) when is_binary(lang), do: String.capitalize(lang)

  def language_display_name(language) when is_atom(language) do
    language_display_name(Atom.to_string(language))
  end

  def type_color("analysis"), do: "px-2 py-1 text-xs font-medium rounded-full bg-blue-100 text-blue-700 border border-blue-200"
  def type_color("competition_submission"), do: "px-2 py-1 text-xs font-medium rounded-full bg-green-100 text-green-700 border border-green-200"
  def type_color("tutorial"), do: "px-2 py-1 text-xs font-medium rounded-full bg-purple-100 text-purple-700 border border-purple-200"
  def type_color("research"), do: "px-2 py-1 text-xs font-medium rounded-full bg-amber-100 text-amber-700 border border-amber-200"
  def type_color(_), do: "px-2 py-1 text-xs font-medium rounded-full bg-slate-100 text-slate-700 border border-slate-200"

  def type_label("analysis"), do: "Analysis"
  def type_label("competition_submission"), do: "Competition"
  def type_label("tutorial"), do: "Tutorial"
  def type_label("research"), do: "Research"
  def type_label(type) when is_binary(type), do: String.capitalize(type)

  def type_label(type) when is_atom(type) do
    type_label(Atom.to_string(type))
  end

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      notebooks = load_notebooks(user, "all")

      socket =
        socket
        |> assign(user: user)
        |> assign(session_uuid: session_uuid)
        |> assign(notebooks: notebooks)
        |> assign(filtered_notebooks: notebooks)
        |> assign(loading: false)
        |> assign(search_query: "")
        |> assign(filter: "all")
        |> assign(sort_by: "recent")
        |> assign(show_create_modal: false)
        |> assign(show_delete_modal: false)
        |> assign(delete_notebook_id: nil)
        |> assign(delete_notebook_title: "")
        |> assign(delete_confirmation_text: "")
        |> assign(page: 1)
        |> assign(per_page: 12)
        |> assign(locale: "en")

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
        notebooks = load_notebooks(user, "all")

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(notebooks: notebooks)
          |> assign(filtered_notebooks: notebooks)
          |> assign(loading: false)
          |> assign(search_query: "")
          |> assign(filter: "all")
          |> assign(sort_by: "recent")
          |> assign(show_create_modal: false)
          |> assign(show_delete_modal: false)
          |> assign(delete_notebook_id: nil)
          |> assign(delete_notebook_title: "")
          |> assign(delete_confirmation_text: "")
          |> assign(page: 1)
          |> assign(per_page: 12)
          |> assign(locale: "en")

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
    filtered = filter_notebooks(socket.assigns.notebooks, query, socket.assigns.filter)
    sorted = sort_notebooks(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, search_query: query, filtered_notebooks: sorted)}
  end

  @impl true
  def handle_event("filter_change", %{"filter" => filter}, socket) do
    user = socket.assigns.user
    notebooks = load_notebooks(user, filter)
    filtered = filter_notebooks(notebooks, socket.assigns.search_query, filter)
    sorted = sort_notebooks(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, filter: filter, notebooks: notebooks, filtered_notebooks: sorted)}
  end

  @impl true
  def handle_event("sort_change", %{"sort" => sort}, socket) do
    sorted = sort_notebooks(socket.assigns.filtered_notebooks, sort)
    {:noreply, assign(socket, sort_by: sort, filtered_notebooks: sorted)}
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
  def handle_event("open_delete_modal", %{"id" => notebook_id, "title" => title}, socket) do
    {:noreply,
     assign(socket,
       show_delete_modal: true,
       delete_notebook_id: notebook_id,
       delete_notebook_title: title,
       delete_confirmation_text: ""
     )}
  end

  @impl true
  def handle_event("close_delete_modal", _params, socket) do
    {:noreply,
     assign(socket,
       show_delete_modal: false,
       delete_notebook_id: nil,
       delete_notebook_title: "",
       delete_confirmation_text: ""
     )}
  end

  @impl true
  def handle_event("update_delete_confirmation", %{"value" => value}, socket) do
    {:noreply, assign(socket, delete_confirmation_text: value)}
  end

  @impl true
  def handle_event("confirm_delete_notebook", _params, socket) do
    notebook_id = socket.assigns.delete_notebook_id
    expected_text = "delete " <> socket.assigns.delete_notebook_title

    if String.downcase(socket.assigns.delete_confirmation_text) == String.downcase(expected_text) do
      user_id = to_string(socket.assigns.user.id)
      user_id_charlist = String.to_charlist(user_id)
      notebook_id_charlist = String.to_charlist(notebook_id)

      case NotebookClient.delete_notebook(notebook_id_charlist, user_id_charlist) do
        :ok ->
          notebooks = load_notebooks(socket.assigns.user, socket.assigns.filter)
          filtered = filter_notebooks(notebooks, socket.assigns.search_query, socket.assigns.filter)
          sorted = sort_notebooks(filtered, socket.assigns.sort_by)

          {:noreply,
           socket
           |> assign(notebooks: notebooks)
           |> assign(filtered_notebooks: sorted)
           |> assign(show_delete_modal: false)
           |> assign(delete_notebook_id: nil)
           |> assign(delete_notebook_title: "")
           |> assign(delete_confirmation_text: "")
           |> put_flash(:info, "Notebook deleted successfully")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(show_delete_modal: false)
           |> put_flash(:error, "Failed to delete: #{inspect(reason)}")}
      end
    else
      {:noreply,
       socket
       |> put_flash(:error, "Confirmation text doesn't match. Please type exactly: delete #{socket.assigns.delete_notebook_title}")}
    end
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("create_notebook", %{"notebook" => notebook_params}, socket) do
    user = socket.assigns.user
    user_id = to_string(user.id)
    user_id_charlist = String.to_charlist(user_id)

    title = Map.get(notebook_params, "title", "Untitled Notebook")
    description = Map.get(notebook_params, "description", "")
    language = String.to_atom(Map.get(notebook_params, "language", "python"))
    kernel_type = String.to_atom(Map.get(notebook_params, "kernel", "python3"))
    _notebook_type = String.to_atom(Map.get(notebook_params, "type", "analysis"))
    visibility = String.to_atom(Map.get(notebook_params, "visibility", "public"))

    environment = %{}

    tags = parse_tags(Map.get(notebook_params, "tags", ""))

    case NotebookClient.create_notebook(
           user_id_charlist,
           title,
           description,
           language,
           kernel_type,
           [],
           :undefined,
           environment,
           tags,
           visibility
         ) do
      {:error, reason} ->
        Logger.error("Failed to create notebook: #{inspect(reason)}")

        {:noreply,
         socket
         |> put_flash(:error, "Failed to create notebook: #{inspect(reason)}")}

      notebook_id when is_list(notebook_id) or is_binary(notebook_id) ->
        Logger.info("Notebook created successfully with ID: #{notebook_id}")

        notebooks = load_notebooks(user, socket.assigns.filter)
        filtered = filter_notebooks(notebooks, socket.assigns.search_query, socket.assigns.filter)
        sorted = sort_notebooks(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(show_create_modal: false)
         |> assign(notebooks: notebooks)
         |> assign(filtered_notebooks: sorted)
         |> put_flash(:info, "Notebook created successfully!")}
    end
  end

  @impl true
  def handle_event("fork_notebook", %{"id" => notebook_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case NotebookClient.fork_notebook(notebook_id, user_id) do
      {:ok, new_notebook_id} ->
        notebooks = load_notebooks(socket.assigns.user, socket.assigns.filter)
        filtered = filter_notebooks(notebooks, socket.assigns.search_query, socket.assigns.filter)
        sorted = sort_notebooks(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(notebooks: notebooks)
         |> assign(filtered_notebooks: sorted)
         |> put_flash(:info, "Notebook forked successfully!")
         |> push_navigate(to: "/en/ai/notebooks/#{new_notebook_id}")}

      new_notebook_id when is_list(new_notebook_id) or is_binary(new_notebook_id) ->
        notebooks = load_notebooks(socket.assigns.user, socket.assigns.filter)
        filtered = filter_notebooks(notebooks, socket.assigns.search_query, socket.assigns.filter)
        sorted = sort_notebooks(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(notebooks: notebooks)
         |> assign(filtered_notebooks: sorted)
         |> put_flash(:info, "Notebook forked successfully!")
         |> push_navigate(to: "/en/ai/notebooks/#{new_notebook_id}")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to fork: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("like_notebook", %{"id" => notebook_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case NotebookClient.like_notebook(notebook_id, user_id) do
      :ok ->
        notebooks = load_notebooks(socket.assigns.user, socket.assigns.filter)
        filtered = filter_notebooks(notebooks, socket.assigns.search_query, socket.assigns.filter)
        sorted = sort_notebooks(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(notebooks: notebooks)
         |> assign(filtered_notebooks: sorted)
         |> put_flash(:info, "Liked!")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to like: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("unlike_notebook", %{"id" => notebook_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case NotebookClient.unlike_notebook(notebook_id, user_id) do
      :ok ->
        notebooks = load_notebooks(socket.assigns.user, socket.assigns.filter)
        filtered = filter_notebooks(notebooks, socket.assigns.search_query, socket.assigns.filter)
        sorted = sort_notebooks(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(notebooks: notebooks)
         |> assign(filtered_notebooks: sorted)
         |> put_flash(:info, "Unliked")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to unlike: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event(event, params, socket) do
    Logger.warning("Unhandled event in Notebooks: #{event} with params: #{inspect(params)}")
    {:noreply, socket}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Received message in Notebooks: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp load_notebooks(user, filter) do
    user_id = to_string(user.id)

    case filter do
      "my" ->
        NotebookClient.get_notebooks_by_creator(user_id)
        |> convert_notebooks()

      "public" ->
        NotebookClient.get_public_notebooks()
        |> convert_notebooks()

      "trending" ->
        NotebookClient.get_trending_notebooks(20)
        |> convert_notebooks()

      "featured" ->
        NotebookClient.get_featured_notebooks()
        |> convert_notebooks()

      "most_liked" ->
        NotebookClient.get_most_liked_notebooks(20)
        |> convert_notebooks()

      "most_forked" ->
        NotebookClient.get_most_forked_notebooks(20)
        |> convert_notebooks()

      language when language in ["python", "julia", "elixir", "rust", "r"] ->
        NotebookClient.get_notebooks_by_language(String.to_atom(language))
        |> convert_notebooks()

      type when type in ["analysis", "competition_submission", "tutorial", "research"] ->
        NotebookClient.get_notebooks_by_type(String.to_atom(type))
        |> convert_notebooks()

      _ ->
        NotebookClient.get_public_notebooks()
        |> convert_notebooks()
    end
  rescue
    error ->
      Logger.error("Error loading notebooks: #{inspect(error)}")
      []
  end

  defp convert_notebooks(notebooks) when is_list(notebooks) do
    Enum.map(notebooks, &convert_single_notebook/1)
    |> Enum.reject(&is_nil/1)
  end

  defp convert_notebooks(_), do: []

  defp convert_single_notebook(notebook) do
    case Notebook.erl_changeset(notebook) do
      %Ecto.Changeset{valid?: true} = changeset ->
        Ecto.Changeset.apply_changes(changeset)

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  defp filter_notebooks(notebooks, query, _filter) do
    if query == "" do
      notebooks
    else
      query_lower = String.downcase(query)

      Enum.filter(notebooks, fn notebook ->
        title_match =
          notebook.title && String.contains?(String.downcase(notebook.title), query_lower)

        desc_match =
          notebook.description &&
            String.contains?(String.downcase(notebook.description), query_lower)

        tag_match =
          Enum.any?(Notebook.tags(notebook) || [], fn tag ->
            String.contains?(String.downcase(tag), query_lower)
          end)

        title_match || desc_match || tag_match
      end)
    end
  end

  defp sort_notebooks(notebooks, sort_by) do
    case sort_by do
      "recent" ->
        Enum.sort_by(notebooks, & &1.date_updated, {:desc, NaiveDateTime})

      "popular" ->
        Enum.sort_by(notebooks, &Notebook.popularity_score/1, :desc)

      "likes" ->
        Enum.sort_by(notebooks, &Notebook.like_count/1, :desc)

      "forks" ->
        Enum.sort_by(notebooks, & &1.fork_count, :desc)

      "executions" ->
        Enum.sort_by(notebooks, & &1.execution_count, :desc)

      _ ->
        notebooks
    end
  rescue
    _ -> notebooks
  end

  defp parse_tags(tags_string) when is_binary(tags_string) do
    tags_string
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp parse_tags(_), do: []
end
