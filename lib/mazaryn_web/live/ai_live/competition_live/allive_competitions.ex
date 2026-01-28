defmodule MazarynWeb.AiLive.Competitions do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.CompetitionClient
  alias Mazaryn.Schema.Competition
  require Logger

  def difficulty_color("beginner"), do: "bg-emerald-100 text-emerald-700"
  def difficulty_color("intermediate"), do: "bg-amber-100 text-amber-700"
  def difficulty_color("advanced"), do: "bg-orange-100 text-orange-700"
  def difficulty_color("expert"), do: "bg-red-100 text-red-700"
  def difficulty_color(_), do: "bg-slate-100 text-slate-700"

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      competitions = load_competitions(user, "all")

      socket =
        socket
        |> assign(user: user)
        |> assign(session_uuid: session_uuid)
        |> assign(competitions: competitions)
        |> assign(filtered_competitions: competitions)
        |> assign(loading: false)
        |> assign(search_query: "")
        |> assign(filter: "all")
        |> assign(sort_by: "deadline")
        |> assign(show_create_modal: false)
        |> assign(show_delete_confirm: false)
        |> assign(competition_to_delete_id: nil)
        |> assign(competition_to_delete_title: nil)
        |> assign(page: 1)
        |> assign(per_page: 12)

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
        competitions = load_competitions(user, "all")

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(competitions: competitions)
          |> assign(filtered_competitions: competitions)
          |> assign(loading: false)
          |> assign(search_query: "")
          |> assign(filter: "all")
          |> assign(sort_by: "deadline")
          |> assign(show_create_modal: false)
          |> assign(show_delete_confirm: false)
          |> assign(competition_to_delete_id: nil)
          |> assign(competition_to_delete_title: nil)
          |> assign(page: 1)
          |> assign(per_page: 12)

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
    filtered = filter_competitions(socket.assigns.competitions, query, socket.assigns.filter)
    sorted = sort_competitions(filtered, socket.assigns.sort_by)
    {:noreply, assign(socket, search_query: query, filtered_competitions: sorted)}
  end

  @impl true
  def handle_event("filter_change", %{"filter" => filter}, socket) do
    user = socket.assigns.user
    competitions = load_competitions(user, filter)
    filtered = filter_competitions(competitions, socket.assigns.search_query, filter)
    sorted = sort_competitions(filtered, socket.assigns.sort_by)

    {:noreply,
     assign(socket, filter: filter, competitions: competitions, filtered_competitions: sorted)}
  end

  @impl true
  def handle_event("sort_change", %{"sort" => sort}, socket) do
    sorted = sort_competitions(socket.assigns.filtered_competitions, sort)
    {:noreply, assign(socket, sort_by: sort, filtered_competitions: sorted)}
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
  def handle_event("create_competition", %{"competition" => comp_params}, socket) do
    user = socket.assigns.user
    user_id = to_string(user.id)

    title = Map.get(comp_params, "title", "Untitled Competition")
    description = Map.get(comp_params, "description", "")

    start_time = parse_datetime(Map.get(comp_params, "start_time", ""))
    end_time = parse_datetime(Map.get(comp_params, "end_time", ""))

    reward_type = String.to_atom(Map.get(comp_params, "reward_type", "cash"))
    reward_value = parse_float(Map.get(comp_params, "reward_value", "0"))

    evaluation_metric = String.to_atom(Map.get(comp_params, "evaluation_metric", "accuracy"))
    submission_limit = parse_integer(Map.get(comp_params, "submission_limit", "5"))
    team_size_limit = parse_integer(Map.get(comp_params, "team_size_limit", "5"))

    difficulty = Map.get(comp_params, "difficulty", "intermediate")

    rules = %{
      "description" => Map.get(comp_params, "rules", ""),
      "external_data" => Map.get(comp_params, "external_data_allowed", "false") == "true"
    }

    case CompetitionClient.create_competition(
           user_id,
           title,
           description,
           [],
           start_time,
           end_time,
           reward_type,
           reward_value,
           rules,
           evaluation_metric,
           submission_limit,
           team_size_limit
         ) do
      {:error, reason} ->
        Logger.error("Failed to create competition: #{inspect(reason)}")

        {:noreply,
         socket
         |> put_flash(:error, "Failed to create competition: #{inspect(reason)}")}

      competition_id when is_list(competition_id) or is_binary(competition_id) ->
        Logger.info("Competition created successfully with ID: #{competition_id}")

        competitions = load_competitions(user, socket.assigns.filter)

        filtered =
          filter_competitions(competitions, socket.assigns.search_query, socket.assigns.filter)

        sorted = sort_competitions(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(show_create_modal: false)
         |> assign(competitions: competitions)
         |> assign(filtered_competitions: sorted)
         |> put_flash(:info, "Competition created successfully!")}
    end
  end

  @impl true
  def handle_event("join_competition", %{"id" => competition_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case CompetitionClient.join_competition(competition_id, user_id) do
      :ok ->
        competitions = load_competitions(socket.assigns.user, socket.assigns.filter)

        filtered =
          filter_competitions(competitions, socket.assigns.search_query, socket.assigns.filter)

        sorted = sort_competitions(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(competitions: competitions)
         |> assign(filtered_competitions: sorted)
         |> put_flash(:info, "Successfully joined the competition!")}

      {:error, :already_participant} ->
        {:noreply, put_flash(socket, :info, "You're already participating in this competition")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to join: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("leave_competition", %{"id" => competition_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case CompetitionClient.leave_competition(competition_id, user_id) do
      :ok ->
        competitions = load_competitions(socket.assigns.user, socket.assigns.filter)

        filtered =
          filter_competitions(competitions, socket.assigns.search_query, socket.assigns.filter)

        sorted = sort_competitions(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(competitions: competitions)
         |> assign(filtered_competitions: sorted)
         |> put_flash(:info, "Left the competition")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to leave: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("confirm_delete", %{"id" => competition_id, "title" => title}, socket) do
    {:noreply,
     socket
     |> assign(show_delete_confirm: true)
     |> assign(competition_to_delete_id: competition_id)
     |> assign(competition_to_delete_title: title)}
  end

  @impl true
  def handle_event("cancel_delete", _params, socket) do
    {:noreply,
     socket
     |> assign(show_delete_confirm: false)
     |> assign(competition_to_delete_id: nil)
     |> assign(competition_to_delete_title: nil)}
  end

  @impl true
  def handle_event("execute_delete", %{"id" => competition_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    Logger.info("Attempting to delete competition: #{competition_id} by user: #{user_id}")

    case safe_delete_competition(competition_id, user_id) do
      :ok ->
        Logger.info("Successfully deleted competition: #{competition_id}")
        competitions = load_competitions(socket.assigns.user, socket.assigns.filter)

        filtered =
          filter_competitions(competitions, socket.assigns.search_query, socket.assigns.filter)

        sorted = sort_competitions(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(show_delete_confirm: false)
         |> assign(competition_to_delete_id: nil)
         |> assign(competition_to_delete_title: nil)
         |> assign(competitions: competitions)
         |> assign(filtered_competitions: sorted)
         |> put_flash(:info, "Competition deleted successfully")}

      {:error, :cannot_delete_active_competition} ->
        Logger.warning("Cannot delete active competition: #{competition_id}")

        {:noreply,
         socket
         |> assign(show_delete_confirm: false)
         |> put_flash(:error, "Cannot delete an active competition. End it first.")}

      {:error, :unauthorized} ->
        Logger.warning("Unauthorized delete attempt: #{competition_id} by user: #{user_id}")

        {:noreply,
         socket
         |> assign(show_delete_confirm: false)
         |> put_flash(:error, "You are not authorized to delete this competition")}

      {:error, reason} ->
        Logger.error("Failed to delete competition #{competition_id}: #{inspect(reason)}")

        {:noreply,
         socket
         |> assign(show_delete_confirm: false)
         |> put_flash(:error, "Failed to delete competition: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("start_competition", %{"id" => competition_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case CompetitionClient.start_competition(competition_id, user_id) do
      :ok ->
        competitions = load_competitions(socket.assigns.user, socket.assigns.filter)

        filtered =
          filter_competitions(competitions, socket.assigns.search_query, socket.assigns.filter)

        sorted = sort_competitions(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(competitions: competitions)
         |> assign(filtered_competitions: sorted)
         |> put_flash(:info, "Competition started!")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to start: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("end_competition", %{"id" => competition_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    case CompetitionClient.end_competition(competition_id, user_id) do
      :ok ->
        competitions = load_competitions(socket.assigns.user, socket.assigns.filter)

        filtered =
          filter_competitions(competitions, socket.assigns.search_query, socket.assigns.filter)

        sorted = sort_competitions(filtered, socket.assigns.sort_by)

        {:noreply,
         socket
         |> assign(competitions: competitions)
         |> assign(filtered_competitions: sorted)
         |> put_flash(:info, "Competition ended")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to end: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event(event, params, socket) do
    Logger.warning("Unhandled event in Competitions: #{event} with params: #{inspect(params)}")
    {:noreply, socket}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Received message in Competitions: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp load_competitions(user, filter) do
    user_id = to_string(user.id)

    case filter do
      "my" ->
        get_my_competitions(user_id)
        |> convert_competitions()

      "active" ->
        CompetitionClient.get_active_competitions()
        |> convert_competitions()

      "featured" ->
        CompetitionClient.get_featured_competitions()
        |> convert_competitions()

      "upcoming" ->
        CompetitionClient.get_upcoming_competitions()
        |> convert_competitions()

      "ended" ->
        CompetitionClient.get_competitions_by_status(:ended)
        |> convert_competitions()

      "participating" ->
        CompetitionClient.get_user_competition_history(user_id)
        |> convert_competitions()

      "trending" ->
        CompetitionClient.get_trending_competitions(20)
        |> convert_competitions()

      _ ->
        CompetitionClient.get_public_competitions()
        |> convert_competitions()
    end
  rescue
    error ->
      Logger.error("Error loading competitions: #{inspect(error)}")
      []
  end

  defp get_my_competitions(user_id) do
    try do
      :competitiondb.get_my_competitions(to_charlist(user_id))
    rescue
      UndefinedFunctionError ->
        try do
          :competitiondb.get_competitions_by_creator(to_charlist(user_id))
        rescue
          _ -> []
        end

      _ ->
        []
    end
  end

  defp safe_delete_competition(competition_id, user_id) do
    try do
      Logger.info("Calling competitiondb:delete_competition(#{competition_id}, #{user_id})")

      case :competitiondb.delete_competition(
             to_charlist(to_string(competition_id)),
             to_charlist(to_string(user_id))
           ) do
        :ok ->
          Logger.info("Delete returned :ok")
          :ok

        {:error, reason} ->
          Logger.error("Delete returned error: #{inspect(reason)}")
          {:error, reason}

        other ->
          Logger.error("Delete returned unexpected value: #{inspect(other)}")
          {:error, :unknown_error}
      end
    rescue
      error ->
        Logger.error("Exception deleting competition: #{inspect(error)}")
        {:error, :deletion_failed}
    end
  end

  defp convert_competitions(competitions) when is_list(competitions) do
    Enum.map(competitions, &convert_single_competition/1)
    |> Enum.reject(&is_nil/1)
  end

  defp convert_competitions(_), do: []

  defp convert_single_competition(competition) do
    case Competition.erl_changeset(competition) do
      %Ecto.Changeset{valid?: true} = changeset ->
        Ecto.Changeset.apply_changes(changeset)

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  defp filter_competitions(competitions, query, _filter) do
    if query == "" do
      competitions
    else
      query_lower = String.downcase(query)

      Enum.filter(competitions, fn comp ->
        title_match = comp.title && String.contains?(String.downcase(comp.title), query_lower)

        desc_match =
          comp.description && String.contains?(String.downcase(comp.description), query_lower)

        tag_match =
          Enum.any?(comp.tags || [], fn tag ->
            String.contains?(String.downcase(tag), query_lower)
          end)

        title_match || desc_match || tag_match
      end)
    end
  end

  defp sort_competitions(competitions, sort_by) do
    case sort_by do
      "deadline" ->
        Enum.sort_by(competitions, & &1.end_time, {:asc, NaiveDateTime})

      "popular" ->
        Enum.sort_by(competitions, &Competition.participant_count/1, :desc)

      "prize" ->
        Enum.sort_by(competitions, & &1.reward_value, :desc)

      "recent" ->
        Enum.sort_by(competitions, & &1.date_created, {:desc, NaiveDateTime})

      "submissions" ->
        Enum.sort_by(competitions, &Competition.submission_count/1, :desc)

      _ ->
        competitions
    end
  rescue
    _ -> competitions
  end

  defp parse_datetime(""), do: calendar_now()
  defp parse_datetime(nil), do: calendar_now()

  defp parse_datetime(datetime_string) when is_binary(datetime_string) do
    case NaiveDateTime.from_iso8601(datetime_string <> ":00") do
      {:ok, naive} -> NaiveDateTime.to_erl(naive)
      _ -> calendar_now()
    end
  end

  defp parse_datetime(_), do: calendar_now()

  defp calendar_now do
    :calendar.universal_time()
  end

  defp parse_float(value) when is_binary(value) do
    case Float.parse(value) do
      {float, _} -> float
      :error -> 0.0
    end
  end

  defp parse_float(value) when is_number(value), do: value / 1
  defp parse_float(_), do: 0.0

  defp parse_integer(value) when is_binary(value) do
    case Integer.parse(value) do
      {int, _} -> int
      :error -> 5
    end
  end

  defp parse_integer(value) when is_integer(value), do: value
  defp parse_integer(_), do: 5
end
