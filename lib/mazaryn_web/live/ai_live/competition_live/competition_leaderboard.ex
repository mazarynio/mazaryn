defmodule MazarynWeb.AiLive.CompetitionLeaderboard do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Mazaryn.Schema.{Competition, Leaderboard}
  require Logger

  @impl true
  def mount(%{"id" => competition_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      case load_competition_and_leaderboard(competition_id) do
        {:ok, competition, leaderboard, submissions} ->
          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(competition: competition)
            |> assign(leaderboard: leaderboard)
            |> assign(submissions: submissions)
            |> assign(filter: "all")
            |> assign(user_rank: get_user_rank(leaderboard, user.id))
            |> assign(loading: false)

          {:ok, socket}

        {:error, reason} ->
          Logger.error("Failed to load competition/leaderboard: #{inspect(reason)}")

          {:ok,
           socket
           |> put_flash(:error, "Competition or leaderboard not found")
           |> redirect(to: "/#{socket.assigns.locale}/ai/competitions")}
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
  def mount(%{"id" => competition_id}, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        case load_competition_and_leaderboard(competition_id) do
          {:ok, competition, leaderboard, submissions} ->
            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(competition: competition)
              |> assign(leaderboard: leaderboard)
              |> assign(submissions: submissions)
              |> assign(filter: "all")
              |> assign(user_rank: get_user_rank(leaderboard, user.id))
              |> assign(loading: false)

            {:ok, socket}

          {:error, reason} ->
            Logger.error("Failed to load competition/leaderboard: #{inspect(reason)}")

            {:ok,
             socket
             |> put_flash(:error, "Competition or leaderboard not found")
             |> redirect(to: "/#{socket.assigns.locale}/ai/competitions")}
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
  def handle_event("filter_change", %{"filter" => filter}, socket) do
    filtered_submissions = apply_filter(socket.assigns.submissions, filter)
    {:noreply, assign(socket, filter: filter, submissions: filtered_submissions)}
  end

  @impl true
  def handle_event(event, params, socket) do
    Logger.warning(
      "Unhandled event in CompetitionLeaderboard: #{event} with params: #{inspect(params)}"
    )

    {:noreply, socket}
  end

  @impl true
  def handle_info(msg, socket) do
    Logger.debug("Received message in CompetitionLeaderboard: #{inspect(msg)}")
    {:noreply, socket}
  end

  defp load_competition_and_leaderboard(competition_id) do
    try do
      competition_id_str = to_string(competition_id)

      case :competitiondb.get_competition_by_id(to_charlist(competition_id_str)) do
        {:error, reason} ->
          {:error, reason}

        competition_tuple ->
          competition = convert_competition(competition_tuple)

          if competition do
            case :leaderboarddb.get_leaderboard_by_competition(to_charlist(competition_id_str)) do
              {:error, _reason} ->
                {:ok, competition, nil, []}

              leaderboard_tuple ->
                leaderboard = convert_leaderboard(leaderboard_tuple)
                submissions = load_leaderboard_submissions(leaderboard_tuple)
                {:ok, competition, leaderboard, submissions}
            end
          else
            {:error, :invalid_competition}
          end
      end
    rescue
      error ->
        Logger.error("Error loading competition/leaderboard: #{inspect(error)}")
        {:error, :load_failed}
    end
  end

  defp convert_competition(competition_tuple) do
    case Competition.erl_changeset(competition_tuple) do
      %Ecto.Changeset{valid?: true} = changeset ->
        Ecto.Changeset.apply_changes(changeset)

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  defp convert_leaderboard(leaderboard_tuple) do
    case Leaderboard.erl_changeset(leaderboard_tuple) do
      %Ecto.Changeset{valid?: true} = changeset ->
        Ecto.Changeset.apply_changes(changeset)

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  defp load_leaderboard_submissions(leaderboard_tuple) do
    try do
      submission_ids =
        case leaderboard_tuple do
          {:leaderboard, _comp_id, ids, _, _, _, _, _, _, _, _, _, _, _, _, _}
          when is_list(ids) ->
            ids

          _ ->
            []
        end

      Enum.with_index(submission_ids, 1)
      |> Enum.map(fn {submission_id, rank} ->
        case :competitiondb.get_submission_by_id(submission_id) do
          {:error, _} ->
            nil

          submission ->
            extract_submission_data(submission, rank)
        end
      end)
      |> Enum.reject(&is_nil/1)
    rescue
      _ -> []
    end
  end

  defp extract_submission_data(submission, rank) do
    case submission do
      {:submission, id, _, team_id, user_id, _, _, _, score_public, _, _, _, _, submission_time,
       _, late, disqualified, _, _, _, _} ->
        %{
          id: to_string(id),
          rank: rank,
          user_id: to_string(user_id),
          team_id: if(team_id == :undefined, do: nil, else: to_string(team_id)),
          score: score_public || 0.0,
          submission_time: submission_time,
          late: late,
          disqualified: disqualified
        }

      _ ->
        nil
    end
  end

  defp get_user_rank(nil, _user_id), do: nil

  defp get_user_rank(leaderboard, user_id) do
    user_id_str = to_string(user_id)

    case Enum.find_index(leaderboard.submission_ids || [], fn sub_id ->
           case :competitiondb.get_submission_by_id(to_charlist(sub_id)) do
             {:error, _} -> false
             submission -> extract_user_id(submission) == user_id_str
           end
         end) do
      nil -> nil
      index -> index + 1
    end
  rescue
    _ -> nil
  end

  defp extract_user_id(submission) do
    case submission do
      {:submission, _, _, _, user_id, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} ->
        to_string(user_id)

      _ ->
        nil
    end
  end

  defp apply_filter(submissions, "all"), do: submissions

  defp apply_filter(submissions, "team") do
    Enum.filter(submissions, fn sub -> sub.team_id != nil end)
  end

  defp apply_filter(submissions, "solo") do
    Enum.filter(submissions, fn sub -> sub.team_id == nil end)
  end

  defp apply_filter(submissions, _), do: submissions

  def get_rank_style(rank) when rank == 1,
    do: "bg-gradient-to-r from-amber-50 to-yellow-50 border-amber-200"

  def get_rank_style(rank) when rank == 2,
    do: "bg-gradient-to-r from-slate-50 to-gray-50 border-slate-200"

  def get_rank_style(rank) when rank == 3,
    do: "bg-gradient-to-r from-orange-50 to-amber-50 border-orange-200"

  def get_rank_style(_), do: "bg-white border-slate-200"

  def get_rank_badge_style(rank) when rank == 1,
    do: "bg-gradient-to-br from-yellow-400 to-amber-500 text-white"

  def get_rank_badge_style(rank) when rank == 2,
    do: "bg-gradient-to-br from-slate-300 to-slate-400 text-white"

  def get_rank_badge_style(rank) when rank == 3,
    do: "bg-gradient-to-br from-amber-600 to-orange-700 text-white"

  def get_rank_badge_style(_), do: "bg-slate-100 text-slate-600"

  def format_submission_time({{year, month, day}, {hour, minute, _second}}) do
    month_name =
      case month do
        1 -> "Jan"
        2 -> "Feb"
        3 -> "Mar"
        4 -> "Apr"
        5 -> "May"
        6 -> "Jun"
        7 -> "Jul"
        8 -> "Aug"
        9 -> "Sep"
        10 -> "Oct"
        11 -> "Nov"
        12 -> "Dec"
      end

    "#{month_name} #{day}, #{year} at #{String.pad_leading(to_string(hour), 2, "0")}:#{String.pad_leading(to_string(minute), 2, "0")}"
  end

  def format_submission_time(_), do: "Unknown"
end
