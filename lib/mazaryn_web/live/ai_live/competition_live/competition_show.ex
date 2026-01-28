defmodule MazarynWeb.AiLive.CompetitionShow do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Core.CompetitionClient
  alias Mazaryn.Schema.Competition
  require Logger

  @impl true
  def mount(%{"id" => competition_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      case load_competition(competition_id) do
        {:ok, competition} ->
          leaderboard = load_leaderboard(competition_id)
          submissions = load_user_submissions(competition_id, user.id)
          datasets = load_competition_datasets(competition)

          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(competition: competition)
            |> assign(leaderboard: leaderboard)
            |> assign(submissions: submissions)
            |> assign(datasets: datasets)
            |> assign(active_tab: "overview")
            |> assign(loading: false)

          {:ok, socket}

        {:error, _reason} ->
          {:ok,
           socket
           |> put_flash(:error, "Competition not found")
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
        case load_competition(competition_id) do
          {:ok, competition} ->
            leaderboard = load_leaderboard(competition_id)
            submissions = load_user_submissions(competition_id, user.id)
            datasets = load_competition_datasets(competition)

            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(competition: competition)
              |> assign(leaderboard: leaderboard)
              |> assign(submissions: submissions)
              |> assign(datasets: datasets)
              |> assign(active_tab: "overview")
              |> assign(loading: false)

            {:ok, socket}

          {:error, _reason} ->
            {:ok,
             socket
             |> put_flash(:error, "Competition not found")
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
  def handle_event("change_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, active_tab: tab)}
  end

  @impl true
  def handle_event("join_competition", _params, socket) do
    competition_id = socket.assigns.competition.id
    user_id = to_string(socket.assigns.user.id)

    case CompetitionClient.join_competition(competition_id, user_id) do
      :ok ->
        case load_competition(competition_id) do
          {:ok, competition} ->
            {:noreply,
             socket
             |> assign(competition: competition)
             |> put_flash(:info, "Successfully joined the competition!")}

          _ ->
            {:noreply, put_flash(socket, :info, "Joined competition")}
        end

      {:error, :already_participant} ->
        {:noreply, put_flash(socket, :info, "You're already participating in this competition")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to join: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("leave_competition", _params, socket) do
    competition_id = socket.assigns.competition.id
    user_id = to_string(socket.assigns.user.id)

    case CompetitionClient.leave_competition(competition_id, user_id) do
      :ok ->
        case load_competition(competition_id) do
          {:ok, competition} ->
            {:noreply,
             socket
             |> assign(competition: competition)
             |> put_flash(:info, "Left the competition")}

          _ ->
            {:noreply, put_flash(socket, :info, "Left competition")}
        end

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to leave: #{inspect(reason)}")}
    end
  end

  defp load_competition(competition_id) do
    try do
      competition_id_str = to_string(competition_id)

      case :competitiondb.get_competition_by_id(to_charlist(competition_id_str)) do
        {:error, reason} ->
          {:error, reason}

        competition ->
          case Competition.erl_changeset(competition) do
            %Ecto.Changeset{valid?: true} = changeset ->
              {:ok, Ecto.Changeset.apply_changes(changeset)}

            _ ->
              {:error, :invalid_competition}
          end
      end
    rescue
      error ->
        Logger.error("Error loading competition: #{inspect(error)}")
        {:error, :load_failed}
    end
  end

  defp load_leaderboard(competition_id) do
    try do
      case :competitiondb.get_leaderboard(to_charlist(to_string(competition_id))) do
        {:error, _} ->
          []

        leaderboard ->
          submission_ids =
            case leaderboard do
              {:leaderboard, _, ids, _, _, _, _, _, _, _, _, _, _, _, _, _} when is_list(ids) ->
                ids

              _ ->
                []
            end

          load_submissions_from_ids(submission_ids)
      end
    rescue
      _ -> []
    end
  end

  defp load_submissions_from_ids(submission_ids) do
    Enum.map(submission_ids, fn submission_id ->
      try do
        case :competitiondb.get_submission_by_id(submission_id) do
          {:error, _} ->
            nil

          submission ->
            extract_submission_data(submission)
        end
      rescue
        _ -> nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp extract_submission_data(submission) do
    case submission do
      {:submission, id, _, team_id, user_id, _, _, _, score_public, _, _, _, _, submission_time,
       _, late, disqualified, _, _, _, _} ->
        %{
          id: to_string(id),
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

  defp load_user_submissions(competition_id, user_id) do
    try do
      :competitiondb.get_user_submissions(
        to_charlist(to_string(competition_id)),
        to_charlist(to_string(user_id))
      )
      |> Enum.map(&extract_submission_data/1)
      |> Enum.reject(&is_nil/1)
    rescue
      _ -> []
    end
  end

  defp load_competition_datasets(competition) do
    try do
      Enum.map(competition.dataset_ids || [], fn dataset_id ->
        case :datasetdb.get_dataset_by_id(to_charlist(to_string(dataset_id))) do
          {:error, _} ->
            nil

          dataset ->
            extract_dataset_info(dataset)
        end
      end)
      |> Enum.reject(&is_nil/1)
    rescue
      _ -> []
    end
  end

  defp extract_dataset_info(dataset) do
    case dataset do
      {:dataset, id, title, description, _, _, _, _, size_bytes, _, _, _, _, _, _, _, _, _, _, _,
       _, _, _, _, _, _, _, _, _} ->
        %{
          id: to_string(id),
          title: to_string(title),
          description: to_string(description),
          size_bytes: size_bytes || 0
        }

      _ ->
        nil
    end
  end
end
