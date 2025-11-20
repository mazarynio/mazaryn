defmodule Mazaryn.Schema.Leaderboard do
  use Ecto.Schema
  import Ecto.Changeset

  @optional_fields ~w(
    competition_id
    submission_ids
    evaluation_metric
    last_updated
    data
    public_submission_ids
    private_submission_ids
    evaluation_history
    team_submission_ids
    solo_submission_ids
    best_scores_per_team
    percentile_data
    score_distribution_cid
    benchmark_score
    prize_cutoffs
  )a

  @required_fields ~w()a

  @primary_key false
  embedded_schema do
    field(:competition_id, :string)
    field(:submission_ids, {:array, :string}, default: [])
    field(:evaluation_metric, :string)
    field(:last_updated, :naive_datetime)
    field(:data, :map, default: %{})
    field(:public_submission_ids, {:array, :string}, default: [])
    field(:private_submission_ids, {:array, :string}, default: [])
    field(:evaluation_history, {:array, :map}, default: [])
    field(:team_submission_ids, {:array, :string}, default: [])
    field(:solo_submission_ids, {:array, :string}, default: [])
    field(:best_scores_per_team, :map, default: %{})
    field(:percentile_data, :map, default: %{})
    field(:score_distribution_cid, :string)
    field(:benchmark_score, :float, default: 0.0)
    field(:prize_cutoffs, :map, default: %{})
  end

  def erl_changeset(
        {:leaderboard, competition_id, submission_ids, evaluation_metric, last_updated, data,
         public_submission_ids, private_submission_ids, evaluation_history, team_submission_ids,
         solo_submission_ids, best_scores_per_team, percentile_data, score_distribution_cid,
         benchmark_score, prize_cutoffs}
      ) do
    processed_submission_ids = process_string_list(submission_ids)
    processed_evaluation_metric = atom_to_string(evaluation_metric)
    processed_data = process_map(data)
    processed_public_submission_ids = process_string_list(public_submission_ids)
    processed_private_submission_ids = process_string_list(private_submission_ids)
    processed_evaluation_history = process_evaluation_history(evaluation_history)
    processed_team_submission_ids = process_string_list(team_submission_ids)
    processed_solo_submission_ids = process_string_list(solo_submission_ids)
    processed_best_scores_per_team = process_best_scores_map(best_scores_per_team)
    processed_percentile_data = process_map(percentile_data)
    processed_score_distribution_cid = process_cid(score_distribution_cid)
    processed_prize_cutoffs = process_map(prize_cutoffs)

    %__MODULE__{}
    |> change(%{
      competition_id: to_string(competition_id),
      submission_ids: processed_submission_ids,
      evaluation_metric: processed_evaluation_metric,
      last_updated: handle_datetime(last_updated),
      data: processed_data,
      public_submission_ids: processed_public_submission_ids,
      private_submission_ids: processed_private_submission_ids,
      evaluation_history: processed_evaluation_history,
      team_submission_ids: processed_team_submission_ids,
      solo_submission_ids: processed_solo_submission_ids,
      best_scores_per_team: processed_best_scores_per_team,
      percentile_data: processed_percentile_data,
      score_distribution_cid: processed_score_distribution_cid,
      benchmark_score: benchmark_score,
      prize_cutoffs: processed_prize_cutoffs
    })
  end

  def erl_changeset(_), do: change(%__MODULE__{}, %{})

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(nil), do: nil

  defp handle_datetime({{year, month, day}, {hour, minute, second}}) do
    case NaiveDateTime.new(year, month, day, hour, minute, second) do
      {:ok, datetime} -> datetime
      _ -> nil
    end
  end

  defp handle_datetime(datetime) when is_struct(datetime, NaiveDateTime), do: datetime
  defp handle_datetime(_), do: nil

  defp atom_to_string(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp atom_to_string(string) when is_binary(string), do: string
  defp atom_to_string(list) when is_list(list), do: to_string(list)
  defp atom_to_string(_), do: nil

  defp process_cid(:undefined), do: nil
  defp process_cid(nil), do: nil
  defp process_cid(cid) when is_binary(cid), do: cid
  defp process_cid(cid) when is_list(cid), do: to_string(cid)
  defp process_cid(_), do: nil

  defp process_string_list(list) when is_list(list) do
    Enum.map(list, fn
      item when is_binary(item) -> item
      item when is_list(item) -> to_string(item)
      item when is_atom(item) -> Atom.to_string(item)
      _ -> nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_string_list(:undefined), do: []
  defp process_string_list(nil), do: []
  defp process_string_list(_), do: []

  defp process_map(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {key, value}, acc ->
      string_key =
        case key do
          k when is_atom(k) -> Atom.to_string(k)
          k when is_binary(k) -> k
          k when is_list(k) -> to_string(k)
          k -> inspect(k)
        end

      processed_value =
        case value do
          v when is_list(v) ->
            if Enum.all?(v, &is_integer/1) and length(v) > 0 do
              try do
                to_string(v)
              rescue
                _ -> v
              end
            else
              v
            end

          v ->
            v
        end

      Map.put(acc, string_key, processed_value)
    end)
  end

  defp process_map(:undefined), do: %{}
  defp process_map(nil), do: %{}
  defp process_map(_), do: %{}

  defp process_evaluation_history(history) when is_list(history) do
    Enum.map(history, fn
      {timestamp, submission_id, score} ->
        %{
          timestamp: handle_datetime(timestamp),
          submission_id: to_string(submission_id),
          score: score
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_evaluation_history(:undefined), do: []
  defp process_evaluation_history(nil), do: []
  defp process_evaluation_history(_), do: []

  defp process_best_scores_map(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {team_id, value}, acc ->
      string_team_id = to_string(team_id)

      processed_value =
        case value do
          {submission_id, score} ->
            %{
              submission_id: to_string(submission_id),
              score: score
            }

          _ ->
            nil
        end

      if processed_value do
        Map.put(acc, string_team_id, processed_value)
      else
        acc
      end
    end)
  end

  defp process_best_scores_map(:undefined), do: %{}
  defp process_best_scores_map(nil), do: %{}
  defp process_best_scores_map(_), do: %{}

  def changeset(leaderboard, attrs \\ %{}) do
    leaderboard
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  def is_frozen?(%__MODULE__{data: %{"frozen" => frozen}}) when is_boolean(frozen), do: frozen
  def is_frozen?(%__MODULE__{data: data}) when is_map(data), do: Map.get(data, "frozen", false)
  def is_frozen?(_), do: false

  def total_submissions(%__MODULE__{submission_ids: ids}) when is_list(ids), do: length(ids)
  def total_submissions(_), do: 0

  def total_teams(%__MODULE__{best_scores_per_team: teams}) when is_map(teams),
    do: map_size(teams)

  def total_teams(_), do: 0

  def team_submission_count(%__MODULE__{team_submission_ids: ids}) when is_list(ids),
    do: length(ids)

  def team_submission_count(_), do: 0

  def solo_submission_count(%__MODULE__{solo_submission_ids: ids}) when is_list(ids),
    do: length(ids)

  def solo_submission_count(_), do: 0

  def has_private_leaderboard?(%__MODULE__{private_submission_ids: ids})
      when is_list(ids) and length(ids) > 0,
      do: true

  def has_private_leaderboard?(_), do: false

  def get_percentile(%__MODULE__{percentile_data: data}, percentile) when is_map(data) do
    Map.get(data, to_string(percentile))
  end

  def get_percentile(_, _), do: nil

  def get_team_best_score(%__MODULE__{best_scores_per_team: teams}, team_id) when is_map(teams) do
    Map.get(teams, to_string(team_id))
  end

  def get_team_best_score(_, _), do: nil

  def snapshot_count(%__MODULE__{data: %{"snapshot_history" => history}})
      when is_list(history),
      do: length(history)

  def snapshot_count(_), do: 0

  def evaluation_history_count(%__MODULE__{evaluation_history: history}) when is_list(history),
    do: length(history)

  def evaluation_history_count(_), do: 0

  def format_datetime(nil), do: "N/A"

  def format_datetime(%NaiveDateTime{} = datetime) do
    Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")
  end

  def format_datetime(_), do: "N/A"

  def format_date(nil), do: "N/A"

  def format_date(%NaiveDateTime{} = datetime) do
    Calendar.strftime(datetime, "%B %d, %Y")
  end

  def format_date(_), do: "N/A"
end
