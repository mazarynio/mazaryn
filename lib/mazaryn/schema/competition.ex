defmodule Mazaryn.Schema.Competition do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Competition
  """

  use Ecto.Schema
  alias Timex
  import Ecto.Changeset

  @optional_fields ~w(
    id
    title
    description
    creator_id
    business_id
    dataset_ids
    dataset_cids
    dataset_ipns
    start_time
    end_time
    reward_type
    reward_value
    rules
    evaluation_metric
    submission_count_limit
    team_size_limit
    submission_ids
    participants
    status
    visibility
    tags
    date_created
    date_updated
    report
    metadata
    discussion_cids
    team_ids
    evaluation_script_cid
    prize_distribution
    external_data_allowed
    late_submission_penalty
    compute_quota
    featured
    difficulty_level
    host_evaluation_cid
  )a

  @required_fields ~w()a

  embedded_schema do
    field(:title, :string)
    field(:description, :string)
    field(:creator_id, :string)
    field(:business_id, {:array, :string}, default: [])
    field(:dataset_ids, {:array, :string}, default: [])
    field(:dataset_cids, {:array, :string}, default: [])
    field(:dataset_ipns, {:array, :string}, default: [])
    field(:start_time, :utc_datetime)
    field(:end_time, :utc_datetime)
    field(:reward_type, :string)
    field(:reward_value, :float, default: 0.0)
    field(:rules, :map, default: %{})
    field(:evaluation_metric, :string)
    field(:submission_count_limit, :integer)
    field(:team_size_limit, :integer)
    field(:submission_ids, {:array, :string}, default: [])
    field(:participants, {:array, :string}, default: [])
    field(:status, :string, default: "draft")
    field(:visibility, :string, default: "public")
    field(:tags, {:array, :string}, default: [])
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
    field(:report, {:array, :string}, default: [])
    field(:metadata, :map, default: %{})
    field(:discussion_cids, {:array, :string}, default: [])
    field(:team_ids, {:array, :string}, default: [])
    field(:evaluation_script_cid, :string)
    field(:prize_distribution, :map, default: %{})
    field(:external_data_allowed, :boolean, default: false)
    field(:late_submission_penalty, :float, default: 0.0)
    field(:compute_quota, :map, default: %{})
    field(:featured, :boolean, default: false)
    field(:difficulty_level, :string, default: "intermediate")
    field(:host_evaluation_cid, :string)
  end

  def erl_changeset(
        {:competition, id, title, description, creator_id, business_id, dataset_ids, dataset_cids,
         dataset_ipns, start_time, end_time, reward_type, reward_value, rules, evaluation_metric,
         submission_count_limit, team_size_limit, submission_ids, participants, status,
         visibility, tags, date_created, date_updated, report, metadata, discussion_cids,
         team_ids, evaluation_script_cid, prize_distribution, external_data_allowed,
         late_submission_penalty, compute_quota, featured, difficulty_level, host_evaluation_cid}
      ) do
    processed_business_id = process_string_list(business_id)
    processed_dataset_ids = process_string_list(dataset_ids)
    processed_dataset_cids = process_string_list(dataset_cids)
    processed_dataset_ipns = process_string_list(dataset_ipns)
    processed_reward_type = atom_to_string(reward_type)
    processed_rules = process_map(rules)
    processed_evaluation_metric = atom_to_string(evaluation_metric)
    processed_submission_ids = process_string_list(submission_ids)
    processed_participants = process_string_list(participants)
    processed_status = atom_to_string(status)
    processed_visibility = atom_to_string(visibility)
    processed_tags = process_string_list(tags)
    processed_report = process_string_list(report)
    processed_metadata = process_map(metadata)
    processed_discussion_cids = process_string_list(discussion_cids)
    processed_team_ids = process_string_list(team_ids)
    processed_evaluation_script_cid = process_cid(evaluation_script_cid)
    processed_prize_distribution = process_prize_distribution(prize_distribution)
    processed_external_data_allowed = process_boolean(external_data_allowed)
    processed_compute_quota = process_map(compute_quota)
    processed_featured = process_boolean(featured)
    processed_difficulty_level = atom_to_string(difficulty_level)
    processed_host_evaluation_cid = process_cid(host_evaluation_cid)

    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      title: to_string(title),
      description: to_string(description),
      creator_id: to_string(creator_id),
      business_id: processed_business_id,
      dataset_ids: processed_dataset_ids,
      dataset_cids: processed_dataset_cids,
      dataset_ipns: processed_dataset_ipns,
      start_time: handle_datetime(start_time),
      end_time: handle_datetime(end_time),
      reward_type: processed_reward_type,
      reward_value: reward_value,
      rules: processed_rules,
      evaluation_metric: processed_evaluation_metric,
      submission_count_limit: submission_count_limit,
      team_size_limit: team_size_limit,
      submission_ids: processed_submission_ids,
      participants: processed_participants,
      status: processed_status,
      visibility: processed_visibility,
      tags: processed_tags,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      report: processed_report,
      metadata: processed_metadata,
      discussion_cids: processed_discussion_cids,
      team_ids: processed_team_ids,
      evaluation_script_cid: processed_evaluation_script_cid,
      prize_distribution: processed_prize_distribution,
      external_data_allowed: processed_external_data_allowed,
      late_submission_penalty: late_submission_penalty,
      compute_quota: processed_compute_quota,
      featured: processed_featured,
      difficulty_level: processed_difficulty_level,
      host_evaluation_cid: processed_host_evaluation_cid
    })
  end

  def erl_changeset(_), do: change(%__MODULE__{}, %{})

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(nil), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  defp atom_to_string(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp atom_to_string(string) when is_binary(string), do: string
  defp atom_to_string(list) when is_list(list), do: to_string(list)
  defp atom_to_string(_), do: nil

  defp process_string(:undefined), do: nil
  defp process_string(nil), do: nil
  defp process_string(value) when is_binary(value), do: value
  defp process_string(value) when is_list(value), do: to_string(value)
  defp process_string(_), do: nil

  defp process_cid({:pending, _id}), do: nil
  defp process_cid(:undefined), do: nil
  defp process_cid(nil), do: nil
  defp process_cid(cid) when is_binary(cid), do: cid
  defp process_cid(cid) when is_list(cid), do: to_string(cid)
  defp process_cid(_), do: nil

  defp process_boolean(true), do: true
  defp process_boolean(false), do: false
  defp process_boolean(:undefined), do: false
  defp process_boolean(_), do: false

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
          k -> inspect(k)
        end

      Map.put(acc, string_key, value)
    end)
  end

  defp process_map(:undefined), do: %{}
  defp process_map(nil), do: %{}
  defp process_map(_), do: %{}

  defp process_prize_distribution(distribution) when is_map(distribution) do
    Enum.reduce(distribution, %{}, fn {rank, prize}, acc ->
      string_key =
        case rank do
          r when is_integer(r) -> Integer.to_string(r)
          r when is_atom(r) -> Atom.to_string(r)
          r when is_binary(r) -> r
          r -> inspect(r)
        end

      Map.put(acc, string_key, prize)
    end)
  end

  defp process_prize_distribution(:undefined), do: %{}
  defp process_prize_distribution(nil), do: %{}
  defp process_prize_distribution(_), do: %{}

  def changeset(competition, attrs \\ %{}) do
    competition
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  def is_active?(%__MODULE__{status: "active"}), do: true
  def is_active?(_), do: false

  def is_draft?(%__MODULE__{status: "draft"}), do: true
  def is_draft?(_), do: false

  def is_ended?(%__MODULE__{status: "ended"}), do: true
  def is_ended?(_), do: false

  def is_archived?(%__MODULE__{status: "archived"}), do: true
  def is_archived?(_), do: false

  def is_voting?(%__MODULE__{status: "voting"}), do: true
  def is_voting?(_), do: false

  def is_public?(%__MODULE__{visibility: "public"}), do: true
  def is_public?(_), do: false

  def is_featured?(%__MODULE__{featured: true}), do: true
  def is_featured?(_), do: false

  def participant_count(%__MODULE__{participants: participants}) when is_list(participants) do
    length(participants)
  end

  def participant_count(_), do: 0

  def team_count(%__MODULE__{team_ids: teams}) when is_list(teams) do
    length(teams)
  end

  def team_count(_), do: 0

  def submission_count(%__MODULE__{submission_ids: submissions}) when is_list(submissions) do
    length(submissions)
  end

  def submission_count(_), do: 0

  def discussion_count(%__MODULE__{discussion_cids: discussions}) when is_list(discussions) do
    length(discussions)
  end

  def discussion_count(_), do: 0

  def is_participant?(%__MODULE__{participants: participants}, user_id)
      when is_list(participants) do
    Enum.member?(participants, user_id)
  end

  def is_participant?(_, _), do: false

  def days_remaining(%__MODULE__{end_time: end_time}) when not is_nil(end_time) do
    now = DateTime.utc_now()

    case DateTime.compare(end_time, now) do
      :gt ->
        diff = DateTime.diff(end_time, now, :second)
        div(diff, 86400)

      _ ->
        0
    end
  end

  def days_remaining(_), do: 0

  def hours_remaining(%__MODULE__{end_time: end_time}) when not is_nil(end_time) do
    now = DateTime.utc_now()

    case DateTime.compare(end_time, now) do
      :gt ->
        diff = DateTime.diff(end_time, now, :second)
        div(diff, 3600)

      _ ->
        0
    end
  end

  def hours_remaining(_), do: 0

  def has_started?(%__MODULE__{start_time: start_time}) when not is_nil(start_time) do
    now = DateTime.utc_now()
    DateTime.compare(start_time, now) != :gt
  end

  def has_started?(_), do: false

  def has_ended?(%__MODULE__{end_time: end_time}) when not is_nil(end_time) do
    now = DateTime.utc_now()
    DateTime.compare(end_time, now) != :gt
  end

  def has_ended?(_), do: false

  def is_running?(%__MODULE__{} = competition) do
    has_started?(competition) and not has_ended?(competition) and is_active?(competition)
  end

  def is_running?(_), do: false

  def difficulty_label(%__MODULE__{difficulty_level: "beginner"}), do: "Beginner"
  def difficulty_label(%__MODULE__{difficulty_level: "intermediate"}), do: "Intermediate"
  def difficulty_label(%__MODULE__{difficulty_level: "advanced"}), do: "Advanced"
  def difficulty_label(%__MODULE__{difficulty_level: "expert"}), do: "Expert"
  def difficulty_label(_), do: "Unknown"

  def total_prize(%__MODULE__{prize_distribution: distribution}) when is_map(distribution) do
    distribution
    |> Map.values()
    |> Enum.sum()
  end

  def total_prize(_), do: 0

  def prize_for_rank(%__MODULE__{prize_distribution: distribution}, rank)
      when is_map(distribution) do
    string_rank = Integer.to_string(rank)
    Map.get(distribution, string_rank, 0)
  end

  def prize_for_rank(_, _), do: 0

  def has_evaluation_script?(%__MODULE__{evaluation_script_cid: cid})
      when not is_nil(cid) and cid != "" do
    true
  end

  def has_evaluation_script?(_), do: false

  def allows_external_data?(%__MODULE__{external_data_allowed: true}), do: true
  def allows_external_data?(_), do: false

  def cpu_quota(%__MODULE__{compute_quota: quota}) when is_map(quota) do
    Map.get(quota, "cpu_hours", 0)
  end

  def cpu_quota(_), do: 0

  def gpu_quota(%__MODULE__{compute_quota: quota}) when is_map(quota) do
    Map.get(quota, "gpu_hours", 0)
  end

  def gpu_quota(_), do: 0

  def tpu_quota(%__MODULE__{compute_quota: quota}) when is_map(quota) do
    Map.get(quota, "tpu_hours", 0)
  end

  def tpu_quota(_), do: 0

  def format_reward(%__MODULE__{reward_type: type, reward_value: value}) do
    case type do
      "cash" -> "$#{value}"
      "token" -> "#{value} tokens"
      "points" -> "#{value} points"
      _ -> "#{value}"
    end
  end

  def format_reward(_), do: "0"

  def status_label(%__MODULE__{status: "draft"}), do: "Draft"
  def status_label(%__MODULE__{status: "active"}), do: "Active"
  def status_label(%__MODULE__{status: "ended"}), do: "Ended"
  def status_label(%__MODULE__{status: "voting"}), do: "Voting"
  def status_label(%__MODULE__{status: "archived"}), do: "Archived"
  def status_label(_), do: "Unknown"
end
