defmodule Mazaryn.Schema.Submission do
  use Ecto.Schema
  import Ecto.Changeset

  @optional_fields ~w(
    id
    competition_id
    team_id
    user_id
    submission_cid
    notebook_id
    submission_number
    score_public
    score_private
    evaluation_status
    evaluation_cid
    error_message
    submission_time
    compute_session_id
    late_submission
    disqualified
    disqualification_reason
    file_size_bytes
    evaluation_time_seconds
    metadata
  )a

  @required_fields ~w()a

  embedded_schema do
    field(:competition_id, :string)
    field(:team_id, :string)
    field(:user_id, :string)
    field(:submission_cid, :string)
    field(:notebook_id, :string)
    field(:submission_number, :integer, default: 0)
    field(:score_public, :float)
    field(:score_private, :float)
    field(:evaluation_status, :string)
    field(:evaluation_cid, :string)
    field(:error_message, :string)
    field(:submission_time, :naive_datetime)
    field(:compute_session_id, :string)
    field(:late_submission, :boolean, default: false)
    field(:disqualified, :boolean, default: false)
    field(:disqualification_reason, :string)
    field(:file_size_bytes, :integer, default: 0)
    field(:evaluation_time_seconds, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:submission, id, competition_id, team_id, user_id, submission_cid, notebook_id,
         submission_number, score_public, score_private, evaluation_status, evaluation_cid,
         error_message, submission_time, compute_session_id, late_submission, disqualified,
         disqualification_reason, file_size_bytes, evaluation_time_seconds, metadata}
      ) do
    processed_team_id = process_string(team_id)
    processed_submission_cid = process_cid(submission_cid)
    processed_notebook_id = process_string(notebook_id)
    processed_evaluation_status = atom_to_string(evaluation_status)
    processed_evaluation_cid = process_cid(evaluation_cid)
    processed_error_message = process_string(error_message)
    processed_compute_session_id = process_string(compute_session_id)
    processed_disqualification_reason = process_string(disqualification_reason)
    processed_evaluation_time_seconds = process_integer(evaluation_time_seconds)
    processed_metadata = process_map(metadata)

    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      competition_id: to_string(competition_id),
      team_id: processed_team_id,
      user_id: to_string(user_id),
      submission_cid: processed_submission_cid,
      notebook_id: processed_notebook_id,
      submission_number: submission_number,
      score_public: score_public,
      score_private: score_private,
      evaluation_status: processed_evaluation_status,
      evaluation_cid: processed_evaluation_cid,
      error_message: processed_error_message,
      submission_time: handle_datetime(submission_time),
      compute_session_id: processed_compute_session_id,
      late_submission: late_submission,
      disqualified: disqualified,
      disqualification_reason: processed_disqualification_reason,
      file_size_bytes: file_size_bytes,
      evaluation_time_seconds: processed_evaluation_time_seconds,
      metadata: processed_metadata
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

  defp process_string(:undefined), do: nil
  defp process_string(nil), do: nil
  defp process_string(value) when is_binary(value), do: value
  defp process_string(value) when is_list(value), do: to_string(value)
  defp process_string(_), do: nil

  defp process_cid({:pending, _id}), do: nil
  defp process_cid({:pending_update, _id}), do: nil
  defp process_cid({:pending_version, _id}), do: nil
  defp process_cid({:error, _}), do: nil
  defp process_cid(:undefined), do: nil
  defp process_cid(nil), do: nil
  defp process_cid(cid) when is_binary(cid), do: cid
  defp process_cid(cid) when is_list(cid), do: to_string(cid)
  defp process_cid(_), do: nil

  defp process_integer(:undefined), do: nil
  defp process_integer(nil), do: nil
  defp process_integer(value) when is_integer(value), do: value
  defp process_integer(_), do: nil

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

  def changeset(submission, attrs \\ %{}) do
    submission
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  def is_pending?(%__MODULE__{evaluation_status: "pending"}), do: true
  def is_pending?(_), do: false

  def is_queued?(%__MODULE__{evaluation_status: "queued"}), do: true
  def is_queued?(_), do: false

  def is_evaluating?(%__MODULE__{evaluation_status: "evaluating"}), do: true
  def is_evaluating?(_), do: false

  def is_completed?(%__MODULE__{evaluation_status: "completed"}), do: true
  def is_completed?(_), do: false

  def is_failed?(%__MODULE__{evaluation_status: "failed"}), do: true
  def is_failed?(_), do: false

  def is_disqualified?(%__MODULE__{disqualified: true}), do: true
  def is_disqualified?(_), do: false

  def is_late?(%__MODULE__{late_submission: true}), do: true
  def is_late?(_), do: false

  def has_public_score?(%__MODULE__{score_public: score})
      when not is_nil(score),
      do: true

  def has_public_score?(_), do: false

  def has_private_score?(%__MODULE__{score_private: score})
      when not is_nil(score),
      do: true

  def has_private_score?(_), do: false

  def has_error?(%__MODULE__{error_message: msg})
      when not is_nil(msg) and msg != "",
      do: true

  def has_error?(_), do: false

  def has_notebook?(%__MODULE__{notebook_id: id})
      when not is_nil(id) and id != "",
      do: true

  def has_notebook?(_), do: false

  def has_compute_session?(%__MODULE__{compute_session_id: id})
      when not is_nil(id) and id != "",
      do: true

  def has_compute_session?(_), do: false

  def is_team_submission?(%__MODULE__{team_id: id})
      when not is_nil(id) and id != "",
      do: true

  def is_team_submission?(_), do: false

  def is_solo_submission?(%__MODULE__{} = submission) do
    not is_team_submission?(submission)
  end

  def format_size(%__MODULE__{file_size_bytes: size}) when is_integer(size) do
    cond do
      size >= 1_073_741_824 ->
        "#{Float.round(size / 1_073_741_824, 2)} GB"

      size >= 1_048_576 ->
        "#{Float.round(size / 1_048_576, 2)} MB"

      size >= 1024 ->
        "#{Float.round(size / 1024, 2)} KB"

      true ->
        "#{size} B"
    end
  end

  def format_size(_), do: "0 B"

  def format_evaluation_time(%__MODULE__{evaluation_time_seconds: time})
      when is_integer(time) do
    cond do
      time >= 3600 ->
        hours = div(time, 3600)
        minutes = div(rem(time, 3600), 60)
        "#{hours}h #{minutes}m"

      time >= 60 ->
        minutes = div(time, 60)
        seconds = rem(time, 60)
        "#{minutes}m #{seconds}s"

      true ->
        "#{time}s"
    end
  end

  def format_evaluation_time(_), do: "N/A"

  def status_badge_color(%__MODULE__{evaluation_status: status}) do
    case status do
      "pending" -> "gray"
      "queued" -> "blue"
      "evaluating" -> "yellow"
      "completed" -> "green"
      "failed" -> "red"
      _ -> "gray"
    end
  end

  def status_badge_color(_), do: "gray"

  def get_best_score(%__MODULE__{score_private: private, score_public: public}) do
    cond do
      not is_nil(private) -> private
      not is_nil(public) -> public
      true -> nil
    end
  end

  def format_score(nil), do: "N/A"

  def format_score(score) when is_number(score) do
    Float.round(score, 4)
  end

  def format_score(_), do: "N/A"

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

  def time_since_submission(%__MODULE__{submission_time: nil}), do: "N/A"

  def time_since_submission(%__MODULE__{submission_time: submission_time}) do
    now = NaiveDateTime.utc_now()
    diff_seconds = NaiveDateTime.diff(now, submission_time)

    cond do
      diff_seconds < 60 ->
        "#{diff_seconds} seconds ago"

      diff_seconds < 3600 ->
        minutes = div(diff_seconds, 60)
        "#{minutes} minute#{if minutes != 1, do: "s", else: ""} ago"

      diff_seconds < 86400 ->
        hours = div(diff_seconds, 3600)
        "#{hours} hour#{if hours != 1, do: "s", else: ""} ago"

      true ->
        days = div(diff_seconds, 86400)
        "#{days} day#{if days != 1, do: "s", else: ""} ago"
    end
  end

  def can_retry?(%__MODULE__{evaluation_status: "failed"}), do: true
  def can_retry?(_), do: false

  def can_delete?(%__MODULE__{evaluation_status: status})
      when status in ["pending", "queued", "failed"],
      do: true

  def can_delete?(_), do: false

  def can_edit?(%__MODULE__{evaluation_status: status})
      when status in ["pending", "queued"],
      do: true

  def can_edit?(_), do: false
end
