defmodule Mazaryn.Schema.Team do
  use Ecto.Schema
  import Ecto.Changeset

  @optional_fields ~w(
    id
    competition_id
    name
    creator_id
    members
    invitations
    merge_requests
    submission_ids
    team_score
    rank
    disbanded
    date_created
    date_updated
    discussion_cids
    notebook_ids
    compute_quota_used
    total_submissions
    best_submission_id
    team_avatar_cid
    metadata
  )a

  @required_fields ~w()a

  embedded_schema do
    field(:competition_id, :string)
    field(:name, :string)
    field(:creator_id, :string)
    field(:members, {:array, :map}, default: [])
    field(:invitations, {:array, :map}, default: [])
    field(:merge_requests, {:array, :map}, default: [])
    field(:submission_ids, {:array, :string}, default: [])
    field(:team_score, :float)
    field(:rank, :integer)
    field(:disbanded, :boolean, default: false)
    field(:date_created, :naive_datetime)
    field(:date_updated, :naive_datetime)
    field(:discussion_cids, {:array, :string}, default: [])
    field(:notebook_ids, {:array, :string}, default: [])
    field(:compute_quota_used, :map, default: %{})
    field(:total_submissions, :integer, default: 0)
    field(:best_submission_id, :string)
    field(:team_avatar_cid, :string)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:team, id, competition_id, name, creator_id, members, invitations, merge_requests,
         submission_ids, team_score, rank, disbanded, date_created, date_updated,
         discussion_cids, notebook_ids, compute_quota_used, total_submissions,
         best_submission_id, team_avatar_cid, metadata}
      ) do
    processed_members = process_members(members)
    processed_invitations = process_invitations(invitations)
    processed_merge_requests = process_merge_requests(merge_requests)
    processed_submission_ids = process_string_list(submission_ids)
    processed_discussion_cids = process_string_list(discussion_cids)
    processed_notebook_ids = process_string_list(notebook_ids)
    processed_compute_quota_used = process_map(compute_quota_used)
    processed_best_submission_id = process_string(best_submission_id)
    processed_team_avatar_cid = process_cid(team_avatar_cid)
    processed_metadata = process_map(metadata)
    processed_rank = process_integer(rank)
    processed_team_score = process_float(team_score)

    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      competition_id: to_string(competition_id),
      name: to_string(name),
      creator_id: to_string(creator_id),
      members: processed_members,
      invitations: processed_invitations,
      merge_requests: processed_merge_requests,
      submission_ids: processed_submission_ids,
      team_score: processed_team_score,
      rank: processed_rank,
      disbanded: disbanded,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      discussion_cids: processed_discussion_cids,
      notebook_ids: processed_notebook_ids,
      compute_quota_used: processed_compute_quota_used,
      total_submissions: total_submissions,
      best_submission_id: processed_best_submission_id,
      team_avatar_cid: processed_team_avatar_cid,
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

  defp process_string(:undefined), do: nil
  defp process_string(nil), do: nil
  defp process_string(value) when is_binary(value), do: value
  defp process_string(value) when is_list(value), do: to_string(value)
  defp process_string(_), do: nil

  defp process_cid(:undefined), do: nil
  defp process_cid(nil), do: nil
  defp process_cid(cid) when is_binary(cid), do: cid
  defp process_cid(cid) when is_list(cid), do: to_string(cid)
  defp process_cid(_), do: nil

  defp process_integer(:undefined), do: nil
  defp process_integer(nil), do: nil
  defp process_integer(value) when is_integer(value), do: value
  defp process_integer(_), do: nil

  defp process_float(:undefined), do: nil
  defp process_float(nil), do: nil
  defp process_float(value) when is_float(value), do: value
  defp process_float(value) when is_integer(value), do: value * 1.0
  defp process_float(_), do: nil

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

  defp process_members(members) when is_list(members) do
    Enum.map(members, fn
      {user_id, role, joined_at} ->
        %{
          user_id: to_string(user_id),
          role: atom_to_string(role),
          joined_at: handle_datetime(joined_at)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_members(:undefined), do: []
  defp process_members(nil), do: []
  defp process_members(_), do: []

  defp process_invitations(invitations) when is_list(invitations) do
    Enum.map(invitations, fn
      {user_id, status, invited_at, expires_at} ->
        %{
          user_id: to_string(user_id),
          status: atom_to_string(status),
          invited_at: handle_datetime(invited_at),
          expires_at: handle_datetime(expires_at)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_invitations(:undefined), do: []
  defp process_invitations(nil), do: []
  defp process_invitations(_), do: []

  defp process_merge_requests(requests) when is_list(requests) do
    Enum.map(requests, fn
      {from_team_id, status, requested_at} ->
        %{
          from_team_id: to_string(from_team_id),
          status: atom_to_string(status),
          requested_at: handle_datetime(requested_at)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_merge_requests(:undefined), do: []
  defp process_merge_requests(nil), do: []
  defp process_merge_requests(_), do: []

  defp atom_to_string(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp atom_to_string(string) when is_binary(string), do: string
  defp atom_to_string(list) when is_list(list), do: to_string(list)
  defp atom_to_string(_), do: nil

  def changeset(team, attrs \\ %{}) do
    team
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  def is_disbanded?(%__MODULE__{disbanded: true}), do: true
  def is_disbanded?(_), do: false

  def is_active?(%__MODULE__{disbanded: false}), do: true
  def is_active?(_), do: false

  def has_score?(%__MODULE__{team_score: score}) when not is_nil(score), do: true
  def has_score?(_), do: false

  def has_rank?(%__MODULE__{rank: rank}) when not is_nil(rank), do: true
  def has_rank?(_), do: false

  def has_avatar?(%__MODULE__{team_avatar_cid: cid})
      when not is_nil(cid) and cid != "",
      do: true

  def has_avatar?(_), do: false

  def member_count(%__MODULE__{members: members}) when is_list(members), do: length(members)
  def member_count(_), do: 0

  def submission_count(%__MODULE__{submission_ids: ids}) when is_list(ids), do: length(ids)
  def submission_count(_), do: 0

  def notebook_count(%__MODULE__{notebook_ids: ids}) when is_list(ids), do: length(ids)
  def notebook_count(_), do: 0

  def discussion_count(%__MODULE__{discussion_cids: ids}) when is_list(ids), do: length(ids)
  def discussion_count(_), do: 0

  def pending_invitation_count(%__MODULE__{invitations: invitations}) when is_list(invitations) do
    Enum.count(invitations, fn inv -> inv.status == "pending" end)
  end

  def pending_invitation_count(_), do: 0

  def merge_request_count(%__MODULE__{merge_requests: requests}) when is_list(requests),
    do: length(requests)

  def merge_request_count(_), do: 0

  def get_captains(%__MODULE__{members: members}) when is_list(members) do
    Enum.filter(members, fn member -> member.role == "captain" end)
  end

  def get_captains(_), do: []

  def get_regular_members(%__MODULE__{members: members}) when is_list(members) do
    Enum.filter(members, fn member -> member.role == "member" end)
  end

  def get_regular_members(_), do: []

  def is_member?(%__MODULE__{members: members}, user_id) when is_list(members) do
    Enum.any?(members, fn member -> member.user_id == user_id end)
  end

  def is_member?(_, _), do: false

  def is_captain?(%__MODULE__{members: members}, user_id) when is_list(members) do
    Enum.any?(members, fn member -> member.user_id == user_id and member.role == "captain" end)
  end

  def is_captain?(_, _), do: false

  def is_creator?(%__MODULE__{creator_id: creator_id}, user_id), do: creator_id == user_id
  def is_creator?(_, _), do: false

  def get_member_role(%__MODULE__{members: members}, user_id) when is_list(members) do
    case Enum.find(members, fn member -> member.user_id == user_id end) do
      nil -> nil
      member -> member.role
    end
  end

  def get_member_role(_, _), do: nil

  def is_archived?(%__MODULE__{metadata: %{"archived" => true}}), do: true
  def is_archived?(%__MODULE__{metadata: metadata}) when is_map(metadata) do
    Map.get(metadata, "archived", false)
  end
  def is_archived?(_), do: false

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

  def compute_resource_usage(%__MODULE__{compute_quota_used: quota}, resource_type)
      when is_map(quota) do
    Map.get(quota, resource_type, 0)
  end

  def compute_resource_usage(_, _), do: 0

  def total_compute_usage(%__MODULE__{compute_quota_used: quota}) when is_map(quota) do
    quota
    |> Map.values()
    |> Enum.sum()
  end

  def total_compute_usage(_), do: 0

  def pending_invitations(%__MODULE__{invitations: invitations}) when is_list(invitations) do
    now = NaiveDateTime.utc_now()

    Enum.filter(invitations, fn inv ->
      inv.status == "pending" and
        (is_nil(inv.expires_at) or NaiveDateTime.compare(now, inv.expires_at) == :lt)
    end)
  end

  def pending_invitations(_), do: []

  def expired_invitations(%__MODULE__{invitations: invitations}) when is_list(invitations) do
    now = NaiveDateTime.utc_now()

    Enum.filter(invitations, fn inv ->
      inv.status == "pending" and not is_nil(inv.expires_at) and
        NaiveDateTime.compare(now, inv.expires_at) == :gt
    end)
  end

  def expired_invitations(_), do: []

  def pending_merge_requests(%__MODULE__{merge_requests: requests}) when is_list(requests) do
    Enum.filter(requests, fn req -> req.status == "pending" end)
  end

  def pending_merge_requests(_), do: []

  def has_best_submission?(%__MODULE__{best_submission_id: id})
      when not is_nil(id) and id != "",
      do: true

  def has_best_submission?(_), do: false

  def time_since_creation(%__MODULE__{date_created: nil}), do: "N/A"

  def time_since_creation(%__MODULE__{date_created: date_created}) do
    now = NaiveDateTime.utc_now()
    diff_seconds = NaiveDateTime.diff(now, date_created)

    cond do
      diff_seconds < 86400 ->
        hours = div(diff_seconds, 3600)
        "#{hours} hour#{if hours != 1, do: "s", else: ""} ago"

      true ->
        days = div(diff_seconds, 86400)
        "#{days} day#{if days != 1, do: "s", else: ""} ago"
    end
  end

  def last_activity(%__MODULE__{date_updated: nil}), do: "N/A"

  def last_activity(%__MODULE__{date_updated: date_updated}) do
    now = NaiveDateTime.utc_now()
    diff_seconds = NaiveDateTime.diff(now, date_updated)

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
end
