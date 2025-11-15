defmodule Mazaryn.Schema.Notebook do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Notebook
  """

  use Ecto.Schema
  alias Timex
  import Ecto.Changeset

  @optional_fields ~w(
    id
    title
    creator_id
    description
    content_cid
    dataset_cids
    competition_id
    submission_id
    language
    kernel_type
    environment
    collaborators
    version_cids
    outputs
    visibility
    execution_time_limit
    execution_count
    likes
    comments
    date_created
    date_updated
    report
    data
    forked_from
    fork_count
    execution_logs_cid
    dependencies_cid
    compute_time_used
    gpu_time_used
    scheduled_runs
    notebook_type
    citations
    interactive_widgets
  )a

  @required_fields ~w()a

  embedded_schema do
    field(:title, :string)
    field(:creator_id, :string)
    field(:description, :string)
    field(:content_cid, :string)
    field(:dataset_cids, {:array, :string}, default: [])
    field(:competition_id, :string)
    field(:submission_id, :string)
    field(:language, :string)
    field(:kernel_type, :string)
    field(:environment, :map, default: %{})
    field(:collaborators, {:array, :string}, default: [])
    field(:version_cids, {:array, :map}, default: [])
    field(:outputs, {:array, :map}, default: [])
    field(:visibility, :string, default: "public")
    field(:execution_time_limit, :integer)
    field(:execution_count, :integer, default: 0)
    field(:likes, {:array, :string}, default: [])
    field(:comments, {:array, :map}, default: [])
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
    field(:report, {:array, :string}, default: [])
    field(:data, :map, default: %{})
    field(:forked_from, :string)
    field(:fork_count, :integer, default: 0)
    field(:execution_logs_cid, :string)
    field(:dependencies_cid, :string)
    field(:compute_time_used, :integer, default: 0)
    field(:gpu_time_used, :integer, default: 0)
    field(:scheduled_runs, {:array, :map}, default: [])
    field(:notebook_type, :string, default: "analysis")
    field(:citations, {:array, :map}, default: [])
    field(:interactive_widgets, {:array, :map}, default: [])
  end

  def erl_changeset(
        {:notebook, id, title, creator_id, description, content_cid, dataset_cids, competition_id,
         submission_id, language, kernel_type, environment, collaborators, version_cids, outputs,
         visibility, execution_time_limit, execution_count, likes, comments, date_created,
         date_updated, report, data, forked_from, fork_count, execution_logs_cid,
         dependencies_cid, compute_time_used, gpu_time_used, scheduled_runs, notebook_type,
         citations, interactive_widgets}
      ) do
    processed_content_cid = process_cid(content_cid)
    processed_dataset_cids = process_string_list(dataset_cids)
    processed_competition_id = process_string(competition_id)
    processed_submission_id = process_string(submission_id)
    processed_language = atom_to_string(language)
    processed_kernel_type = atom_to_string(kernel_type)
    processed_environment = process_map(environment)
    processed_collaborators = process_string_list(collaborators)
    processed_version_cids = process_versions(version_cids)
    processed_outputs = process_list(outputs)
    processed_visibility = atom_to_string(visibility)
    processed_likes = process_string_list(likes)
    processed_comments = process_comments(comments)
    processed_report = process_string_list(report)
    processed_data = process_map(data)
    processed_forked_from = process_string(forked_from)
    processed_execution_logs_cid = process_cid(execution_logs_cid)
    processed_dependencies_cid = process_cid(dependencies_cid)
    processed_scheduled_runs = process_scheduled_runs(scheduled_runs)
    processed_notebook_type = atom_to_string(notebook_type)
    processed_citations = process_citations(citations)
    processed_interactive_widgets = process_widgets(interactive_widgets)

    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      title: to_string(title),
      creator_id: to_string(creator_id),
      description: to_string(description),
      content_cid: processed_content_cid,
      dataset_cids: processed_dataset_cids,
      competition_id: processed_competition_id,
      submission_id: processed_submission_id,
      language: processed_language,
      kernel_type: processed_kernel_type,
      environment: processed_environment,
      collaborators: processed_collaborators,
      version_cids: processed_version_cids,
      outputs: processed_outputs,
      visibility: processed_visibility,
      execution_time_limit: execution_time_limit,
      execution_count: execution_count,
      likes: processed_likes,
      comments: processed_comments,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      report: processed_report,
      data: processed_data,
      forked_from: processed_forked_from,
      fork_count: fork_count,
      execution_logs_cid: processed_execution_logs_cid,
      dependencies_cid: processed_dependencies_cid,
      compute_time_used: compute_time_used,
      gpu_time_used: gpu_time_used,
      scheduled_runs: processed_scheduled_runs,
      notebook_type: processed_notebook_type,
      citations: processed_citations,
      interactive_widgets: processed_interactive_widgets
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
  defp process_cid({:pending_update, _id}), do: nil
  defp process_cid(:undefined), do: nil
  defp process_cid(nil), do: nil
  defp process_cid(cid) when is_binary(cid), do: cid
  defp process_cid(cid) when is_list(cid), do: to_string(cid)
  defp process_cid(_), do: nil

  defp process_list(list) when is_list(list), do: list
  defp process_list(:undefined), do: []
  defp process_list(nil), do: []
  defp process_list(_), do: []

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

  defp process_versions(versions) when is_list(versions) do
    Enum.map(versions, fn
      {version_num, cid, timestamp, description} ->
        %{
          version_num: version_num,
          cid: process_cid(cid),
          timestamp: handle_datetime(timestamp),
          description: to_string(description)
        }

      {version_num, cid, timestamp} ->
        %{
          version_num: version_num,
          cid: process_cid(cid),
          timestamp: handle_datetime(timestamp),
          description: nil
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_versions(:undefined), do: []
  defp process_versions(nil), do: []
  defp process_versions(_), do: []

  defp process_comments(comments) when is_list(comments) do
    Enum.map(comments, fn
      {comment_id, user_id, content_cid, timestamp} ->
        %{
          comment_id: to_string(comment_id),
          user_id: to_string(user_id),
          content_cid: process_cid(content_cid),
          timestamp: handle_datetime(timestamp)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_comments(:undefined), do: []
  defp process_comments(nil), do: []
  defp process_comments(_), do: []

  defp process_scheduled_runs(runs) when is_list(runs) do
    Enum.map(runs, fn
      {schedule_id, cron_expression, enabled, created_at, last_run} ->
        %{
          schedule_id: to_string(schedule_id),
          cron_expression: to_string(cron_expression),
          enabled: enabled,
          created_at: handle_datetime(created_at),
          last_run: handle_datetime(last_run)
        }

      {cron_expression, enabled, last_run} ->
        %{
          schedule_id: nil,
          cron_expression: to_string(cron_expression),
          enabled: enabled,
          created_at: nil,
          last_run: handle_datetime(last_run)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_scheduled_runs(:undefined), do: []
  defp process_scheduled_runs(nil), do: []
  defp process_scheduled_runs(_), do: []

  defp process_citations(citations) when is_list(citations) do
    Enum.map(citations, fn
      {paper_doi, dataset_id, external_url} ->
        %{
          paper_doi: process_string(paper_doi),
          dataset_id: process_string(dataset_id),
          external_url: process_string(external_url)
        }

      citation when is_map(citation) ->
        citation

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_citations(:undefined), do: []
  defp process_citations(nil), do: []
  defp process_citations(_), do: []

  defp process_widgets(widgets) when is_list(widgets) do
    Enum.map(widgets, fn
      {widget_id, widget_type, widget_cid, position} ->
        %{
          widget_id: to_string(widget_id),
          widget_type: atom_to_string(widget_type),
          widget_cid: process_cid(widget_cid),
          position: position
        }

      {widget_type, widget_cid, position} ->
        %{
          widget_id: nil,
          widget_type: atom_to_string(widget_type),
          widget_cid: process_cid(widget_cid),
          position: position
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_widgets(:undefined), do: []
  defp process_widgets(nil), do: []
  defp process_widgets(_), do: []

  def changeset(notebook, attrs \\ %{}) do
    notebook
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  def is_public?(%__MODULE__{visibility: "public"}), do: true
  def is_public?(_), do: false

  def is_private?(%__MODULE__{visibility: "private"}), do: true
  def is_private?(_), do: false

  def is_fork?(%__MODULE__{forked_from: parent}) when not is_nil(parent) and parent != "" do
    true
  end

  def is_fork?(_), do: false

  def has_content?(%__MODULE__{content_cid: cid}) when not is_nil(cid) and cid != "" do
    true
  end

  def has_content?(_), do: false

  def has_dependencies?(%__MODULE__{dependencies_cid: cid})
      when not is_nil(cid) and cid != "" do
    true
  end

  def has_dependencies?(_), do: false

  def has_execution_logs?(%__MODULE__{execution_logs_cid: cid})
      when not is_nil(cid) and cid != "" do
    true
  end

  def has_execution_logs?(_), do: false

  def is_competition_submission?(%__MODULE__{notebook_type: "competition_submission"}), do: true
  def is_competition_submission?(_), do: false

  def is_tutorial?(%__MODULE__{notebook_type: "tutorial"}), do: true
  def is_tutorial?(_), do: false

  def is_analysis?(%__MODULE__{notebook_type: "analysis"}), do: true
  def is_analysis?(_), do: false

  def is_research?(%__MODULE__{notebook_type: "research"}), do: true
  def is_research?(_), do: false

  def like_count(%__MODULE__{likes: likes}) when is_list(likes) do
    length(likes)
  end

  def like_count(_), do: 0

  def comment_count(%__MODULE__{comments: comments}) when is_list(comments) do
    length(comments)
  end

  def comment_count(_), do: 0

  def version_count(%__MODULE__{version_cids: versions}) when is_list(versions) do
    length(versions)
  end

  def version_count(_), do: 0

  def collaborator_count(%__MODULE__{collaborators: collaborators}) when is_list(collaborators) do
    length(collaborators)
  end

  def collaborator_count(_), do: 0

  def dataset_count(%__MODULE__{dataset_cids: datasets}) when is_list(datasets) do
    length(datasets)
  end

  def dataset_count(_), do: 0

  def widget_count(%__MODULE__{interactive_widgets: widgets}) when is_list(widgets) do
    length(widgets)
  end

  def widget_count(_), do: 0

  def citation_count(%__MODULE__{citations: citations}) when is_list(citations) do
    length(citations)
  end

  def citation_count(_), do: 0

  def scheduled_run_count(%__MODULE__{scheduled_runs: runs}) when is_list(runs) do
    length(runs)
  end

  def scheduled_run_count(_), do: 0

  def has_user_liked?(%__MODULE__{likes: likes}, user_id) when is_list(likes) do
    Enum.member?(likes, user_id)
  end

  def has_user_liked?(_, _), do: false

  def is_collaborator?(%__MODULE__{collaborators: collaborators}, user_id)
      when is_list(collaborators) do
    Enum.member?(collaborators, user_id)
  end

  def is_collaborator?(_, _), do: false

  def is_owner?(%__MODULE__{creator_id: creator_id}, user_id) do
    creator_id == user_id
  end

  def is_owner?(_, _), do: false

  def can_edit?(%__MODULE__{} = notebook, user_id) do
    is_owner?(notebook, user_id) or is_collaborator?(notebook, user_id)
  end

  def can_edit?(_, _), do: false

  def format_compute_time(%__MODULE__{compute_time_used: seconds}) when is_integer(seconds) do
    cond do
      seconds >= 3600 ->
        hours = div(seconds, 3600)
        remaining_minutes = div(rem(seconds, 3600), 60)
        "#{hours}h #{remaining_minutes}m"

      seconds >= 60 ->
        minutes = div(seconds, 60)
        remaining_seconds = rem(seconds, 60)
        "#{minutes}m #{remaining_seconds}s"

      true ->
        "#{seconds}s"
    end
  end

  def format_compute_time(_), do: "0s"

  def format_gpu_time(%__MODULE__{gpu_time_used: seconds}) when is_integer(seconds) do
    cond do
      seconds >= 3600 ->
        hours = div(seconds, 3600)
        remaining_minutes = div(rem(seconds, 3600), 60)
        "#{hours}h #{remaining_minutes}m"

      seconds >= 60 ->
        minutes = div(seconds, 60)
        remaining_seconds = rem(seconds, 60)
        "#{minutes}m #{remaining_seconds}s"

      true ->
        "#{seconds}s"
    end
  end

  def format_gpu_time(_), do: "0s"

  def language_label(%__MODULE__{language: "python"}), do: "Python"
  def language_label(%__MODULE__{language: "r"}), do: "R"
  def language_label(%__MODULE__{language: "julia"}), do: "Julia"
  def language_label(%__MODULE__{language: "scala"}), do: "Scala"
  def language_label(%__MODULE__{language: "sql"}), do: "SQL"

  def language_label(%__MODULE__{language: lang}) when is_binary(lang),
    do: String.capitalize(lang)

  def language_label(_), do: "Unknown"

  def kernel_label(%__MODULE__{kernel_type: "python3"}), do: "Python 3"
  def kernel_label(%__MODULE__{kernel_type: "ir"}), do: "R"
  def kernel_label(%__MODULE__{kernel_type: "ijulia"}), do: "Julia"
  def kernel_label(%__MODULE__{kernel_type: kernel}) when is_binary(kernel), do: kernel
  def kernel_label(_), do: "Unknown"

  def type_label(%__MODULE__{notebook_type: "analysis"}), do: "Analysis"

  def type_label(%__MODULE__{notebook_type: "competition_submission"}),
    do: "Competition Submission"

  def type_label(%__MODULE__{notebook_type: "tutorial"}), do: "Tutorial"
  def type_label(%__MODULE__{notebook_type: "research"}), do: "Research"
  def type_label(_), do: "Unknown"

  def popularity_score(%__MODULE__{} = notebook) do
    like_count(notebook) + fork_count(notebook) * 2 + notebook.execution_count
  end

  def popularity_score(_), do: 0

  defp fork_count(%__MODULE__{fork_count: count}) when is_integer(count), do: count
  defp fork_count(_), do: 0

  def tags(%__MODULE__{data: data}) when is_map(data) do
    Map.get(data, "tags", [])
  end

  def tags(_), do: []

  def active_scheduled_runs(%__MODULE__{scheduled_runs: runs}) when is_list(runs) do
    Enum.filter(runs, fn run ->
      Map.get(run, :enabled, false) == true
    end)
  end

  def active_scheduled_runs(_), do: []

  def latest_version(%__MODULE__{version_cids: versions})
      when is_list(versions) and length(versions) > 0 do
    Enum.max_by(versions, fn v -> Map.get(v, :version_num, 0) end, fn -> nil end)
  end

  def latest_version(_), do: nil
end
