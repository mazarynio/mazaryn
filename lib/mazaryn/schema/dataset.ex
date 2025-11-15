defmodule Mazaryn.Schema.Dataset do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Dataset
  """

  use Ecto.Schema
  alias Timex
  import Ecto.Changeset

  @optional_fields ~w(
    id
    title
    description
    creator_id
    content_cid
    ipns
    metadata_cid
    size_bytes
    license
    version
    tags
    visibility
    downloads
    ratings
    pin_info
    competition_ids
    date_created
    date_updated
    report
    metadata
    version_history
    schema_cid
    sample_cid
    citation_count
    doi
    related_dataset_ids
    data_quality_score
    update_frequency
    access_requests
    collaborators
    used_in_notebook_ids
    used_in_model_ids
  )a

  @required_fields ~w()a

  embedded_schema do
    field(:title, :string)
    field(:description, :string)
    field(:creator_id, :string)
    field(:content_cid, :string)
    field(:ipns, :string)
    field(:metadata_cid, :string)
    field(:size_bytes, :integer, default: 0)
    field(:license, :string)
    field(:version, :string)
    field(:tags, {:array, :string}, default: [])
    field(:visibility, :string, default: "public")
    field(:downloads, :integer, default: 0)
    field(:ratings, {:array, :map}, default: [])
    field(:pin_info, {:array, :map}, default: [])
    field(:competition_ids, {:array, :string}, default: [])
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
    field(:report, {:array, :string}, default: [])
    field(:metadata, :map, default: %{})
    field(:version_history, {:array, :map}, default: [])
    field(:schema_cid, :string)
    field(:sample_cid, :string)
    field(:citation_count, :integer, default: 0)
    field(:doi, :string)
    field(:related_dataset_ids, {:array, :string}, default: [])
    field(:data_quality_score, :float, default: 0.0)
    field(:update_frequency, :string)
    field(:access_requests, {:array, :map}, default: [])
    field(:collaborators, {:array, :string}, default: [])
    field(:used_in_notebook_ids, {:array, :string}, default: [])
    field(:used_in_model_ids, {:array, :string}, default: [])
  end

  def erl_changeset(
        {:dataset, id, title, description, creator_id, content_cid, ipns, metadata_cid,
         size_bytes, license, version, tags, visibility, downloads, ratings, pin_info,
         competition_ids, date_created, date_updated, report, metadata, version_history,
         schema_cid, sample_cid, citation_count, doi, related_dataset_ids, data_quality_score,
         update_frequency, access_requests, collaborators, used_in_notebook_ids,
         used_in_model_ids}
      ) do
    processed_content_cid = process_cid(content_cid)
    processed_ipns = process_string(ipns)
    processed_metadata_cid = process_cid(metadata_cid)
    processed_schema_cid = process_cid(schema_cid)
    processed_sample_cid = process_cid(sample_cid)

    processed_tags = process_list(tags)
    processed_visibility = atom_to_string(visibility)
    processed_ratings = process_ratings(ratings)
    processed_pin_info = process_list(pin_info)
    processed_competition_ids = process_string_list(competition_ids)
    processed_report = process_string_list(report)
    processed_metadata = process_map(metadata)
    processed_version_history = process_version_history(version_history)
    processed_doi = process_string(doi)
    processed_related_dataset_ids = process_string_list(related_dataset_ids)
    processed_update_frequency = atom_to_string(update_frequency)
    processed_access_requests = process_access_requests(access_requests)
    processed_collaborators = process_string_list(collaborators)
    processed_used_in_notebook_ids = process_string_list(used_in_notebook_ids)
    processed_used_in_model_ids = process_string_list(used_in_model_ids)

    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      title: to_string(title),
      description: to_string(description),
      creator_id: to_string(creator_id),
      content_cid: processed_content_cid,
      ipns: processed_ipns,
      metadata_cid: processed_metadata_cid,
      size_bytes: size_bytes,
      license: to_string(license),
      version: to_string(version),
      tags: processed_tags,
      visibility: processed_visibility,
      downloads: downloads,
      ratings: processed_ratings,
      pin_info: processed_pin_info,
      competition_ids: processed_competition_ids,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      report: processed_report,
      metadata: processed_metadata,
      version_history: processed_version_history,
      schema_cid: processed_schema_cid,
      sample_cid: processed_sample_cid,
      citation_count: citation_count,
      doi: processed_doi,
      related_dataset_ids: processed_related_dataset_ids,
      data_quality_score: data_quality_score,
      update_frequency: processed_update_frequency,
      access_requests: processed_access_requests,
      collaborators: processed_collaborators,
      used_in_notebook_ids: processed_used_in_notebook_ids,
      used_in_model_ids: processed_used_in_model_ids
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
  defp process_cid({:pending_version, _id}), do: nil
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

  defp process_ratings(ratings) when is_list(ratings) do
    Enum.map(ratings, fn
      {user_id, rating, timestamp} ->
        %{
          user_id: to_string(user_id),
          rating: rating,
          timestamp: handle_datetime(timestamp)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_ratings(:undefined), do: []
  defp process_ratings(nil), do: []
  defp process_ratings(_), do: []

  defp process_version_history(history) when is_list(history) do
    Enum.map(history, fn
      {version_num, version_str, cid, timestamp, description} ->
        %{
          version_num: version_num,
          version: to_string(version_str),
          cid: process_cid(cid),
          timestamp: handle_datetime(timestamp),
          description: to_string(description)
        }

      {version_num, version_str, cid, timestamp} ->
        %{
          version_num: version_num,
          version: to_string(version_str),
          cid: process_cid(cid),
          timestamp: handle_datetime(timestamp),
          description: nil
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_version_history(:undefined), do: []
  defp process_version_history(nil), do: []
  defp process_version_history(_), do: []

  defp process_access_requests(requests) when is_list(requests) do
    Enum.map(requests, fn
      {request_id, user_id, reason, status, timestamp} ->
        %{
          request_id: to_string(request_id),
          user_id: to_string(user_id),
          reason: to_string(reason),
          status: atom_to_string(status),
          timestamp: handle_datetime(timestamp)
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_access_requests(:undefined), do: []
  defp process_access_requests(nil), do: []
  defp process_access_requests(_), do: []

  def changeset(dataset, attrs \\ %{}) do
    dataset
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  def average_rating(%__MODULE__{ratings: ratings})
      when is_list(ratings) and length(ratings) > 0 do
    total =
      ratings
      |> Enum.map(& &1.rating)
      |> Enum.sum()

    total / length(ratings)
  end

  def average_rating(_), do: 0.0

  def rating_count(%__MODULE__{ratings: ratings}) when is_list(ratings) do
    length(ratings)
  end

  def rating_count(_), do: 0

  def is_public?(%__MODULE__{visibility: "public"}), do: true
  def is_public?(_), do: false

  def is_private?(%__MODULE__{visibility: "private"}), do: true
  def is_private?(_), do: false

  def has_sample?(%__MODULE__{sample_cid: cid}) when not is_nil(cid) and cid != "", do: true
  def has_sample?(_), do: false

  def has_schema?(%__MODULE__{schema_cid: cid}) when not is_nil(cid) and cid != "", do: true
  def has_schema?(_), do: false

  def has_doi?(%__MODULE__{doi: doi}) when not is_nil(doi) and doi != "", do: true
  def has_doi?(_), do: false

  def is_collaborator?(%__MODULE__{collaborators: collaborators}, user_id)
      when is_list(collaborators) do
    Enum.member?(collaborators, user_id)
  end

  def is_collaborator?(_, _), do: false

  def total_usage(%__MODULE__{
        used_in_notebook_ids: notebooks,
        used_in_model_ids: models,
        competition_ids: competitions
      }) do
    length(notebooks) + length(models) + length(competitions)
  end

  def total_usage(_), do: 0

  def format_size(%__MODULE__{size_bytes: size}) when is_integer(size) do
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

  def quality_tier(%__MODULE__{data_quality_score: score}) when is_number(score) do
    cond do
      score >= 90 -> :excellent
      score >= 75 -> :good
      score >= 50 -> :average
      score >= 25 -> :below_average
      true -> :poor
    end
  end

  def quality_tier(_), do: :unknown

  def pending_access_requests(%__MODULE__{access_requests: requests}) when is_list(requests) do
    Enum.filter(requests, fn request ->
      request.status == "pending"
    end)
  end

  def pending_access_requests(_), do: []
end
