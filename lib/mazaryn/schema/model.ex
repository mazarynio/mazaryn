defmodule Mazaryn.Schema.Model do
  use Ecto.Schema
  import Ecto.Changeset

  @optional_fields ~w(
    id
    creator_id
    title
    description
    framework
    task_type
    file_cid
    size_bytes
    license
    tags
    visibility
    downloads
    pin_info
    deployment_info
    date_created
    date_updated
    report
    data
    training_dataset_cids
    performance_metrics
    inference_api_endpoint
    docker_image_cid
    model_card_cid
    version_history
    benchmark_results
    dependencies_cid
    inference_time_ms
    carbon_footprint
  )a

  @required_fields ~w()a

  embedded_schema do
    field(:creator_id, :string)
    field(:title, :string)
    field(:description, :string)
    field(:framework, :string)
    field(:task_type, :string)
    field(:file_cid, :string)
    field(:size_bytes, :integer, default: 0)
    field(:license, :string)
    field(:tags, {:array, :string}, default: [])
    field(:visibility, :string, default: "public")
    field(:downloads, :integer, default: 0)
    field(:pin_info, {:array, :map}, default: [])
    field(:deployment_info, :map, default: %{})
    field(:date_created, :naive_datetime)
    field(:date_updated, :naive_datetime)
    field(:report, {:array, :string}, default: [])
    field(:data, :map, default: %{})
    field(:training_dataset_cids, {:array, :string}, default: [])
    field(:performance_metrics, :map, default: %{})
    field(:inference_api_endpoint, :string)
    field(:docker_image_cid, :string)
    field(:model_card_cid, :string)
    field(:version_history, {:array, :map}, default: [])
    field(:benchmark_results, {:array, :map}, default: [])
    field(:dependencies_cid, :string)
    field(:inference_time_ms, :integer, default: 0)
    field(:carbon_footprint, :float, default: 0.0)
  end

  def erl_changeset(
        {:model, id, creator_id, title, description, framework, task_type, file_cid, size_bytes,
         license, tags, visibility, downloads, pin_info, deployment_info, date_created,
         date_updated, report, data, training_dataset_cids, performance_metrics,
         inference_api_endpoint, docker_image_cid, model_card_cid, version_history,
         benchmark_results, dependencies_cid, inference_time_ms, carbon_footprint}
      ) do
    processed_file_cid = process_cid(file_cid)
    processed_tags = process_string_list(tags)
    processed_visibility = atom_to_string(visibility)
    processed_framework = process_string(framework)
    processed_task_type = atom_to_string(task_type)
    processed_pin_info = process_list(pin_info)
    processed_deployment_info = process_map(deployment_info)
    processed_report = process_string_list(report)
    processed_data = process_map(data)
    processed_training_dataset_cids = process_string_list(training_dataset_cids)
    processed_performance_metrics = process_map(performance_metrics)
    processed_inference_api_endpoint = process_string(inference_api_endpoint)
    processed_docker_image_cid = process_cid(docker_image_cid)
    processed_model_card_cid = process_cid(model_card_cid)
    processed_version_history = process_version_history(version_history)
    processed_benchmark_results = process_benchmark_results(benchmark_results)
    processed_dependencies_cid = process_cid(dependencies_cid)

    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      creator_id: to_string(creator_id),
      title: to_string(title),
      description: to_string(description),
      framework: processed_framework,
      task_type: processed_task_type,
      file_cid: processed_file_cid,
      size_bytes: size_bytes,
      license: to_string(license),
      tags: processed_tags,
      visibility: processed_visibility,
      downloads: downloads,
      pin_info: processed_pin_info,
      deployment_info: processed_deployment_info,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      report: processed_report,
      data: processed_data,
      training_dataset_cids: processed_training_dataset_cids,
      performance_metrics: processed_performance_metrics,
      inference_api_endpoint: processed_inference_api_endpoint,
      docker_image_cid: processed_docker_image_cid,
      model_card_cid: processed_model_card_cid,
      version_history: processed_version_history,
      benchmark_results: processed_benchmark_results,
      dependencies_cid: processed_dependencies_cid,
      inference_time_ms: inference_time_ms,
      carbon_footprint: carbon_footprint
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

  defp process_benchmark_results(results) when is_list(results) do
    Enum.map(results, fn
      {dataset_id, metric, score} ->
        %{
          dataset_id: to_string(dataset_id),
          metric: atom_to_string(metric),
          score: score
        }

      _ ->
        nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_benchmark_results(:undefined), do: []
  defp process_benchmark_results(nil), do: []
  defp process_benchmark_results(_), do: []

  def changeset(model, attrs \\ %{}) do
    model
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

  def is_deployed?(%__MODULE__{deployment_info: info}) when map_size(info) > 0, do: true
  def is_deployed?(_), do: false

  def has_endpoint?(%__MODULE__{inference_api_endpoint: endpoint})
      when not is_nil(endpoint) and endpoint != "",
      do: true

  def has_endpoint?(_), do: false

  def has_model_card?(%__MODULE__{model_card_cid: cid})
      when not is_nil(cid) and cid != "",
      do: true

  def has_model_card?(_), do: false

  def has_docker_image?(%__MODULE__{docker_image_cid: cid})
      when not is_nil(cid) and cid != "",
      do: true

  def has_docker_image?(_), do: false

  def average_rating(%__MODULE__{data: %{"ratings" => ratings}})
      when is_list(ratings) and length(ratings) > 0 do
    total =
      ratings
      |> Enum.map(fn {_, rating, _} -> rating end)
      |> Enum.sum()

    total / length(ratings)
  end

  def average_rating(_), do: 0.0

  def rating_count(%__MODULE__{data: %{"ratings" => ratings}}) when is_list(ratings) do
    length(ratings)
  end

  def rating_count(_), do: 0

  def training_datasets_count(%__MODULE__{training_dataset_cids: datasets})
      when is_list(datasets) do
    length(datasets)
  end

  def training_datasets_count(_), do: 0

  def benchmark_count(%__MODULE__{benchmark_results: benchmarks}) when is_list(benchmarks) do
    length(benchmarks)
  end

  def benchmark_count(_), do: 0

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

  def format_inference_time(%__MODULE__{inference_time_ms: time}) when is_integer(time) do
    cond do
      time >= 1000 ->
        "#{Float.round(time / 1000, 2)} s"

      true ->
        "#{time} ms"
    end
  end

  def format_inference_time(_), do: "0 ms"

  def format_carbon_footprint(%__MODULE__{carbon_footprint: footprint})
      when is_number(footprint) do
    cond do
      footprint >= 1000 ->
        "#{Float.round(footprint / 1000, 2)} t CO₂"

      true ->
        "#{Float.round(footprint, 2)} kg CO₂"
    end
  end

  def format_carbon_footprint(_), do: "0 kg CO₂"

  def get_metric(%__MODULE__{performance_metrics: metrics}, metric_name)
      when is_map(metrics) do
    Map.get(metrics, metric_name)
  end

  def get_metric(_, _), do: nil

  def deployment_status(%__MODULE__{deployment_info: info}) when is_map(info) do
    Map.get(info, "status", "not_deployed")
  end

  def deployment_status(_), do: "not_deployed"

  def deployment_endpoint(%__MODULE__{deployment_info: info}) when is_map(info) do
    Map.get(info, "endpoint_url")
  end

  def deployment_endpoint(%__MODULE__{inference_api_endpoint: endpoint}), do: endpoint
  def deployment_endpoint(_), do: nil

  def version_count(%__MODULE__{version_history: history}) when is_list(history) do
    length(history)
  end

  def version_count(_), do: 0

  def latest_version(%__MODULE__{version_history: [latest | _]}) when is_map(latest) do
    latest
  end

  def latest_version(_), do: nil

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

  def best_benchmark(%__MODULE__{benchmark_results: benchmarks}, metric)
      when is_list(benchmarks) and length(benchmarks) > 0 do
    benchmarks
    |> Enum.filter(fn b -> b.metric == metric end)
    |> Enum.max_by(fn b -> b.score end, fn -> nil end)
  end

  def best_benchmark(_, _), do: nil

  def competitions(%__MODULE__{data: %{"competitions" => competitions}})
      when is_list(competitions) do
    competitions
  end

  def competitions(_), do: []

  def competitions_count(%__MODULE__{} = model) do
    model |> competitions() |> length()
  end
end
