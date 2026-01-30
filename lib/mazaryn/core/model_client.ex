defmodule Core.ModelClient do
  require Logger

  defp normalize_id_for_erlang(term) when is_binary(term) do
    str = to_string(term)

    if String.starts_with?(str, "id:") do
      String.to_charlist(str)
    else
      String.to_charlist("id:" <> str)
    end
  end

  defp normalize_id_for_erlang(term) when is_list(term) do
    str = List.to_string(term)

    if String.starts_with?(str, "id:") do
      term
    else
      String.to_charlist("id:" <> str)
    end
  end

  defp normalize_id_for_erlang(term) when is_atom(term) do
    str = Atom.to_string(term)

    if String.starts_with?(str, "id:") do
      String.to_charlist(str)
    else
      String.to_charlist("id:" <> str)
    end
  end

  def delete_model(model_id, user_id) do
    normalized_model_id = String.to_charlist(model_id)
    normalized_user_id = String.to_charlist(user_id)
    :modeldb.delete_model(normalized_model_id, normalized_user_id)
  end

  def create_model(
        creator_id,
        title,
        description,
        framework,
        task_type,
        model_file,
        license,
        tags,
        visibility,
        performance_metrics
      ) do
    :modeldb.create_model(
      normalize_id_for_erlang(creator_id),
      title,
      description,
      framework,
      task_type,
      model_file,
      license,
      tags,
      visibility,
      performance_metrics
    )
  end

  def create_model_concurrent(
        creator_id,
        title,
        description,
        framework,
        task_type,
        model_file,
        license,
        tags,
        visibility,
        performance_metrics
      ) do
    :modeldb.create_model_concurrent(
      normalize_id_for_erlang(creator_id),
      title,
      description,
      framework,
      task_type,
      model_file,
      license,
      tags,
      visibility,
      performance_metrics
    )
  end

  def create_model_from_file(
        creator_id,
        title,
        description,
        framework,
        task_type,
        file_path,
        license,
        tags,
        visibility
      ) do
    :modeldb.create_model_from_file(
      normalize_id_for_erlang(creator_id),
      title,
      description,
      framework,
      task_type,
      file_path,
      license,
      tags,
      visibility
    )
  end

  def update_model(
        model_id,
        creator_id,
        new_title,
        new_description,
        new_framework,
        new_task_type,
        new_model_file,
        new_license,
        new_tags,
        new_visibility,
        new_performance_metrics
      ) do
    :modeldb.update_model(
      normalize_id_for_erlang(model_id),
      normalize_id_for_erlang(creator_id),
      new_title,
      new_description,
      new_framework,
      new_task_type,
      new_model_file,
      new_license,
      new_tags,
      new_visibility,
      new_performance_metrics
    )
  end

  def upload_model_file(model_id, file_path) do
    :modeldb.upload_model_file(normalize_id_for_erlang(model_id), file_path)
  end

  def get_model_by_id(model_id) do
    :modeldb.get_model_by_id(normalize_id_for_erlang(model_id))
  end

  def get_models_by_creator(creator_id) do
    :modeldb.get_models_by_creator(normalize_id_for_erlang(creator_id))
  end

  def get_public_models() do
    :modeldb.get_public_models()
  end

  def get_models_by_framework(framework) do
    :modeldb.get_models_by_framework(framework)
  end

  def get_models_by_task(task_type) do
    :modeldb.get_models_by_task(task_type)
  end

  def get_models_by_tag(tag) do
    :modeldb.get_models_by_tag(tag)
  end

  def download_model(model_id, user_id) do
    Logger.info(
      "DEBUG: download_model called with model_id: #{inspect(model_id)}, user_id: #{inspect(user_id)}"
    )

    normalized_model_id = normalize_id_for_erlang(model_id)
    normalized_user_id = normalize_id_for_erlang(user_id)

    Logger.info("DEBUG: normalized_model_id: #{inspect(normalized_model_id)}")
    Logger.info("DEBUG: normalized_user_id: #{inspect(normalized_user_id)}")

    try do
      result = :modeldb.download_model(normalized_model_id, normalized_user_id)
      Logger.info("DEBUG: :modeldb.download_model result: #{inspect(result)}")

      case result do
        {:error, reason} ->
          Logger.error("DEBUG: download_model returned error: #{inspect(reason)}")
          {:error, reason}

        file_content when is_binary(file_content) ->
          Logger.info("DEBUG: Got binary file content of size: #{byte_size(file_content)} bytes")
          file_content

        file_content when is_list(file_content) ->
          Logger.info("DEBUG: Got list file content of length: #{length(file_content)}")
          :erlang.list_to_binary(file_content)

        other ->
          Logger.warn("DEBUG: Unexpected result type: #{inspect(other)}")
          other
      end
    rescue
      error ->
        Logger.error("DEBUG: Exception in download_model: #{inspect(error)}")
        Logger.error("DEBUG: Stacktrace: #{inspect(__STACKTRACE__)}")
        {:error, :exception}
    end
  end

  def create_model_version(model_id, user_id, new_model_file, change_description) do
    :modeldb.create_model_version(
      normalize_id_for_erlang(model_id),
      normalize_id_for_erlang(user_id),
      new_model_file,
      change_description
    )
  end

  def get_model_versions(model_id) do
    :modeldb.get_model_versions(normalize_id_for_erlang(model_id))
  end

  def get_version_by_number(model_id, version_num) do
    :modeldb.get_version_by_number(normalize_id_for_erlang(model_id), version_num)
  end

  def rollback_to_version(model_id, user_id, version_num) do
    :modeldb.rollback_to_version(
      normalize_id_for_erlang(model_id),
      normalize_id_for_erlang(user_id),
      version_num
    )
  end

  def add_training_dataset(model_id, dataset_cid) do
    :modeldb.add_training_dataset(normalize_id_for_erlang(model_id), dataset_cid)
  end

  def remove_training_dataset(model_id, dataset_cid) do
    :modeldb.remove_training_dataset(normalize_id_for_erlang(model_id), dataset_cid)
  end

  def get_training_datasets(model_id) do
    :modeldb.get_training_datasets(normalize_id_for_erlang(model_id))
  end

  def set_performance_metrics(model_id, metrics) do
    :modeldb.set_performance_metrics(normalize_id_for_erlang(model_id), metrics)
  end

  def get_performance_metrics(model_id) do
    :modeldb.get_performance_metrics(normalize_id_for_erlang(model_id))
  end

  def update_metric(model_id, metric_name, metric_value) do
    :modeldb.update_metric(normalize_id_for_erlang(model_id), metric_name, metric_value)
  end

  def deploy_model(model_id, deployment_config) do
    :modeldb.deploy_model(normalize_id_for_erlang(model_id), deployment_config)
  end

  def undeploy_model(model_id, user_id) do
    :modeldb.undeploy_model(normalize_id_for_erlang(model_id), normalize_id_for_erlang(user_id))
  end

  def get_deployment_info(model_id) do
    :modeldb.get_deployment_info(normalize_id_for_erlang(model_id))
  end

  def update_deployment_endpoint(model_id, new_endpoint) do
    :modeldb.update_deployment_endpoint(normalize_id_for_erlang(model_id), new_endpoint)
  end

  def add_benchmark_result(model_id, dataset_id, metric, score) do
    :modeldb.add_benchmark_result(normalize_id_for_erlang(model_id), dataset_id, metric, score)
  end

  def get_benchmark_results(model_id) do
    :modeldb.get_benchmark_results(normalize_id_for_erlang(model_id))
  end

  def get_best_benchmark(model_id, metric) do
    :modeldb.get_best_benchmark(normalize_id_for_erlang(model_id), metric)
  end

  def rate_model(model_id, user_id, rating) do
    :modeldb.rate_model(
      normalize_id_for_erlang(model_id),
      normalize_id_for_erlang(user_id),
      rating
    )
  end

  def get_model_rating(model_id) do
    :modeldb.get_model_rating(normalize_id_for_erlang(model_id))
  end

  def increment_download_count(model_id) do
    :modeldb.increment_download_count(normalize_id_for_erlang(model_id))
  end

  def track_inference_time(model_id, time_ms) do
    :modeldb.track_inference_time(normalize_id_for_erlang(model_id), time_ms)
  end

  def calculate_carbon_footprint(model_id, training_hours) do
    :modeldb.calculate_carbon_footprint(normalize_id_for_erlang(model_id), training_hours)
  end

  def search_models(query) do
    :modeldb.search_models(query)
  end

  def search_models_advanced(search_params) do
    :modeldb.search_models_advanced(search_params)
  end

  def get_trending_models(limit) do
    :modeldb.get_trending_models(limit)
  end

  def get_featured_models() do
    :modeldb.get_featured_models()
  end

  def get_models_by_performance(metric, min_score) do
    :modeldb.get_models_by_performance(metric, min_score)
  end

  def pin_model(model_id) do
    :modeldb.pin_model(normalize_id_for_erlang(model_id))
  end

  def unpin_model(model_id) do
    :modeldb.unpin_model(normalize_id_for_erlang(model_id))
  end

  def update_pin_status(model_id, pin_info) do
    :modeldb.update_pin_status(normalize_id_for_erlang(model_id), pin_info)
  end

  def report_model(reporter_id, model_id, type, description) do
    :modeldb.report_model(
      normalize_id_for_erlang(reporter_id),
      normalize_id_for_erlang(model_id),
      type,
      description
    )
  end

  def link_to_competition(model_id, competition_id) do
    :modeldb.link_to_competition(normalize_id_for_erlang(model_id), competition_id)
  end

  def unlink_from_competition(model_id, competition_id) do
    :modeldb.unlink_from_competition(normalize_id_for_erlang(model_id), competition_id)
  end

  def get_competition_models(competition_id) do
    :modeldb.get_competition_models(competition_id)
  end

  def export_model_card(model_id) do
    :modeldb.export_model_card(normalize_id_for_erlang(model_id))
  end

  def import_model_card(model_id, card_data) do
    :modeldb.import_model_card(normalize_id_for_erlang(model_id), card_data)
  end

  def validate_model_file(file_path) do
    :modeldb.validate_model_file(file_path)
  end

  def get_supported_frameworks() do
    :modeldb.get_supported_frameworks()
  end
end
