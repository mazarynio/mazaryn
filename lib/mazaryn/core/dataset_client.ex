defmodule Core.DatasetClient do
  def create_dataset(
        creator_id,
        title,
        description,
        content,
        metadata_map,
        license,
        tags,
        visibility
      ) do
    :datasetdb.create_dataset(
      creator_id,
      title,
      description,
      content,
      metadata_map,
      license,
      tags,
      visibility
    )
  end

  def create_dataset_concurrent(
        creator_id,
        title,
        description,
        content,
        metadata_map,
        license,
        tags,
        visibility
      ) do
    :datasetdb.create_dataset_concurrent(
      creator_id,
      title,
      description,
      content,
      metadata_map,
      license,
      tags,
      visibility
    )
  end

  def create_dataset_from_file(
        creator_id,
        title,
        description,
        file_path,
        license,
        tags,
        visibility
      ) do
    :datasetdb.create_dataset_from_file(
      creator_id,
      title,
      description,
      file_path,
      license,
      tags,
      visibility
    )
  end

  def create_dataset_from_zip(
        creator_id,
        title,
        description,
        zip_file_path,
        license,
        tags,
        visibility
      ) do
    :datasetdb.create_dataset_from_zip(
      creator_id,
      title,
      description,
      zip_file_path,
      license,
      tags,
      visibility
    )
  end

  def create_dataset_from_url(creator_id, title, description, url, license, tags, visibility) do
    :datasetdb.create_dataset_from_url(
      creator_id,
      title,
      description,
      url,
      license,
      tags,
      visibility
    )
  end

  def update_dataset(
        dataset_id,
        creator_id,
        new_title,
        new_description,
        new_content,
        new_metadata,
        new_license,
        new_tags,
        new_visibility
      ) do
    :datasetdb.update_dataset(
      dataset_id,
      creator_id,
      new_title,
      new_description,
      new_content,
      new_metadata,
      new_license,
      new_tags,
      new_visibility
    )
  end

  def delete_dataset(dataset_id, user_id) do
    :datasetdb.delete_dataset(dataset_id, user_id)
  end

  def upload_dataset_file(dataset_id, file_path) do
    :datasetdb.upload_dataset_file(dataset_id, file_path)
  end

  def get_dataset_by_id(dataset_id) do
    :datasetdb.get_dataset_by_id(dataset_id)
  end

  def get_datasets_by_creator(creator_id) do
    :datasetdb.get_datasets_by_creator(creator_id)
  end

  def get_public_datasets() do
    :datasetdb.get_public_datasets()
  end

  def get_datasets_by_tag(tag) do
    :datasetdb.get_datasets_by_tag(tag)
  end

  def get_dataset_content(dataset_id) do
    :datasetdb.get_dataset_content(dataset_id)
  end

  def get_dataset_sample(dataset_id) do
    :datasetdb.get_dataset_sample(dataset_id)
  end

  def get_dataset_schema(dataset_id) do
    :datasetdb.get_dataset_schema(dataset_id)
  end

  def get_dataset_metadata(dataset_id) do
    :datasetdb.get_dataset_metadata(dataset_id)
  end

  def create_dataset_version(dataset_id, user_id, new_content, change_description) do
    :datasetdb.create_dataset_version(dataset_id, user_id, new_content, change_description)
  end

  def get_dataset_versions(dataset_id) do
    :datasetdb.get_dataset_versions(dataset_id)
  end

  def get_version_by_number(dataset_id, version_num) do
    :datasetdb.get_version_by_number(dataset_id, version_num)
  end

  def rollback_to_version(dataset_id, user_id, version_num) do
    :datasetdb.rollback_to_version(dataset_id, user_id, version_num)
  end

  def request_dataset_access(dataset_id, user_id, reason) do
    :datasetdb.request_dataset_access(dataset_id, user_id, reason)
  end

  def approve_access_request(dataset_id, creator_id, request_id) do
    :datasetdb.approve_access_request(dataset_id, creator_id, request_id)
  end

  def reject_access_request(dataset_id, creator_id, request_id) do
    :datasetdb.reject_access_request(dataset_id, creator_id, request_id)
  end

  def add_collaborator(dataset_id, creator_id, collaborator_id) do
    :datasetdb.add_collaborator(dataset_id, creator_id, collaborator_id)
  end

  def remove_collaborator(dataset_id, creator_id, collaborator_id) do
    :datasetdb.remove_collaborator(dataset_id, creator_id, collaborator_id)
  end

  def get_dataset_collaborators(dataset_id) do
    :datasetdb.get_dataset_collaborators(dataset_id)
  end

  def rate_dataset(dataset_id, user_id, rating) do
    :datasetdb.rate_dataset(dataset_id, user_id, rating)
  end

  def get_dataset_rating(dataset_id) do
    :datasetdb.get_dataset_rating(dataset_id)
  end

  def calculate_quality_score(dataset_id) do
    :datasetdb.calculate_quality_score(dataset_id)
  end

  def validate_dataset_schema(dataset_id, expected_schema) do
    :datasetdb.validate_dataset_schema(dataset_id, expected_schema)
  end

  def link_dataset_to_competition(dataset_id, competition_id) do
    :datasetdb.link_dataset_to_competition(dataset_id, competition_id)
  end

  def unlink_dataset_from_competition(dataset_id, competition_id) do
    :datasetdb.unlink_dataset_from_competition(dataset_id, competition_id)
  end

  def get_competitions_using_dataset(dataset_id) do
    :datasetdb.get_competitions_using_dataset(dataset_id)
  end

  def link_related_datasets(dataset_id, related_dataset_id) do
    :datasetdb.link_related_datasets(dataset_id, related_dataset_id)
  end

  def get_related_datasets(dataset_id) do
    :datasetdb.get_related_datasets(dataset_id)
  end

  def track_notebook_usage(dataset_id, notebook_id) do
    :datasetdb.track_notebook_usage(dataset_id, notebook_id)
  end

  def track_model_usage(dataset_id, model_id) do
    :datasetdb.track_model_usage(dataset_id, model_id)
  end

  def get_dataset_usage_stats(dataset_id) do
    :datasetdb.get_dataset_usage_stats(dataset_id)
  end

  def increment_download_count(dataset_id) do
    :datasetdb.increment_download_count(dataset_id)
  end

  def search_datasets(query) do
    :datasetdb.search_datasets(query)
  end

  def search_datasets_advanced(search_params) do
    :datasetdb.search_datasets_advanced(search_params)
  end

  def get_trending_datasets(limit) do
    :datasetdb.get_trending_datasets(limit)
  end

  def get_featured_datasets() do
    :datasetdb.get_featured_datasets()
  end

  def generate_doi(dataset_id) do
    :datasetdb.generate_doi(dataset_id)
  end

  def increment_citation_count(dataset_id) do
    :datasetdb.increment_citation_count(dataset_id)
  end

  def get_citation_info(dataset_id) do
    :datasetdb.get_citation_info(dataset_id)
  end

  def pin_dataset(dataset_id) do
    :datasetdb.pin_dataset(dataset_id)
  end

  def unpin_dataset(dataset_id) do
    :datasetdb.unpin_dataset(dataset_id)
  end

  def update_pin_status(dataset_id, pin_info) do
    :datasetdb.update_pin_status(dataset_id, pin_info)
  end

  def report_dataset(reporter_id, dataset_id, type, description) do
    :datasetdb.report_dataset(reporter_id, dataset_id, type, description)
  end

  def schedule_dataset_update(dataset_id, frequency, cron_expression) do
    :datasetdb.schedule_dataset_update(dataset_id, frequency, cron_expression)
  end

  def get_dataset_update_schedule(dataset_id) do
    :datasetdb.get_dataset_update_schedule(dataset_id)
  end

  def validate_dataset_file(file_path) do
    :datasetdb.validate_dataset_file(file_path)
  end

  def get_supported_formats() do
    :datasetdb.get_supported_formats()
  end

  def extract_zip_metadata(zip_content) do
    :datasetdb.extract_zip_metadata(zip_content)
  end

  def download_dataset(dataset_id, user_id) do
    :datasetdb.download_dataset(dataset_id, user_id)
  end

  def download_dataset(dataset_id, user_id, options) do
    :datasetdb.download_dataset(dataset_id, user_id, options)
  end

  def schedule_dataset_download(dataset_id, user_id, schedule_time) do
    :datasetdb.schedule_dataset_download(dataset_id, user_id, schedule_time)
  end

  def get_dataset_download_status(download_id) do
     :datasetdb.get_dataset_download_status(download_id)
  end

  def cancel_dataset_download(download_id) do
    :datasetdb.cancel_dataset_download(download_id)
  end

  def calculate_dela(schedule_time) do
    :datasetdb.calculate_dela(schedule_time)
  end
end
