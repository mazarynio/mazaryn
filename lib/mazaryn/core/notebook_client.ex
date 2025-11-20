defmodule Core.NotebookClient do
  def create_notebook(
        creator_id,
        title,
        description,
        language,
        kernel_type,
        dataset_cids,
        competition_id,
        environment,
        tags,
        visibility
      ) do
    :notebookdb.create_notebook(
      creator_id,
      title,
      description,
      language,
      kernel_type,
      dataset_cids,
      competition_id,
      environment,
      tags,
      visibility
    )
  end

  def update_notebook(
        notebook_id,
        user_id,
        new_title,
        new_description,
        new_language,
        new_kernel_type,
        new_dataset_cids,
        new_competition_id,
        new_environment,
        new_tags,
        new_visibility
      ) do
    :notebookdb.update_notebook(
      notebook_id,
      user_id,
      new_title,
      new_description,
      new_language,
      new_kernel_type,
      new_dataset_cids,
      new_competition_id,
      new_environment,
      new_tags,
      new_visibility
    )
  end

  def delete_notebook(notebook_id, user_id) do
    :notebookdb.delete_notebook(notebook_id, user_id)
  end

  def get_notebook_by_id(notebook_id) do
    :notebookdb.get_notebook_by_id(notebook_id)
  end

  def get_notebooks_by_creator(creator_id) do
    :notebookdb.get_notebooks_by_creator(creator_id)
  end

  def get_public_notebooks() do
    :notebookdb.get_public_notebooks()
  end

  def get_notebooks_by_language(language) do
    :notebookdb.get_notebooks_by_language(language)
  end

  def get_notebooks_by_kernel(kernel_type) do
    :notebookdb.get_notebooks_by_kernel(kernel_type)
  end

  def get_notebooks_by_tag(tag) do
    :notebookdb.get_notebooks_by_tag(tag)
  end

  def get_notebook_content(notebook_id) do
    :notebookdb.get_notebook_content(notebook_id)
  end

  def update_notebook_content(notebook_id, user_id, new_content) do
    :notebookdb.update_notebook_content(notebook_id, user_id, new_content)
  end

  def get_notebook_outputs(notebook_id) do
    :notebookdb.get_notebook_outputs(notebook_id)
  end

  def save_notebook_outputs(notebook_id, outputs) do
    :notebookdb.save_notebook_outputs(notebook_id, outputs)
  end

  def add_dataset_to_notebook(notebook_id, dataset_cid) do
    :notebookdb.add_dataset_to_notebook(notebook_id, dataset_cid)
  end

  def remove_dataset_from_notebook(notebook_id, dataset_cid) do
    :notebookdb.remove_dataset_from_notebook(notebook_id, dataset_cid)
  end

  def get_notebook_datasets(notebook_id) do
    :notebookdb.get_notebook_datasets(notebook_id)
  end

  def link_notebook_to_competition(notebook_id, competition_id) do
    :notebookdb.link_notebook_to_competition(notebook_id, competition_id)
  end

  def unlink_notebook_from_competition(notebook_id, competition_id) do
    :notebookdb.unlink_notebook_from_competition(notebook_id, competition_id)
  end

  def get_competition_notebooks(competition_id) do
    :notebookdb.get_competition_notebooks(competition_id)
  end

  def add_collaborator(notebook_id, owner_id, collaborator_id) do
    :notebookdb.add_collaborator(notebook_id, owner_id, collaborator_id)
  end

  def remove_collaborator(notebook_id, owner_id, collaborator_id) do
    :notebookdb.remove_collaborator(notebook_id, owner_id, collaborator_id)
  end

  def get_notebook_collaborators(notebook_id) do
    :notebookdb.get_notebook_collaborators(notebook_id)
  end

  def is_collaborator(notebook_id, user_id) do
    :notebookdb.is_collaborator(notebook_id, user_id)
  end

  def create_version(notebook_id, user_id, change_description) do
    :notebookdb.create_version(notebook_id, user_id, change_description)
  end

  def get_notebook_versions(notebook_id) do
    :notebookdb.get_notebook_versions(notebook_id)
  end

  def get_version_by_number(notebook_id, version_num) do
    :notebookdb.get_version_by_number(notebook_id, version_num)
  end

  def rollback_to_version(notebook_id, user_id, version_num) do
    :notebookdb.rollback_to_version(notebook_id, user_id, version_num)
  end

  def compare_versions(notebook_id, version_num1, version_num2) do
    :notebookdb.compare_versions(notebook_id, version_num1, version_num2)
  end

  def fork_notebook(notebook_id, user_id) do
    :notebookdb.fork_notebook(notebook_id, user_id)
  end

  def get_notebook_forks(notebook_id) do
    :notebookdb.get_notebook_forks(notebook_id)
  end

  def get_fork_parent(notebook_id) do
    :notebookdb.get_fork_parent(notebook_id)
  end

  def increment_fork_count(notebook_id) do
    :notebookdb.increment_fork_count(notebook_id)
  end

  def execute_notebook(notebook_id, user_id) do
    :notebookdb.execute_notebook(notebook_id, user_id)
  end

  def get_execution_logs(notebook_id) do
    :notebookdb.get_execution_logs(notebook_id)
  end

  def get_execution_stats(notebook_id) do
    :notebookdb.get_execution_stats(notebook_id)
  end

  def schedule_notebook_run(notebook_id, user_id, cron_expression) do
    :notebookdb.schedule_notebook_run(notebook_id, user_id, cron_expression)
  end

  def cancel_scheduled_run(notebook_id, schedule_id) do
    :notebookdb.cancel_scheduled_run(notebook_id, schedule_id)
  end

  def get_scheduled_runs(notebook_id) do
    :notebookdb.get_scheduled_runs(notebook_id)
  end

  def like_notebook(notebook_id, user_id) do
    :notebookdb.like_notebook(notebook_id, user_id)
  end

  def unlike_notebook(notebook_id, user_id) do
    :notebookdb.unlike_notebook(notebook_id, user_id)
  end

  def get_notebook_likes(notebook_id) do
    :notebookdb.get_notebook_likes(notebook_id)
  end

  def has_user_liked(notebook_id, user_id) do
    :notebookdb.has_user_liked(notebook_id, user_id)
  end

  def add_comment(notebook_id, user_id, content) do
    :notebookdb.add_comment(notebook_id, user_id, content)
  end

  def get_notebook_comments(notebook_id) do
    :notebookdb.get_notebook_comments(notebook_id)
  end

  def delete_comment(notebook_id, comment_id, user_id) do
    :notebookdb.delete_comment(notebook_id, comment_id, user_id)
  end

  def update_comment(notebook_id, comment_id, new_content) do
    :notebookdb.update_comment(notebook_id, comment_id, new_content)
  end

  def set_notebook_environment(notebook_id, environment) do
    :notebookdb.set_notebook_environment(notebook_id, environment)
  end

  def get_notebook_environment(notebook_id) do
    :notebookdb.get_notebook_environment(notebook_id)
  end

  def set_dependencies(notebook_id, dependencies) do
    :notebookdb.set_dependencies(notebook_id, dependencies)
  end

  def get_dependencies(notebook_id) do
    :notebookdb.get_dependencies(notebook_id)
  end

  def add_citation(notebook_id, citation) do
    :notebookdb.add_citation(notebook_id, citation)
  end

  def get_notebook_citations(notebook_id) do
    :notebookdb.get_notebook_citations(notebook_id)
  end

  def remove_citation(notebook_id, citation) do
    :notebookdb.remove_citation(notebook_id, citation)
  end

  def add_interactive_widget(notebook_id, widget_type, widget_cid) do
    :notebookdb.add_interactive_widget(notebook_id, widget_type, widget_cid)
  end

  def remove_interactive_widget(notebook_id, widget_id) do
    :notebookdb.remove_interactive_widget(notebook_id, widget_id)
  end

  def get_notebook_widgets(notebook_id) do
    :notebookdb.get_notebook_widgets(notebook_id)
  end

  def set_notebook_type(notebook_id, notebook_type) do
    :notebookdb.set_notebook_type(notebook_id, notebook_type)
  end

  def get_notebooks_by_type(notebook_type) do
    :notebookdb.get_notebooks_by_type(notebook_type)
  end

  def search_notebooks(query) do
    :notebookdb.search_notebooks(query)
  end

  def search_notebooks_advanced(search_params) do
    :notebookdb.search_notebooks_advanced(search_params)
  end

  def get_trending_notebooks(limit) do
    :notebookdb.get_trending_notebooks(limit)
  end

  def get_featured_notebooks() do
    :notebookdb.get_featured_notebooks()
  end

  def get_most_forked_notebooks(limit) do
    :notebookdb.get_most_forked_notebooks(limit)
  end

  def get_most_liked_notebooks(limit) do
    :notebookdb.get_most_liked_notebooks(limit)
  end

  def report_notebook(reporter_id, notebook_id, type, description) do
    :notebookdb.report_notebook(reporter_id, notebook_id, type, description)
  end

  def get_notebook_stats(notebook_id) do
    :notebookdb.get_notebook_stats(notebook_id)
  end

  def get_user_notebook_stats(user_id) do
    :notebookdb.get_user_notebook_stats(user_id)
  end

  def pin_notebook(notebook_id) do
    :notebookdb.pin_notebook(notebook_id)
  end

  def unpin_notebook(notebook_id) do
    :notebookdb.unpin_notebook(notebook_id)
  end

  def submit_notebook_to_competition(notebook_id, user_id, competition_id) do
    :notebookdb.submit_notebook_to_competition(notebook_id, user_id, competition_id)
  end

  def get_submission_from_notebook(notebook_id) do
    :notebookdb.get_submission_from_notebook(notebook_id)
  end
end
