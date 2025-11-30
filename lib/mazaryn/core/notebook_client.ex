defmodule Core.NotebookClient do
  require Logger

  defp to_charlist(value) when is_binary(value), do: String.to_charlist(value)
  defp to_charlist(value) when is_list(value), do: value
  defp to_charlist(value), do: value

  defp ensure_charlist(value) when is_binary(value) do
    Logger.debug("Converting binary to charlist: #{value}")
    String.to_charlist(value)
  end

  defp ensure_charlist(value) when is_list(value) do
    Logger.debug("Already charlist: #{inspect(value)}")
    value
  end

  defp ensure_charlist(value) do
    Logger.warning("Unexpected value type: #{inspect(value)}")
    value
  end

  def create_notebook(
        token,
        title,
        description,
        language,
        kernel_type,
        tags,
        dataset_cids,
        environment,
        collaborators,
        visibility
      ) do
    Logger.info("Creating notebook: #{title}")
    token_charlist = ensure_charlist(token)

    result =
      :notebookdb.create_notebook(
        token_charlist,
        title,
        description,
        language,
        kernel_type,
        tags,
        dataset_cids,
        environment,
        collaborators,
        visibility
      )

    case result do
      id when is_list(id) ->
        binary_id = to_string(id)
        Logger.info("Notebook created with ID: #{binary_id}")
        binary_id

      error ->
        error
    end
  end

  def get_notebook_by_id(notebook_id) do
    Logger.info("Getting notebook by ID: #{inspect(notebook_id)}")
    id_charlist = ensure_charlist(notebook_id)

    case :notebookdb.get_notebook_by_id(id_charlist) do
      {:error, :notebook_not_found} = error ->
        Logger.error("Notebook not found: #{inspect(notebook_id)}")
        error

      notebook when is_tuple(notebook) ->
        Logger.info("Notebook found")
        notebook

      error ->
        Logger.error("Error getting notebook: #{inspect(error)}")
        error
    end
  end

  def update_notebook(
        notebook_id,
        token,
        title,
        description,
        language,
        kernel_type,
        tags,
        dataset_cids,
        environment,
        collaborators,
        visibility
      ) do
    Logger.info("Updating notebook: #{inspect(notebook_id)}")

    :notebookdb.update_notebook(
      ensure_charlist(notebook_id),
      ensure_charlist(token),
      title,
      description,
      language,
      kernel_type,
      tags,
      dataset_cids,
      environment,
      collaborators,
      visibility
    )
  end

  def delete_notebook(notebook_id, user_id) do
    Logger.info("Deleting notebook: #{inspect(notebook_id)} by user: #{inspect(user_id)}")

    result =
      :notebookdb.delete_notebook(
        ensure_charlist(notebook_id),
        ensure_charlist(user_id)
      )

    Logger.info("Delete result: #{inspect(result)}")
    result
  end

  def get_notebooks_by_creator(creator_id) do
    Logger.info("Getting notebooks by creator: #{inspect(creator_id)}")
    :notebookdb.get_notebooks_by_creator(ensure_charlist(creator_id))
  end

  def get_public_notebooks do
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
    Logger.info("Getting notebook content: #{inspect(notebook_id)}")
    :notebookdb.get_notebook_content(ensure_charlist(notebook_id))
  end

  def update_notebook_content(notebook_id, user_id, content) do
    Logger.info("Updating notebook content: #{inspect(notebook_id)}")

    :notebookdb.update_notebook_content(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id),
      content
    )
  end

  def get_notebook_outputs(notebook_id) do
    :notebookdb.get_notebook_outputs(ensure_charlist(notebook_id))
  end

  def save_notebook_outputs(notebook_id, outputs) do
    :notebookdb.save_notebook_outputs(ensure_charlist(notebook_id), outputs)
  end

  def add_dataset_to_notebook(notebook_id, dataset_cid) do
    :notebookdb.add_dataset_to_notebook(ensure_charlist(notebook_id), dataset_cid)
  end

  def remove_dataset_from_notebook(notebook_id, dataset_cid) do
    :notebookdb.remove_dataset_from_notebook(ensure_charlist(notebook_id), dataset_cid)
  end

  def get_notebook_datasets(notebook_id) do
    :notebookdb.get_notebook_datasets(ensure_charlist(notebook_id))
  end

  def link_notebook_to_competition(notebook_id, competition_id) do
    :notebookdb.link_notebook_to_competition(
      ensure_charlist(notebook_id),
      ensure_charlist(competition_id)
    )
  end

  def unlink_notebook_from_competition(notebook_id, competition_id) do
    :notebookdb.unlink_notebook_from_competition(
      ensure_charlist(notebook_id),
      ensure_charlist(competition_id)
    )
  end

  def get_competition_notebooks(competition_id) do
    :notebookdb.get_competition_notebooks(ensure_charlist(competition_id))
  end

  def add_collaborator(notebook_id, user_id, permission_level) do
    :notebookdb.add_collaborator(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id),
      permission_level
    )
  end

  def remove_collaborator(notebook_id, user_id, requester_id) do
    :notebookdb.remove_collaborator(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id),
      ensure_charlist(requester_id)
    )
  end

  def get_notebook_collaborators(notebook_id) do
    :notebookdb.get_notebook_collaborators(ensure_charlist(notebook_id))
  end

  def is_collaborator(notebook_id, user_id) do
    :notebookdb.is_collaborator(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id)
    )
  end

  def create_version(notebook_id, user_id, change_description) do
    :notebookdb.create_version(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id),
      change_description
    )
  end

  def get_notebook_versions(notebook_id) do
    :notebookdb.get_notebook_versions(ensure_charlist(notebook_id))
  end

  def get_version_by_number(notebook_id, version_num) do
    :notebookdb.get_version_by_number(
      ensure_charlist(notebook_id),
      version_num
    )
  end

  def rollback_to_version(notebook_id, version_num, user_id) do
    :notebookdb.rollback_to_version(
      ensure_charlist(notebook_id),
      version_num,
      ensure_charlist(user_id)
    )
  end

  def compare_versions(notebook_id, version1, version2) do
    :notebookdb.compare_versions(
      ensure_charlist(notebook_id),
      version1,
      version2
    )
  end

  def fork_notebook(notebook_id, user_id) do
    result =
      :notebookdb.fork_notebook(
        ensure_charlist(notebook_id),
        ensure_charlist(user_id)
      )

    case result do
      {:ok, new_id} when is_list(new_id) -> {:ok, to_string(new_id)}
      other -> other
    end
  end

  def get_notebook_forks(notebook_id) do
    :notebookdb.get_notebook_forks(ensure_charlist(notebook_id))
  end

  def get_fork_parent(notebook_id) do
    :notebookdb.get_fork_parent(ensure_charlist(notebook_id))
  end

  def increment_fork_count(notebook_id) do
    :notebookdb.increment_fork_count(ensure_charlist(notebook_id))
  end

  def execute_notebook(notebook_id, user_id) do
    :notebookdb.execute_notebook(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id)
    )
  end

  def get_execution_logs(notebook_id) do
    :notebookdb.get_execution_logs(ensure_charlist(notebook_id))
  end

  def get_execution_stats(notebook_id) do
    :notebookdb.get_execution_stats(ensure_charlist(notebook_id))
  end

  def schedule_notebook_run(notebook_id, user_id, schedule) do
    :notebookdb.schedule_notebook_run(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id),
      schedule
    )
  end

  def cancel_scheduled_run(notebook_id, schedule_id) do
    :notebookdb.cancel_scheduled_run(
      ensure_charlist(notebook_id),
      ensure_charlist(schedule_id)
    )
  end

  def get_scheduled_runs(notebook_id) do
    :notebookdb.get_scheduled_runs(ensure_charlist(notebook_id))
  end

  def like_notebook(notebook_id, user_id) do
    Logger.info("Liking notebook: #{inspect(notebook_id)} by user: #{inspect(user_id)}")

    :notebookdb.like_notebook(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id)
    )
  end

  def unlike_notebook(notebook_id, user_id) do
    Logger.info("Unliking notebook: #{inspect(notebook_id)} by user: #{inspect(user_id)}")

    :notebookdb.unlike_notebook(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id)
    )
  end

  def get_notebook_likes(notebook_id) do
    :notebookdb.get_notebook_likes(ensure_charlist(notebook_id))
  end

  def has_user_liked(notebook_id, user_id) do
    :notebookdb.has_user_liked(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id)
    )
  end

  def add_comment(notebook_id, user_id, comment_text) do
    :notebookdb.add_comment(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id),
      comment_text
    )
  end

  def get_notebook_comments(notebook_id) do
    :notebookdb.get_notebook_comments(ensure_charlist(notebook_id))
  end

  def delete_comment(notebook_id, comment_id, user_id) do
    :notebookdb.delete_comment(
      ensure_charlist(notebook_id),
      ensure_charlist(comment_id),
      ensure_charlist(user_id)
    )
  end

  def update_comment(notebook_id, comment_id, new_text) do
    :notebookdb.update_comment(
      ensure_charlist(notebook_id),
      ensure_charlist(comment_id),
      new_text
    )
  end

  def set_notebook_environment(notebook_id, environment) do
    :notebookdb.set_notebook_environment(ensure_charlist(notebook_id), environment)
  end

  def get_notebook_environment(notebook_id) do
    :notebookdb.get_notebook_environment(ensure_charlist(notebook_id))
  end

  def set_dependencies(notebook_id, dependencies) do
    :notebookdb.set_dependencies(ensure_charlist(notebook_id), dependencies)
  end

  def get_dependencies(notebook_id) do
    :notebookdb.get_dependencies(ensure_charlist(notebook_id))
  end

  def add_citation(notebook_id, citation) do
    :notebookdb.add_citation(ensure_charlist(notebook_id), citation)
  end

  def get_notebook_citations(notebook_id) do
    :notebookdb.get_notebook_citations(ensure_charlist(notebook_id))
  end

  def remove_citation(notebook_id, citation_id) do
    :notebookdb.remove_citation(
      ensure_charlist(notebook_id),
      ensure_charlist(citation_id)
    )
  end

  def add_interactive_widget(notebook_id, widget_id, widget_config) do
    :notebookdb.add_interactive_widget(
      ensure_charlist(notebook_id),
      ensure_charlist(widget_id),
      widget_config
    )
  end

  def remove_interactive_widget(notebook_id, widget_id) do
    :notebookdb.remove_interactive_widget(
      ensure_charlist(notebook_id),
      ensure_charlist(widget_id)
    )
  end

  def get_notebook_widgets(notebook_id) do
    :notebookdb.get_notebook_widgets(ensure_charlist(notebook_id))
  end

  def set_notebook_type(notebook_id, notebook_type) do
    :notebookdb.set_notebook_type(ensure_charlist(notebook_id), notebook_type)
  end

  def get_notebooks_by_type(notebook_type) do
    :notebookdb.get_notebooks_by_type(notebook_type)
  end

  def search_notebooks(query) do
    :notebookdb.search_notebooks(query)
  end

  def search_notebooks_advanced(filters) do
    :notebookdb.search_notebooks_advanced(filters)
  end

  def get_trending_notebooks(limit) do
    :notebookdb.get_trending_notebooks(limit)
  end

  def get_featured_notebooks do
    :notebookdb.get_featured_notebooks()
  end

  def get_most_forked_notebooks(limit) do
    :notebookdb.get_most_forked_notebooks(limit)
  end

  def get_most_liked_notebooks(limit) do
    :notebookdb.get_most_liked_notebooks(limit)
  end

  def report_notebook(notebook_id, user_id, reason, details) do
    :notebookdb.report_notebook(
      ensure_charlist(notebook_id),
      ensure_charlist(user_id),
      reason,
      details
    )
  end

  def get_notebook_stats(notebook_id) do
    :notebookdb.get_notebook_stats(ensure_charlist(notebook_id))
  end

  def get_user_notebook_stats(user_id) do
    :notebookdb.get_user_notebook_stats(ensure_charlist(user_id))
  end

  def pin_notebook(notebook_id) do
    :notebookdb.pin_notebook(ensure_charlist(notebook_id))
  end

  def unpin_notebook(notebook_id) do
    :notebookdb.unpin_notebook(ensure_charlist(notebook_id))
  end

  def submit_notebook_to_competition(notebook_id, competition_id, user_id) do
    :notebookdb.submit_notebook_to_competition(
      ensure_charlist(notebook_id),
      ensure_charlist(competition_id),
      ensure_charlist(user_id)
    )
  end

  def get_submission_from_notebook(notebook_id) do
    :notebookdb.get_submission_from_notebook(ensure_charlist(notebook_id))
  end
end
