defmodule Core.JobClient do
  require Logger

  def create_job_posting(poster_data, job_data) do
    Logger.debug("JobClient.create_job_posting")

    try do
      poster_map = %{
        poster_type: to_charlist_safe(poster_data[:poster_type] || "business"),
        poster_id: to_charlist_safe(poster_data[:poster_id]),
        posted_by_user_id: to_charlist_safe(poster_data[:posted_by_user_id]),
        business_id: to_charlist_safe(poster_data[:business_id] || "")
      }

      job_map = convert_job_data(job_data)

      result = :jobdb.create_job_posting_concurrent(poster_map, job_map)

      case result do
        {:ok, job_id} when is_list(job_id) ->
          {:ok, List.to_string(job_id)}

        {:ok, job_id} when is_binary(job_id) ->
          {:ok, job_id}

        {:error, reason} ->
          {:error, reason}

        other ->
          Logger.error("Unexpected result: #{inspect(other)}")
          {:error, :unexpected_result}
      end
    rescue
      error ->
        Logger.error("Exception in create_job_posting: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_job_posting_by_id(job_id) do
    Logger.debug("JobClient.get_job_posting_by_id - id: #{inspect(job_id)}")

    try do
      result = :jobdb.get_job_posting_by_id(to_charlist_safe(job_id))

      case result do
        job when is_tuple(job) and tuple_size(job) > 0 ->
          job

        {:error, reason} ->
          {:error, reason}

        other ->
          Logger.error("Unexpected result from get_job_posting_by_id: #{inspect(other)}")
          {:error, :not_found}
      end
    rescue
      error ->
        Logger.error("Exception in get_job_posting_by_id: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_job_postings_by_poster(poster_type, poster_id) do
    Logger.debug("JobClient.get_job_postings_by_poster")

    try do
      result =
        :jobdb.get_job_postings_by_poster(
          to_charlist_safe(poster_type),
          to_charlist_safe(poster_id)
        )

      case result do
        jobs when is_list(jobs) -> jobs
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in get_job_postings_by_poster: #{inspect(error)}")
        []
    end
  end

  def get_active_job_postings do
    Logger.debug("JobClient.get_active_job_postings")

    try do
      result = :jobdb.get_active_job_postings()

      case result do
        jobs when is_list(jobs) -> jobs
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in get_active_job_postings: #{inspect(error)}")
        []
    end
  end

  def update_job_posting(job_id, update_data) do
    Logger.debug("JobClient.update_job_posting - id: #{inspect(job_id)}")

    try do
      erlang_map = convert_update_map(update_data)
      :jobdb.update_job_posting(to_charlist_safe(job_id), erlang_map)
    rescue
      error ->
        Logger.error("Exception in update_job_posting: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def delete_job_posting(job_id) do
    Logger.debug("JobClient.delete_job_posting - id: #{inspect(job_id)}")

    try do
      :jobdb.delete_job_posting(to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in delete_job_posting: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def publish_job(job_id) do
    try do
      :jobdb.publish_job(to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in publish_job: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def unpublish_job(job_id) do
    try do
      :jobdb.unpublish_job(to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in unpublish_job: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def close_job(job_id) do
    try do
      :jobdb.close_job(to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in close_job: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def increment_view_count(job_id) do
    try do
      :jobdb.increment_view_count(to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in increment_view_count: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def search_jobs_by_title(title) do
    try do
      result = :jobdb.search_jobs_by_title(to_charlist_safe(title))

      case result do
        jobs when is_list(jobs) -> jobs
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in search_jobs_by_title: #{inspect(error)}")
        []
    end
  end

  def search_jobs_by_location(city, country) do
    try do
      result =
        :jobdb.search_jobs_by_location(
          to_charlist_safe(city),
          to_charlist_safe(country)
        )

      case result do
        jobs when is_list(jobs) -> jobs
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in search_jobs_by_location: #{inspect(error)}")
        []
    end
  end

  def filter_jobs(filter_criteria) do
    try do
      erlang_criteria =
        Enum.map(filter_criteria, fn {key, value} ->
          {key, convert_filter_value(value)}
        end)

      result = :jobdb.filter_jobs(erlang_criteria)

      case result do
        jobs when is_list(jobs) -> jobs
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in filter_jobs: #{inspect(error)}")
        []
    end
  end

  def save_job(user_id, job_id) do
    try do
      :jobdb.save_job(to_charlist_safe(user_id), to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in save_job: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def unsave_job(user_id, job_id) do
    try do
      :jobdb.unsave_job(to_charlist_safe(user_id), to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in unsave_job: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_saved_jobs(user_id) do
    try do
      result = :jobdb.get_saved_jobs(to_charlist_safe(user_id))

      case result do
        jobs when is_list(jobs) -> jobs
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in get_saved_jobs: #{inspect(error)}")
        []
    end
  end

  def is_job_saved(user_id, job_id) do
    try do
      :jobdb.is_job_saved(to_charlist_safe(user_id), to_charlist_safe(job_id))
    rescue
      error ->
        Logger.error("Exception in is_job_saved: #{inspect(error)}")
        false
    end
  end

  defp convert_job_data(job_data) do
    %{
      title: to_charlist_safe(job_data[:title] || ""),
      description: to_charlist_safe(job_data[:description] || ""),
      short_description: to_charlist_safe(job_data[:short_description] || ""),
      category: to_charlist_safe(job_data[:category] || ""),
      sub_category: to_charlist_safe(job_data[:sub_category] || ""),
      industry: to_charlist_safe(job_data[:industry] || ""),
      job_function: to_charlist_safe(job_data[:job_function] || ""),
      seniority_level: to_charlist_safe(job_data[:seniority_level] || "mid"),
      employment_type: to_charlist_safe(job_data[:employment_type] || "full_time"),
      location_type: to_charlist_safe(job_data[:location_type] || "on_site"),
      locations: convert_locations(job_data[:locations] || []),
      salary_range: convert_salary_range(job_data[:salary_range] || %{}),
      salary_visible: job_data[:salary_visible] || false,
      required_skills: convert_list(job_data[:required_skills] || []),
      preferred_skills: convert_list(job_data[:preferred_skills] || []),
      experience_required: convert_experience(job_data[:experience_required] || %{}),
      responsibilities: convert_list(job_data[:responsibilities] || []),
      requirements: convert_list(job_data[:requirements] || []),
      benefits: convert_list(job_data[:benefits] || []),
      application_deadline: job_data[:application_deadline],
      visibility: to_charlist_safe(job_data[:visibility] || "public")
    }
  end

  defp convert_locations(locations) when is_list(locations) do
    Enum.map(locations, fn location ->
      %{
        country: to_charlist_safe(Map.get(location, :country, "")),
        state: to_charlist_safe(Map.get(location, :state, "")),
        city: to_charlist_safe(Map.get(location, :city, "")),
        address: to_charlist_safe(Map.get(location, :address, "")),
        remote: Map.get(location, :remote, false)
      }
    end)
  end

  defp convert_salary_range(salary) when is_map(salary) do
    %{
      min: Map.get(salary, :min, 0.0),
      max: Map.get(salary, :max, 0.0),
      currency: to_charlist_safe(Map.get(salary, :currency, "USD")),
      period: to_charlist_safe(Map.get(salary, :period, "yearly"))
    }
  end

  defp convert_experience(experience) when is_map(experience) do
    %{
      min_years: Map.get(experience, :min_years, 0),
      max_years: Map.get(experience, :max_years, 0)
    }
  end

  defp convert_list(list) when is_list(list) do
    Enum.map(list, &to_charlist_safe/1)
  end

  defp convert_update_map(data) when is_map(data) do
    data
    |> Enum.map(fn {key, value} ->
      {key, convert_value(value)}
    end)
    |> Map.new()
  end

  defp convert_value(value) when is_binary(value), do: to_charlist_safe(value)

  defp convert_value(value) when is_list(value) and length(value) > 0 do
    case hd(value) do
      v when is_binary(v) -> Enum.map(value, &to_charlist_safe/1)
      _ -> value
    end
  end

  defp convert_value(value) when is_list(value), do: value
  defp convert_value(value) when is_map(value), do: convert_update_map(value)
  defp convert_value(value), do: value

  defp convert_filter_value(value) when is_binary(value), do: to_charlist_safe(value)
  defp convert_filter_value(value), do: value

  defp to_charlist_safe(value) when is_binary(value), do: String.to_charlist(value)
  defp to_charlist_safe(value) when is_list(value), do: value
  defp to_charlist_safe(value) when is_atom(value), do: Atom.to_charlist(value)
  defp to_charlist_safe(value) when is_integer(value), do: Integer.to_charlist(value)
  defp to_charlist_safe(value), do: to_charlist_safe(inspect(value))

  def get_complete_resume(resume_id) do
    Logger.debug("JobClient.get_complete_resume - id: #{inspect(resume_id)}")

    case :jobdb.get_complete_resume(to_charlist_safe(resume_id)) do
      {:ok, resume_map} when is_map(resume_map) ->
        {:ok, resume_map}

      {:error, :resume_not_found} ->
        {:error, :resume_not_found}

      {:error, reason} ->
        {:error, reason}

      {:aborted, reason} ->
        Logger.error("Transaction aborted in get_complete_resume: #{inspect(reason)}")
        {:error, {:transaction_failed, reason}}

      other ->
        Logger.error("Unexpected result from get_complete_resume: #{inspect(other)}")
        {:error, :unexpected_result}
    end
  end

  def get_resume_by_id(resume_id) do
    Logger.debug("JobClient.get_resume_by_id - id: #{inspect(resume_id)}")

    try do
      result = :jobdb.get_resume_by_id(to_charlist_safe(resume_id))

      case result do
        resume when is_tuple(resume) and tuple_size(resume) > 0 ->
          resume

        {:error, reason} ->
          {:error, reason}

        other ->
          Logger.error("Unexpected result from get_resume_by_id: #{inspect(other)}")
          {:error, :not_found}
      end
    rescue
      error ->
        Logger.error("Exception in get_resume_by_id: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_resume_by_user_id(user_id) do
    try do
      result = :jobdb.get_resume_by_user_id(to_charlist_safe(user_id))

      case result do
        resume when is_tuple(resume) and tuple_size(resume) > 0 ->
          resume

        {:error, reason} ->
          {:error, reason}

        other ->
          Logger.error("Unexpected result from get_resume_by_user_id: #{inspect(other)}")
          {:error, :not_found}
      end
    rescue
      error ->
        Logger.error("Exception in get_resume_by_user_id: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_resumes_by_user_id(user_id) do
    try do
      result = :jobdb.get_resumes_by_user_id(to_charlist_safe(user_id))

      case result do
        resumes when is_list(resumes) -> resumes
        {:error, reason} -> {:error, reason}
        _ -> []
      end
    rescue
      error ->
        Logger.error("Exception in get_resumes_by_user_id: #{inspect(error)}")
        []
    end
  end

  def increment_resume_views(resume_id) do
    try do
      :jobdb.increment_resume_views(to_charlist_safe(resume_id))
    rescue
      error ->
        Logger.error("Exception in increment_resume_views: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def set_primary_resume(user_id, resume_id) do
    Logger.debug("JobClient.set_primary_resume - user: #{user_id}, resume: #{resume_id}")

    try do
      :jobdb.set_primary_resume(to_charlist_safe(user_id), to_charlist_safe(resume_id))
    rescue
      error ->
        Logger.error("Exception in set_primary_resume: #{inspect(error)}")
        {:error, :exception}
    end
  end

  defp to_charlist_safe(value) when is_binary(value), do: String.to_charlist(value)
  defp to_charlist_safe(value) when is_list(value), do: value
  defp to_charlist_safe(value) when is_atom(value), do: Atom.to_charlist(value)
  defp to_charlist_safe(value) when is_integer(value), do: Integer.to_charlist(value)
  defp to_charlist_safe(value), do: to_charlist_safe(inspect(value))
end
