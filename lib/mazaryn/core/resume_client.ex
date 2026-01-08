defmodule Core.ResumeClient do
  require Logger

  def create_resume(user_id, resume_data) do
    Logger.debug("ResumeClient.create_resume")

    try do
      erlang_data = convert_resume_data(resume_data)
      result = :jobdb.create_resume(to_charlist_safe(user_id), erlang_data)

      case result do
        {:ok, resume_id} when is_list(resume_id) ->
          {:ok, List.to_string(resume_id)}

        {:ok, resume_id} when is_binary(resume_id) ->
          {:ok, resume_id}

        {:error, reason} ->
          {:error, reason}

        other ->
          Logger.error("Unexpected result: #{inspect(other)}")
          {:error, :unexpected_result}
      end
    rescue
      error ->
        Logger.error("Exception in create_resume: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def get_resume_by_id(resume_id) do
    Logger.debug("ResumeClient.get_resume_by_id - id: #{inspect(resume_id)}")

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

  def update_resume(resume_id, update_data) do
    try do
      erlang_map = convert_update_map(update_data)
      :jobdb.update_resume(to_charlist_safe(resume_id), erlang_map)
    rescue
      error ->
        Logger.error("Exception in update_resume: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def delete_resume(resume_id) do
    try do
      :jobdb.delete_resume(to_charlist_safe(resume_id))
    rescue
      error ->
        Logger.error("Exception in delete_resume: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def set_primary_resume(user_id, resume_id) do
    try do
      :jobdb.set_primary_resume(to_charlist_safe(user_id), to_charlist_safe(resume_id))
    rescue
      error ->
        Logger.error("Exception in set_primary_resume: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def add_work_experience(resume_id, experience) do
    try do
      erlang_exp = convert_work_experience(experience)
      :jobdb.add_work_experience(to_charlist_safe(resume_id), erlang_exp)
    rescue
      error ->
        Logger.error("Exception in add_work_experience: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def remove_work_experience(resume_id, experience_id) do
    try do
      :jobdb.remove_work_experience(to_charlist_safe(resume_id), to_charlist_safe(experience_id))
    rescue
      error ->
        Logger.error("Exception in remove_work_experience: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def update_work_experience(resume_id, experience_id, updated_experience) do
    try do
      erlang_exp = convert_work_experience(updated_experience)

      :jobdb.update_work_experience(
        to_charlist_safe(resume_id),
        to_charlist_safe(experience_id),
        erlang_exp
      )
    rescue
      error ->
        Logger.error("Exception in update_work_experience: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def add_education(resume_id, education) do
    try do
      erlang_edu = convert_education(education)
      :jobdb.add_education(to_charlist_safe(resume_id), erlang_edu)
    rescue
      error ->
        Logger.error("Exception in add_education: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def remove_education(resume_id, education_id) do
    try do
      :jobdb.remove_education(to_charlist_safe(resume_id), to_charlist_safe(education_id))
    rescue
      error ->
        Logger.error("Exception in remove_education: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def add_skill(resume_id, skill) do
    try do
      :jobdb.add_skill(to_charlist_safe(resume_id), to_charlist_safe(skill))
    rescue
      error ->
        Logger.error("Exception in add_skill: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def remove_skill(resume_id, skill) do
    try do
      :jobdb.remove_skill(to_charlist_safe(resume_id), to_charlist_safe(skill))
    rescue
      error ->
        Logger.error("Exception in remove_skill: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def add_certification(resume_id, certification) do
    try do
      erlang_cert = convert_certification(certification)
      :jobdb.add_certification_to_resume(to_charlist_safe(resume_id), erlang_cert)
    rescue
      error ->
        Logger.error("Exception in add_certification: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def add_project(resume_id, project) do
    try do
      erlang_proj = convert_project(project)
      :jobdb.add_project(to_charlist_safe(resume_id), erlang_proj)
    rescue
      error ->
        Logger.error("Exception in add_project: #{inspect(error)}")
        {:error, :exception}
    end
  end

  def remove_project(resume_id, project_id) do
    try do
      :jobdb.remove_project(to_charlist_safe(resume_id), to_charlist_safe(project_id))
    rescue
      error ->
        Logger.error("Exception in remove_project: #{inspect(error)}")
        {:error, :exception}
    end
  end

  defp convert_resume_data(data) do
    %{
      full_name: to_charlist_safe(Map.get(data, :full_name, "")),
      professional_title: to_charlist_safe(Map.get(data, :professional_title, "")),
      headline: to_charlist_safe(Map.get(data, :headline, "")),
      summary: to_charlist_safe(Map.get(data, :summary, "")),
      email: to_charlist_safe(Map.get(data, :email, "")),
      phone: to_charlist_safe(Map.get(data, :phone, "")),
      location: convert_location(Map.get(data, :location, %{})),
      website: to_charlist_safe(Map.get(data, :website, "")),
      linkedin_url: to_charlist_safe(Map.get(data, :linkedin_url, "")),
      github_url: to_charlist_safe(Map.get(data, :github_url, "")),
      portfolio_url: to_charlist_safe(Map.get(data, :portfolio_url, "")),
      technical_skills: convert_list(Map.get(data, :technical_skills, [])),
      soft_skills: convert_list(Map.get(data, :soft_skills, [])),
      visibility: to_charlist_safe(Map.get(data, :visibility, "private")),
      resume_type: to_charlist_safe(Map.get(data, :resume_type, "platform_generated"))
    }
  end

  defp convert_location(location) when is_map(location) do
    %{
      country: to_charlist_safe(Map.get(location, :country, "")),
      state: to_charlist_safe(Map.get(location, :state, "")),
      city: to_charlist_safe(Map.get(location, :city, "")),
      postal_code: to_charlist_safe(Map.get(location, :postal_code, "")),
      willing_to_relocate: Map.get(location, :willing_to_relocate, false)
    }
  end

  defp convert_work_experience(exp) when is_map(exp) do
    %{
      id: to_charlist_safe(Map.get(exp, :id, "")),
      company: to_charlist_safe(Map.get(exp, :company, "")),
      job_title: to_charlist_safe(Map.get(exp, :job_title, "")),
      start_date: Map.get(exp, :start_date),
      end_date: Map.get(exp, :end_date),
      currently_working: Map.get(exp, :currently_working, false),
      description: to_charlist_safe(Map.get(exp, :description, "")),
      location: to_charlist_safe(Map.get(exp, :location, "")),
      duration_months: Map.get(exp, :duration_months, 0)
    }
  end

  defp convert_education(edu) when is_map(edu) do
    %{
      id: to_charlist_safe(Map.get(edu, :id, "")),
      institution: to_charlist_safe(Map.get(edu, :institution, "")),
      degree: to_charlist_safe(Map.get(edu, :degree, "")),
      field_of_study: to_charlist_safe(Map.get(edu, :field_of_study, "")),
      start_date: Map.get(edu, :start_date),
      end_date: Map.get(edu, :end_date),
      gpa: to_charlist_safe(Map.get(edu, :gpa, "")),
      description: to_charlist_safe(Map.get(edu, :description, ""))
    }
  end

  defp convert_certification(cert) when is_map(cert) do
    %{
      id: to_charlist_safe(Map.get(cert, :id, "")),
      name: to_charlist_safe(Map.get(cert, :name, "")),
      issuing_organization: to_charlist_safe(Map.get(cert, :issuing_organization, "")),
      issue_date: Map.get(cert, :issue_date),
      expiry_date: Map.get(cert, :expiry_date),
      credential_id: to_charlist_safe(Map.get(cert, :credential_id, "")),
      credential_url: to_charlist_safe(Map.get(cert, :credential_url, ""))
    }
  end

  defp convert_project(proj) when is_map(proj) do
    %{
      id: to_charlist_safe(Map.get(proj, :id, "")),
      name: to_charlist_safe(Map.get(proj, :name, "")),
      description: to_charlist_safe(Map.get(proj, :description, "")),
      url: to_charlist_safe(Map.get(proj, :url, "")),
      start_date: Map.get(proj, :start_date),
      end_date: Map.get(proj, :end_date),
      technologies: convert_list(Map.get(proj, :technologies, []))
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

  defp to_charlist_safe(value) when is_binary(value), do: String.to_charlist(value)
  defp to_charlist_safe(value) when is_list(value), do: value
  defp to_charlist_safe(value) when is_atom(value), do: Atom.to_charlist(value)
  defp to_charlist_safe(value) when is_integer(value), do: Integer.to_charlist(value)
  defp to_charlist_safe(value), do: to_charlist_safe(inspect(value))
end
