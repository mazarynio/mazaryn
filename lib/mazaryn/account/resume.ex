defmodule Account.Resumes do
  alias Core.ResumeClient
  require Logger

  def create_resume(user_id, resume_data) do
    ResumeClient.create_resume(user_id, resume_data)
  end

  def get_resume_by_id(resume_id) do
    case ResumeClient.get_resume_by_id(resume_id) do
      resume when is_tuple(resume) ->
        {:ok, resume_record_to_map(resume)}

      {:error, reason} ->
        {:error, reason}

      error ->
        Logger.error("Unexpected response from get_resume_by_id: #{inspect(error)}")
        {:error, :unexpected_response}
    end
  end

  def get_resume_by_user_id(user_id) do
    case ResumeClient.get_resume_by_user_id(user_id) do
      resume when is_tuple(resume) ->
        {:ok, resume_record_to_map(resume)}

      {:error, reason} ->
        {:error, reason}

      error ->
        Logger.error("Unexpected response from get_resume_by_user_id: #{inspect(error)}")
        {:error, :unexpected_response}
    end
  end

  def get_resumes_by_user_id(user_id) do
    resumes = ResumeClient.get_resumes_by_user_id(user_id)

    if is_list(resumes) do
      Enum.map(resumes, &resume_record_to_map/1)
    else
      []
    end
  end

  def update_resume(resume_id, update_data) do
    ResumeClient.update_resume(resume_id, update_data)
  end

  def delete_resume(resume_id) do
    ResumeClient.delete_resume(resume_id)
  end

  def set_primary_resume(user_id, resume_id) do
    ResumeClient.set_primary_resume(user_id, resume_id)
  end

  def add_work_experience(resume_id, experience) do
    ResumeClient.add_work_experience(resume_id, experience)
  end

  def remove_work_experience(resume_id, experience_id) do
    ResumeClient.remove_work_experience(resume_id, experience_id)
  end

  def update_work_experience(resume_id, experience_id, updated_experience) do
    ResumeClient.update_work_experience(resume_id, experience_id, updated_experience)
  end

  def add_education(resume_id, education) do
    ResumeClient.add_education(resume_id, education)
  end

  def remove_education(resume_id, education_id) do
    ResumeClient.remove_education(resume_id, education_id)
  end

  def add_skill(resume_id, skill) do
    ResumeClient.add_skill(resume_id, skill)
  end

  def remove_skill(resume_id, skill) do
    ResumeClient.remove_skill(resume_id, skill)
  end

  def add_certification(resume_id, certification) do
    ResumeClient.add_certification(resume_id, certification)
  end

  def add_project(resume_id, project) do
    ResumeClient.add_project(resume_id, project)
  end

  def remove_project(resume_id, project_id) do
    ResumeClient.remove_project(resume_id, project_id)
  end

  def resume_record_to_map(resume_tuple) when is_tuple(resume_tuple) do
    fields = Tuple.to_list(resume_tuple)
    [:resume | values] = fields

    %{
      id: to_string_safe(Enum.at(values, 0)),
      user_id: to_string_safe(Enum.at(values, 1)),
      resume_type: to_string_safe(Enum.at(values, 2)),
      is_primary: Enum.at(values, 3) || false,
      visibility: to_string_safe(Enum.at(values, 4)),
      status: to_string_safe(Enum.at(values, 5)),
      full_name: to_string_safe(Enum.at(values, 6)),
      professional_title: to_string_safe(Enum.at(values, 7)),
      headline: to_string_safe(Enum.at(values, 8)),
      summary: to_string_safe(Enum.at(values, 9)),
      email: to_string_safe(Enum.at(values, 10)),
      phone: to_string_safe(Enum.at(values, 11)),
      location: convert_location(Enum.at(values, 12)),
      website: to_string_safe(Enum.at(values, 13)),
      linkedin_url: to_string_safe(Enum.at(values, 14)),
      github_url: to_string_safe(Enum.at(values, 15)),
      portfolio_url: to_string_safe(Enum.at(values, 16)),
      work_experience: convert_work_experiences(Enum.at(values, 17)),
      education: convert_educations(Enum.at(values, 18)),
      technical_skills: safe_list_to_strings(Enum.at(values, 19)),
      soft_skills: safe_list_to_strings(Enum.at(values, 20)),
      languages: convert_languages(Enum.at(values, 21)),
      certifications: convert_certifications(Enum.at(values, 22)),
      projects: convert_projects(Enum.at(values, 23)),
      total_years_experience: safe_float(Enum.at(values, 24)),
      job_preferences: convert_job_preferences(Enum.at(values, 25)),
      date_created: Enum.at(values, 26),
      date_updated: Enum.at(values, 27),
      version: Enum.at(values, 28) || 1,
      views: Enum.at(values, 29) || 0,
      downloads: Enum.at(values, 30) || 0
    }
  end

  defp convert_location(nil), do: %{city: "", country: "", state: "", postal_code: ""}
  defp convert_location(:undefined), do: %{city: "", country: "", state: "", postal_code: ""}

  defp convert_location(location) when is_map(location) do
    %{
      city: to_string_safe(Map.get(location, :city, "")),
      country: to_string_safe(Map.get(location, :country, "")),
      state: to_string_safe(Map.get(location, :state, "")),
      postal_code: to_string_safe(Map.get(location, :postal_code, ""))
    }
  end

  defp convert_location(_), do: %{city: "", country: "", state: "", postal_code: ""}

  defp convert_work_experiences(nil), do: []
  defp convert_work_experiences(:undefined), do: []
  defp convert_work_experiences([]), do: []

  defp convert_work_experiences(experiences) when is_list(experiences) do
    Enum.map(experiences, fn exp ->
      cond do
        is_map(exp) ->
          %{
            id: to_string_safe(Map.get(exp, :id, "")),
            company: to_string_safe(Map.get(exp, :company_name, Map.get(exp, :company, ""))),
            job_title:
              to_string_safe(Map.get(exp, :position_title, Map.get(exp, :job_title, ""))),
            location: format_work_location(Map.get(exp, :location, "")),
            start_month: extract_month(Map.get(exp, :start_date)),
            start_year: extract_year(Map.get(exp, :start_date)),
            end_month: extract_month(Map.get(exp, :end_date)),
            end_year: extract_year(Map.get(exp, :end_date)),
            currently_working:
              Map.get(exp, :current_position, Map.get(exp, :currently_working, false)),
            description: to_string_safe(Map.get(exp, :description, "")),
            responsibilities: safe_list_to_strings(Map.get(exp, :responsibilities, [])),
            achievements: safe_list_to_strings(Map.get(exp, :achievements, [])),
            technologies_used: safe_list_to_strings(Map.get(exp, :technologies_used, [])),
            duration_months: safe_integer(Map.get(exp, :duration_months, 0))
          }

        is_tuple(exp) and tuple_size(exp) > 0 ->
          [record_name | exp_values] = Tuple.to_list(exp)

          if record_name == :work_experience do
            %{
              id: to_string_safe(Enum.at(exp_values, 0)),
              company: to_string_safe(Enum.at(exp_values, 1)),
              job_title: to_string_safe(Enum.at(exp_values, 3)),
              location: format_work_location(Enum.at(exp_values, 5)),
              start_month: extract_month(Enum.at(exp_values, 6)),
              start_year: extract_year(Enum.at(exp_values, 6)),
              end_month: extract_month(Enum.at(exp_values, 7)),
              end_year: extract_year(Enum.at(exp_values, 7)),
              currently_working: Enum.at(exp_values, 8) || false,
              description: to_string_safe(Enum.at(exp_values, 10)),
              responsibilities: safe_list_to_strings(Enum.at(exp_values, 11)),
              achievements: safe_list_to_strings(Enum.at(exp_values, 12)),
              technologies_used: safe_list_to_strings(Enum.at(exp_values, 13)),
              duration_months: safe_integer(Enum.at(exp_values, 9))
            }
          else
            default_work_experience()
          end

        true ->
          default_work_experience()
      end
    end)
  end

  defp convert_work_experiences(_), do: []

  defp convert_educations(nil), do: []
  defp convert_educations(:undefined), do: []
  defp convert_educations([]), do: []

  defp convert_educations(educations) when is_list(educations) do
    Enum.map(educations, fn edu ->
      cond do
        is_map(edu) ->
          %{
            id: to_string_safe(Map.get(edu, :id, "")),
            institution:
              to_string_safe(Map.get(edu, :institution_name, Map.get(edu, :institution, ""))),
            degree: to_string_safe(Map.get(edu, :degree, "")),
            field_of_study: to_string_safe(Map.get(edu, :field_of_study, "")),
            start_month: extract_month(Map.get(edu, :start_date)),
            start_year: extract_year(Map.get(edu, :start_date)),
            end_month: extract_month(Map.get(edu, :end_date)),
            end_year: extract_year(Map.get(edu, :end_date)),
            gpa: to_string_safe(Map.get(edu, :grade, Map.get(edu, :gpa, ""))),
            description: to_string_safe(Map.get(edu, :description, "")),
            activities: safe_list_to_strings(Map.get(edu, :activities, [])),
            honors: safe_list_to_strings(Map.get(edu, :honors, []))
          }

        is_tuple(edu) and tuple_size(edu) > 0 ->
          [record_name | edu_values] = Tuple.to_list(edu)

          if record_name == :education do
            %{
              id: to_string_safe(Enum.at(edu_values, 0)),
              institution: to_string_safe(Enum.at(edu_values, 1)),
              degree: to_string_safe(Enum.at(edu_values, 2)),
              field_of_study: to_string_safe(Enum.at(edu_values, 3)),
              start_month: extract_month(Enum.at(edu_values, 4)),
              start_year: extract_year(Enum.at(edu_values, 4)),
              end_month: extract_month(Enum.at(edu_values, 5)),
              end_year: extract_year(Enum.at(edu_values, 5)),
              gpa: to_string_safe(Enum.at(edu_values, 6)),
              description: to_string_safe(Enum.at(edu_values, 8)),
              activities: safe_list_to_strings(Enum.at(edu_values, 9)),
              honors: safe_list_to_strings(Enum.at(edu_values, 10))
            }
          else
            default_education()
          end

        true ->
          default_education()
      end
    end)
  end

  defp convert_educations(_), do: []

  defp convert_certifications(nil), do: []
  defp convert_certifications(:undefined), do: []
  defp convert_certifications([]), do: []

  defp convert_certifications(certs) when is_list(certs) do
    Enum.map(certs, fn cert ->
      if is_map(cert) do
        %{
          id: to_string_safe(Map.get(cert, :id, "")),
          name: to_string_safe(Map.get(cert, :name, "")),
          issuing_organization: to_string_safe(Map.get(cert, :issuing_organization, "")),
          issue_date: Map.get(cert, :issue_date),
          expiry_date: Map.get(cert, :expiry_date),
          credential_id: to_string_safe(Map.get(cert, :credential_id, "")),
          credential_url: to_string_safe(Map.get(cert, :credential_url, ""))
        }
      else
        %{
          id: "",
          name: "",
          issuing_organization: "",
          issue_date: nil,
          expiry_date: nil,
          credential_id: "",
          credential_url: ""
        }
      end
    end)
  end

  defp convert_certifications(_), do: []

  defp convert_projects(nil), do: []
  defp convert_projects(:undefined), do: []
  defp convert_projects([]), do: []

  defp convert_projects(projects) when is_list(projects) do
    Enum.map(projects, fn proj ->
      cond do
        is_map(proj) ->
          %{
            id: to_string_safe(Map.get(proj, :id, "")),
            name: to_string_safe(Map.get(proj, :title, Map.get(proj, :name, ""))),
            description: to_string_safe(Map.get(proj, :description, "")),
            url: to_string_safe(Map.get(proj, :url, "")),
            start_date: Map.get(proj, :start_date),
            end_date: Map.get(proj, :end_date),
            technologies: safe_list_to_strings(Map.get(proj, :technologies, []))
          }

        is_tuple(proj) and tuple_size(proj) > 0 ->
          [record_name | proj_values] = Tuple.to_list(proj)

          if record_name == :job_project do
            %{
              id: to_string_safe(Enum.at(proj_values, 0)),
              name: to_string_safe(Enum.at(proj_values, 1)),
              description: to_string_safe(Enum.at(proj_values, 2)),
              url: to_string_safe(Enum.at(proj_values, 5)),
              start_date: Enum.at(proj_values, 3),
              end_date: Enum.at(proj_values, 4),
              technologies: safe_list_to_strings(Enum.at(proj_values, 7))
            }
          else
            default_project()
          end

        true ->
          default_project()
      end
    end)
  end

  defp convert_projects(_), do: []

  defp convert_languages(nil), do: []
  defp convert_languages(:undefined), do: []
  defp convert_languages([]), do: []

  defp convert_languages(langs) when is_list(langs) do
    Enum.map(langs, fn lang ->
      if is_map(lang) or is_tuple(lang) do
        language_name = if is_map(lang), do: Map.get(lang, :language, ""), else: elem(lang, 0)

        proficiency =
          if is_map(lang),
            do: Map.get(lang, :proficiency, ""),
            else: if(tuple_size(lang) > 1, do: elem(lang, 1), else: "")

        %{
          language: to_string_safe(language_name),
          proficiency: to_string_safe(proficiency)
        }
      else
        %{language: "", proficiency: ""}
      end
    end)
  end

  defp convert_languages(_), do: []

  defp convert_job_preferences(nil), do: %{}
  defp convert_job_preferences(:undefined), do: %{}

  defp convert_job_preferences(prefs) when is_map(prefs) do
    %{
      desired_titles: safe_list_to_strings(Map.get(prefs, :desired_titles, [])),
      desired_industries: safe_list_to_strings(Map.get(prefs, :desired_industries, [])),
      desired_locations: safe_list_to_strings(Map.get(prefs, :desired_locations, [])),
      remote_preference: to_string_safe(Map.get(prefs, :remote_preference, "")),
      employment_types: safe_list_to_strings(Map.get(prefs, :employment_types, [])),
      min_salary: safe_float(Map.get(prefs, :min_salary, 0.0)),
      salary_currency: to_string_safe(Map.get(prefs, :salary_currency, "USD"))
    }
  end

  defp convert_job_preferences(_), do: %{}

  defp format_work_location(location) when is_map(location) do
    city = to_string_safe(Map.get(location, :city, ""))
    country = to_string_safe(Map.get(location, :country, ""))
    state = to_string_safe(Map.get(location, :state, ""))

    [city, state, country]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(", ")
  end

  defp format_work_location(location) when is_binary(location), do: location
  defp format_work_location(_), do: ""

  defp extract_month(nil), do: ""
  defp extract_month(:undefined), do: ""

  defp extract_month({year, month}) when is_integer(year) and is_integer(month) do
    months = [
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    ]

    Enum.at(months, month - 1) || ""
  end

  defp extract_month(_), do: ""

  defp extract_year(nil), do: ""
  defp extract_year(:undefined), do: ""
  defp extract_year({year, _month}) when is_integer(year), do: Integer.to_string(year)
  defp extract_year(_), do: ""

  defp default_work_experience do
    %{
      id: "",
      company: "",
      job_title: "",
      location: "",
      start_month: "",
      start_year: "",
      end_month: "",
      end_year: "",
      currently_working: false,
      description: "",
      responsibilities: [],
      achievements: [],
      technologies_used: [],
      duration_months: 0
    }
  end

  defp default_education do
    %{
      id: "",
      institution: "",
      degree: "",
      field_of_study: "",
      start_month: "",
      start_year: "",
      end_month: "",
      end_year: "",
      gpa: "",
      description: "",
      activities: [],
      honors: []
    }
  end

  defp default_project do
    %{
      id: "",
      name: "",
      description: "",
      url: "",
      start_date: nil,
      end_date: nil,
      technologies: []
    }
  end

  defp safe_integer(value) when is_integer(value), do: value
  defp safe_integer(_), do: 0

  defp safe_float(value) when is_float(value), do: value
  defp safe_float(value) when is_integer(value), do: value / 1.0
  defp safe_float(_), do: 0.0

  defp safe_list(nil), do: []
  defp safe_list(:undefined), do: []
  defp safe_list([]), do: []
  defp safe_list(value) when is_list(value), do: value
  defp safe_list(_), do: []

  defp safe_value(nil), do: nil
  defp safe_value(:undefined), do: nil
  defp safe_value([]), do: []

  defp safe_value(value) when is_list(value) do
    if Enum.all?(value, &is_integer/1) do
      List.to_string(value)
    else
      value
    end
  rescue
    _ -> value
  end

  defp safe_value(value), do: value

  defp safe_list_to_strings(list) when is_list(list) do
    Enum.map(list, &to_string_safe/1)
  end

  defp safe_list_to_strings(_), do: []

  defp to_string_safe(value) when is_binary(value), do: value

  defp to_string_safe(value) when is_list(value) do
    try do
      List.to_string(value)
    rescue
      _ -> ""
    end
  end

  defp to_string_safe(value) when is_atom(value) and value != nil and value != :undefined do
    Atom.to_string(value)
  end

  defp to_string_safe(_), do: ""
end
