defmodule Account.Jobs do
  alias Core.JobClient
  require Logger

  def create_job_posting(poster_data, job_data) do
    JobClient.create_job_posting(poster_data, job_data)
  end

  def get_job_posting_by_id(job_id) do
    case JobClient.get_job_posting_by_id(job_id) do
      job when is_tuple(job) ->
        {:ok, job_record_to_map(job)}

      {:error, reason} ->
        {:error, reason}

      error ->
        Logger.error("Unexpected response from get_job_posting_by_id: #{inspect(error)}")
        {:error, :unexpected_response}
    end
  end

  def get_job_postings_by_poster(poster_type, poster_id) do
    jobs = JobClient.get_job_postings_by_poster(poster_type, poster_id)

    if is_list(jobs) do
      Enum.map(jobs, &job_record_to_map/1)
    else
      []
    end
  end

  def get_active_job_postings do
    jobs = JobClient.get_active_job_postings()

    if is_list(jobs) do
      Enum.map(jobs, &job_record_to_map/1)
    else
      []
    end
  end

  def update_job_posting(job_id, update_data) do
    JobClient.update_job_posting(job_id, update_data)
  end

  def delete_job_posting(job_id) do
    JobClient.delete_job_posting(job_id)
  end

  def publish_job(job_id) do
    JobClient.publish_job(job_id)
  end

  def unpublish_job(job_id) do
    JobClient.unpublish_job(job_id)
  end

  def close_job(job_id) do
    JobClient.close_job(job_id)
  end

  def increment_view_count(job_id) do
    JobClient.increment_view_count(job_id)
  end

  def search_jobs_by_title(title) do
    jobs = JobClient.search_jobs_by_title(title)

    if is_list(jobs) do
      Enum.map(jobs, &job_record_to_map/1)
    else
      []
    end
  end

  defp convert_complete_resume(resume) when is_map(resume) do
    %{
      id: to_string_safe(Map.get(resume, :id)),
      user_id: to_string_safe(Map.get(resume, :user_id)),
      resume_type: to_string_safe(Map.get(resume, :resume_type)),
      is_primary: Map.get(resume, :is_primary, false),
      visibility: to_string_safe(Map.get(resume, :visibility)),
      status: to_string_safe(Map.get(resume, :status)),
      full_name: to_string_safe(Map.get(resume, :full_name)),
      professional_title: to_string_safe(Map.get(resume, :professional_title)),
      headline: to_string_safe(Map.get(resume, :headline)),
      summary: to_string_safe(Map.get(resume, :summary)),
      email: to_string_safe(Map.get(resume, :email)),
      phone: to_string_safe(Map.get(resume, :phone)),
      location: convert_resume_location(Map.get(resume, :location)),
      website: to_string_safe(Map.get(resume, :website)),
      linkedin_url: to_string_safe(Map.get(resume, :linkedin_url)),
      github_url: to_string_safe(Map.get(resume, :github_url)),
      portfolio_url: to_string_safe(Map.get(resume, :portfolio_url)),
      work_experience: convert_complete_work_experiences(Map.get(resume, :work_experience, [])),
      total_years_experience: Map.get(resume, :total_years_experience, 0),
      education: convert_complete_educations(Map.get(resume, :education, [])),
      technical_skills: safe_list_to_strings(Map.get(resume, :technical_skills, [])),
      soft_skills: safe_list_to_strings(Map.get(resume, :soft_skills, [])),
      languages: convert_complete_languages(Map.get(resume, :languages, [])),
      certifications: convert_complete_certifications(Map.get(resume, :certifications, [])),
      projects: convert_complete_projects(Map.get(resume, :projects, [])),
      job_preferences: convert_complete_job_preferences(Map.get(resume, :job_preferences, %{})),
      date_created: Map.get(resume, :date_created),
      date_updated: Map.get(resume, :date_updated),
      version: Map.get(resume, :version, 1),
      views: Map.get(resume, :views, 0),
      downloads: Map.get(resume, :downloads, 0)
    }
  end

  defp convert_complete_work_experiences(experiences) when is_list(experiences) do
    Enum.map(experiences, fn exp ->
      if is_map(exp) do
        location = Map.get(exp, :location, %{})
        location_str = format_complete_location(location)

        %{
          id: to_string_safe(Map.get(exp, :id)),
          company_name: to_string_safe(Map.get(exp, :company_name)),
          position_title: to_string_safe(Map.get(exp, :position_title)),
          employment_type: to_string_safe(Map.get(exp, :employment_type)),
          location: location_str,
          start_month: extract_month_from_date(Map.get(exp, :start_date)),
          start_year: extract_year_from_date(Map.get(exp, :start_date)),
          end_month: extract_month_from_date(Map.get(exp, :end_date)),
          end_year: extract_year_from_date(Map.get(exp, :end_date)),
          currently_working: Map.get(exp, :current_position, false),
          duration_months: Map.get(exp, :duration_months, 0),
          description: to_string_safe(Map.get(exp, :description)),
          responsibilities: safe_list_to_strings(Map.get(exp, :responsibilities, [])),
          achievements: safe_list_to_strings(Map.get(exp, :achievements, [])),
          technologies_used: safe_list_to_strings(Map.get(exp, :technologies_used, []))
        }
      else
        default_work_experience()
      end
    end)
  end

  defp convert_complete_work_experiences(_), do: []

  defp convert_complete_educations(educations) when is_list(educations) do
    Enum.map(educations, fn edu ->
      if is_map(edu) do
        %{
          id: to_string_safe(Map.get(edu, :id)),
          institution_name: to_string_safe(Map.get(edu, :institution_name)),
          degree: to_string_safe(Map.get(edu, :degree)),
          field_of_study: to_string_safe(Map.get(edu, :field_of_study)),
          start_date: Map.get(edu, :start_date),
          end_date: Map.get(edu, :end_date),
          grade: to_string_safe(Map.get(edu, :grade)),
          grade_type: to_string_safe(Map.get(edu, :grade_type)),
          description: to_string_safe(Map.get(edu, :description)),
          activities: safe_list_to_strings(Map.get(edu, :activities, [])),
          honors: safe_list_to_strings(Map.get(edu, :honors, [])),
          relevant_coursework: safe_list_to_strings(Map.get(edu, :relevant_coursework, []))
        }
      else
        default_education()
      end
    end)
  end

  defp convert_complete_educations(_), do: []

  defp convert_complete_languages(languages) when is_list(languages) do
    Enum.map(languages, fn lang ->
      if is_map(lang) do
        %{
          language: to_string_safe(Map.get(lang, :language)),
          proficiency: to_string_safe(Map.get(lang, :proficiency))
        }
      else
        %{language: "", proficiency: ""}
      end
    end)
  end

  defp convert_complete_languages(_), do: []

  defp convert_complete_certifications(certs) when is_list(certs) do
    Enum.map(certs, fn cert ->
      if is_map(cert) do
        %{
          id: to_string_safe(Map.get(cert, :id)),
          name: to_string_safe(Map.get(cert, :name)),
          issuing_organization: to_string_safe(Map.get(cert, :issuing_organization)),
          issue_date: Map.get(cert, :issue_date),
          expiry_date: Map.get(cert, :expiry_date),
          credential_id: to_string_safe(Map.get(cert, :credential_id)),
          credential_url: to_string_safe(Map.get(cert, :credential_url))
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

  defp convert_complete_certifications(_), do: []

  defp convert_complete_projects(projects) when is_list(projects) do
    Enum.map(projects, fn proj ->
      if is_map(proj) do
        %{
          id: to_string_safe(Map.get(proj, :id)),
          title: to_string_safe(Map.get(proj, :title)),
          description: to_string_safe(Map.get(proj, :description)),
          role: to_string_safe(Map.get(proj, :role)),
          start_date: Map.get(proj, :start_date),
          end_date: Map.get(proj, :end_date),
          current_project: Map.get(proj, :current_project, false),
          technologies: safe_list_to_strings(Map.get(proj, :technologies, [])),
          url: to_string_safe(Map.get(proj, :url)),
          github_url: to_string_safe(Map.get(proj, :github_url)),
          achievements: safe_list_to_strings(Map.get(proj, :achievements, []))
        }
      else
        default_project()
      end
    end)
  end

  defp convert_complete_projects(_), do: []

  defp convert_complete_job_preferences(prefs) when is_map(prefs) do
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

  defp convert_complete_job_preferences(_), do: %{}

  defp convert_resume_location(location) when is_map(location) do
    %{
      country: to_string_safe(Map.get(location, :country, "")),
      state: to_string_safe(Map.get(location, :state, "")),
      city: to_string_safe(Map.get(location, :city, "")),
      postal_code: to_string_safe(Map.get(location, :postal_code, "")),
      willing_to_relocate: Map.get(location, :willing_to_relocate, false)
    }
  end

  defp convert_resume_location(_),
    do: %{country: "", state: "", city: "", postal_code: "", willing_to_relocate: false}

  defp format_complete_location(location) when is_map(location) do
    city = to_string_safe(Map.get(location, :city, ""))
    state = to_string_safe(Map.get(location, :state, ""))
    country = to_string_safe(Map.get(location, :country, ""))

    [city, state, country]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join(", ")
  end

  defp format_complete_location(_), do: ""

  defp extract_month_from_date(nil), do: ""
  defp extract_month_from_date(:undefined), do: ""
  defp extract_month_from_date("present"), do: ""

  defp extract_month_from_date({year, month}) when is_integer(year) and is_integer(month) do
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

  defp extract_month_from_date(_), do: ""

  defp extract_year_from_date(nil), do: ""
  defp extract_year_from_date(:undefined), do: ""
  defp extract_year_from_date("present"), do: ""
  defp extract_year_from_date({year, _month}) when is_integer(year), do: Integer.to_string(year)
  defp extract_year_from_date(_), do: ""

  defp default_work_experience do
    %{
      id: "",
      company_name: "",
      position_title: "",
      employment_type: "",
      location: "",
      start_month: "",
      start_year: "",
      end_month: "",
      end_year: "",
      currently_working: false,
      duration_months: 0,
      description: "",
      responsibilities: [],
      achievements: [],
      technologies_used: []
    }
  end

  defp default_education do
    %{
      id: "",
      institution_name: "",
      degree: "",
      field_of_study: "",
      start_date: nil,
      end_date: nil,
      grade: "",
      grade_type: "",
      description: "",
      activities: [],
      honors: [],
      relevant_coursework: []
    }
  end

  defp default_project do
    %{
      id: "",
      title: "",
      description: "",
      role: "",
      start_date: nil,
      end_date: nil,
      current_project: false,
      technologies: [],
      url: "",
      github_url: "",
      achievements: []
    }
  end

  defp safe_float(value) when is_float(value), do: value
  defp safe_float(value) when is_integer(value), do: value / 1.0
  defp safe_float(_), do: 0.0

  def search_jobs_by_location(city, country) do
    jobs = JobClient.search_jobs_by_location(city, country)

    if is_list(jobs) do
      Enum.map(jobs, &job_record_to_map/1)
    else
      []
    end
  end

  def filter_jobs(filter_criteria) do
    jobs = JobClient.filter_jobs(filter_criteria)

    if is_list(jobs) do
      Enum.map(jobs, &job_record_to_map/1)
    else
      []
    end
  end

  def save_job(user_id, job_id) do
    JobClient.save_job(user_id, job_id)
  end

  def unsave_job(user_id, job_id) do
    JobClient.unsave_job(user_id, job_id)
  end

  def get_saved_jobs(user_id) do
    saved_jobs = JobClient.get_saved_jobs(user_id)

    if is_list(saved_jobs) do
      Enum.map(saved_jobs, fn saved_job ->
        saved_job_map = saved_job_to_map(saved_job)

        case get_job_posting_by_id(saved_job_map.job_posting_id) do
          {:ok, job} ->
            Map.put(saved_job_map, :job, job)

          _ ->
            saved_job_map
        end
      end)
    else
      []
    end
  end

  def is_job_saved?(user_id, job_id) do
    JobClient.is_job_saved(user_id, job_id)
  end

  def get_resume_by_id(resume_id) do
    case JobClient.get_resume_by_id(resume_id) do
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
    case JobClient.get_resume_by_user_id(user_id) do
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
    resumes = JobClient.get_resumes_by_user_id(user_id)

    if is_list(resumes) do
      Enum.map(resumes, &resume_record_to_map/1)
    else
      []
    end
  end

  def get_complete_resume(resume_id) do
    Logger.info("=== GET_COMPLETE_RESUME START ===")
    Logger.info("Resume ID: #{resume_id}")

    case JobClient.get_complete_resume(resume_id) do
      {:ok, resume_map} ->
        converted = convert_complete_resume(resume_map)

        {:ok, converted}

      {:error, reason} ->
        {:error, reason}

      error ->
        {:error, :unexpected_response}
    end
  end

  def increment_resume_views(resume_id) do
    JobClient.increment_resume_views(resume_id)
  end

  def set_primary_resume(user_id, resume_id) do
    JobClient.set_primary_resume(user_id, resume_id)
  end

  def job_record_to_map(job_tuple) when is_tuple(job_tuple) do
    fields = Tuple.to_list(job_tuple)
    [:job_posting | values] = fields

    %{
      id: to_string_safe(Enum.at(values, 0)),
      job_id: to_string_safe(Enum.at(values, 1)),
      poster_type: to_string_safe(Enum.at(values, 2)),
      poster_id: to_string_safe(Enum.at(values, 3)),
      posted_by_user_id: to_string_safe(Enum.at(values, 4)),
      business_id: safe_value(Enum.at(values, 5)),
      title: to_string_safe(Enum.at(values, 6)),
      description: to_string_safe(Enum.at(values, 7)),
      short_description: to_string_safe(Enum.at(values, 8)),
      category: safe_value(Enum.at(values, 9)),
      sub_category: safe_value(Enum.at(values, 10)),
      industry: safe_value(Enum.at(values, 11)),
      job_function: safe_value(Enum.at(values, 12)),
      seniority_level: safe_value(Enum.at(values, 13)),
      employment_type: to_string_safe(Enum.at(values, 14)),
      location_type: to_string_safe(Enum.at(values, 16)),
      locations: convert_locations(Enum.at(values, 17)),
      salary_range: convert_salary_range(Enum.at(values, 18)),
      salary_visible: Enum.at(values, 19) || false,
      required_skills: safe_list_to_strings(Enum.at(values, 26)),
      preferred_skills: safe_list_to_strings(Enum.at(values, 27)),
      responsibilities: safe_list_to_strings(Enum.at(values, 28)),
      requirements: safe_list_to_strings(Enum.at(values, 29)),
      experience_required: convert_experience(Enum.at(values, 30)),
      benefits: safe_list_to_strings(Enum.at(values, 31)),
      status: to_string_safe(Enum.at(values, 56)),
      visibility: to_string_safe(Enum.at(values, 57)),
      featured: Enum.at(values, 58) || false,
      featured_until: Enum.at(values, 59),
      boosted: Enum.at(values, 60) || false,
      boosted_until: Enum.at(values, 61),
      applications: safe_list_to_strings(Enum.at(values, 62)),
      applications_count: Enum.at(values, 63) || 0,
      views: Enum.at(values, 64) || 0,
      shortlisted_candidates: safe_list_to_strings(Enum.at(values, 65)),
      shortlisted_count: Enum.at(values, 66) || 0,
      rejected_candidates: safe_list_to_strings(Enum.at(values, 67)),
      hired_candidates: safe_list_to_strings(Enum.at(values, 68)),
      offers_sent: Enum.at(values, 69) || 0,
      offers_accepted: Enum.at(values, 70) || 0,
      date_created: Enum.at(values, 85),
      date_updated: Enum.at(values, 86),
      date_published: Enum.at(values, 87),
      date_closed: Enum.at(values, 88)
    }
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
      location: convert_resume_location(Enum.at(values, 12)),
      website: to_string_safe(Enum.at(values, 13)),
      linkedin_url: to_string_safe(Enum.at(values, 14)),
      github_url: to_string_safe(Enum.at(values, 15)),
      portfolio_url: to_string_safe(Enum.at(values, 16)),
      work_experience: safe_value(Enum.at(values, 18)),
      total_years_experience: Enum.at(values, 19) || 0,
      education: safe_value(Enum.at(values, 20)),
      technical_skills: safe_list_to_strings(Enum.at(values, 22)),
      soft_skills: safe_list_to_strings(Enum.at(values, 23)),
      languages: safe_value(Enum.at(values, 24)),
      certifications: safe_value(Enum.at(values, 25)),
      projects: safe_value(Enum.at(values, 26)),
      job_preferences: safe_value(Enum.at(values, 27)),
      resume_pdf_ipfs_hash: to_string_safe(Enum.at(values, 30)),
      cover_letter_ipfs_hash: to_string_safe(Enum.at(values, 31)),
      portfolio_ipfs_hash: to_string_safe(Enum.at(values, 32)),
      views: Enum.at(values, 35) || 0,
      downloads: Enum.at(values, 36) || 0,
      date_created: Enum.at(values, 40),
      date_updated: Enum.at(values, 41),
      version: Enum.at(values, 43) || 1
    }
  end

  def saved_job_to_map(saved_job_tuple) when is_tuple(saved_job_tuple) do
    [:saved_job, id, user_id, job_posting_id, date_saved, notes, applied] =
      Tuple.to_list(saved_job_tuple)

    %{
      id: to_string_safe(id),
      user_id: to_string_safe(user_id),
      job_posting_id: to_string_safe(job_posting_id),
      date_saved: date_saved,
      notes: to_string_safe(notes),
      applied: applied
    }
  end

  defp convert_locations(locations) when is_list(locations) do
    Enum.map(locations, fn location ->
      if is_map(location) do
        %{
          city: to_string_safe(Map.get(location, :city, "")),
          country: to_string_safe(Map.get(location, :country, "")),
          state: to_string_safe(Map.get(location, :state, "")),
          address: to_string_safe(Map.get(location, :address, "")),
          remote: Map.get(location, :remote, false)
        }
      else
        %{city: "", country: "", state: "", address: "", remote: false}
      end
    end)
  end

  defp convert_locations(_), do: []

  defp convert_salary_range(nil), do: %{min: 0.0, max: 0.0, currency: "USD", period: "yearly"}

  defp convert_salary_range(:undefined),
    do: %{min: 0.0, max: 0.0, currency: "USD", period: "yearly"}

  defp convert_salary_range([]), do: %{min: 0.0, max: 0.0, currency: "USD", period: "yearly"}

  defp convert_salary_range(salary) when is_map(salary) do
    %{
      min: Map.get(salary, :min, 0.0),
      max: Map.get(salary, :max, 0.0),
      currency: to_string_safe(Map.get(salary, :currency, "USD")),
      period: to_string_safe(Map.get(salary, :period, "yearly"))
    }
  end

  defp convert_salary_range(_), do: %{min: 0.0, max: 0.0, currency: "USD", period: "yearly"}

  defp convert_experience(nil), do: %{min_years: 0, max_years: 0}
  defp convert_experience(:undefined), do: %{min_years: 0, max_years: 0}
  defp convert_experience([]), do: %{min_years: 0, max_years: 0}

  defp convert_experience(exp) when is_map(exp) do
    %{min_years: Map.get(exp, :min_years, 0), max_years: Map.get(exp, :max_years, 0)}
  end

  defp convert_experience(_), do: %{min_years: 0, max_years: 0}

  defp convert_resume_location(location) when is_map(location) do
    %{
      country: to_string_safe(Map.get(location, :country, "")),
      state: to_string_safe(Map.get(location, :state, "")),
      city: to_string_safe(Map.get(location, :city, "")),
      postal_code: to_string_safe(Map.get(location, :postal_code, "")),
      willing_to_relocate: Map.get(location, :willing_to_relocate, false)
    }
  end

  defp convert_resume_location(_),
    do: %{country: "", state: "", city: "", postal_code: "", willing_to_relocate: false}

  defp safe_value(nil), do: []
  defp safe_value(:undefined), do: []
  defp safe_value([]), do: []
  defp safe_value(value), do: value

  defp safe_list_to_strings(list) when is_list(list) and list != [] do
    Enum.map(list, &to_string_safe/1)
  end

  defp safe_list_to_strings(_), do: []

  defp to_string_safe(value) when is_binary(value) do
    value
  end

  defp to_string_safe(value) when is_list(value) do
    if is_charlist?(value) do
      try do
        List.to_string(value)
      rescue
        _ -> ""
      end
    else
      ""
    end
  end

  defp to_string_safe(value) when is_atom(value) and value != nil and value != :undefined do
    Atom.to_string(value)
  end

  defp to_string_safe(nil), do: ""
  defp to_string_safe(:undefined), do: ""
  defp to_string_safe(_), do: ""

  defp is_charlist?([]), do: true

  defp is_charlist?(list) when is_list(list) do
    Enum.all?(list, fn
      char when is_integer(char) and char >= 0 and char <= 1_114_111 -> true
      _ -> false
    end)
  end

  defp is_charlist?(_), do: false
end
