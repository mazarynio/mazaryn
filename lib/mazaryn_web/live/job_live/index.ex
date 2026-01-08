defmodule MazarynWeb.JobLive.Index do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Jobs
  alias Account.Businesses

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting JobLive.Index")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        jobs = Jobs.get_active_job_postings()
        user_businesses = Businesses.get_businesses_by_user_id(current_user.id)

        {:ok,
         socket
         |> assign(:current_user, current_user)
         |> assign(:jobs, jobs)
         |> assign(:filtered_jobs, jobs)
         |> assign(:search_query, "")
         |> assign(:filter_location, "")
         |> assign(:filter_employment_type, "all")
         |> assign(:filter_location_type, "all")
         |> assign(:filter_seniority, "all")
         |> assign(:show_filters, false)
         |> assign(:show_create_job_modal, false)
         |> assign(:user_businesses, user_businesses)
         |> assign(:selected_poster_type, "individual")
         |> assign(:selected_business_id, nil)
         |> assign(:page_title, "Browse Jobs")}

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Invalid session")
         |> push_redirect(to: ~p"/en/login")}
    end
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    filtered_jobs = filter_jobs(socket.assigns.jobs, query, socket.assigns)

    {:noreply,
     socket
     |> assign(:search_query, query)
     |> assign(:filtered_jobs, filtered_jobs)}
  end

  @impl true
  def handle_event("toggle_filters", _params, socket) do
    {:noreply, assign(socket, :show_filters, !socket.assigns.show_filters)}
  end

  @impl true
  def handle_event("apply_filters", params, socket) do
    employment_type = params["employment_type"] || "all"
    location_type = params["location_type"] || "all"
    seniority = params["seniority"] || "all"
    location = params["location"] || ""

    filtered_jobs =
      apply_all_filters(
        socket.assigns.jobs,
        socket.assigns.search_query,
        employment_type,
        location_type,
        seniority,
        location
      )

    {:noreply,
     socket
     |> assign(:filter_employment_type, employment_type)
     |> assign(:filter_location_type, location_type)
     |> assign(:filter_seniority, seniority)
     |> assign(:filter_location, location)
     |> assign(:filtered_jobs, filtered_jobs)
     |> assign(:show_filters, false)}
  end

  @impl true
  def handle_event("clear_filters", _params, socket) do
    {:noreply,
     socket
     |> assign(:search_query, "")
     |> assign(:filter_location, "")
     |> assign(:filter_employment_type, "all")
     |> assign(:filter_location_type, "all")
     |> assign(:filter_seniority, "all")
     |> assign(:filtered_jobs, socket.assigns.jobs)
     |> assign(:show_filters, false)}
  end

  @impl true
  def handle_event("save_job", %{"id" => job_id}, socket) do
    user_id = socket.assigns.current_user.id

    case Jobs.save_job(user_id, job_id) do
      {:ok, _} ->
        {:noreply, put_flash(socket, :info, "Job saved successfully!")}

      :ok ->
        {:noreply, put_flash(socket, :info, "Job saved successfully!")}

      {:error, reason} ->
        Logger.error("Failed to save job: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to save job")}
    end
  end

  @impl true
  def handle_event("show_create_job_modal", _params, socket) do
    {:noreply, assign(socket, :show_create_job_modal, true)}
  end

  @impl true
  def handle_event("hide_create_job_modal", _params, socket) do
    {:noreply, assign(socket, :show_create_job_modal, false)}
  end

  @impl true
  def handle_event("change_poster_type", %{"poster_type" => poster_type}, socket) do
    {:noreply, assign(socket, :selected_poster_type, poster_type)}
  end

  @impl true
  def handle_event("create_job", params, socket) do
    user_id = socket.assigns.current_user.id
    poster_type = params["poster_type"]

    Logger.info("Creating job with poster_type: #{poster_type}, user_id: #{inspect(user_id)}")

    poster_data =
      case poster_type do
        "business" ->
          business_id = params["business_id"]
          Logger.info("Business poster - business_id: #{inspect(business_id)}")

          if business_id == nil || business_id == "" do
            Logger.error("No business_id provided!")
          end

          %{
            poster_type: "business",
            poster_id: business_id,
            posted_by_user_id: user_id,
            business_id: business_id
          }

        _ ->
          Logger.info("Individual poster - using user_id: #{inspect(user_id)}")

          %{
            poster_type: "individual",
            poster_id: user_id,
            posted_by_user_id: user_id,
            business_id: ""
          }
      end

    Logger.info("Poster data: #{inspect(poster_data)}")

    locations =
      if params["location"] && params["location"] != "" do
        [
          %{
            country: params["country"] || "",
            state: params["state"] || "",
            city: params["location"],
            address: "",
            remote: params["location_type"] == "remote"
          }
        ]
      else
        []
      end

    salary_min = parse_float(params["salary_min"])
    salary_max = parse_float(params["salary_max"])

    salary_range = %{
      min: salary_min,
      max: salary_max,
      currency: params["salary_currency"] || "USD",
      period: params["salary_period"] || "yearly"
    }

    experience_min = parse_integer(params["experience_min"])
    experience_max = parse_integer(params["experience_max"])

    experience_required = %{
      min_years: experience_min,
      max_years: experience_max
    }

    required_skills =
      if params["required_skills"] && params["required_skills"] != "" do
        String.split(params["required_skills"], ",") |> Enum.map(&String.trim/1)
      else
        []
      end

    job_data = %{
      title: params["title"],
      description: params["description"],
      short_description: String.slice(params["description"], 0, 200),
      category: params["category"] || "",
      industry: params["industry"] || "",
      employment_type: params["employment_type"],
      location_type: params["location_type"],
      locations: locations,
      seniority_level: params["seniority_level"] || "mid",
      salary_range: salary_range,
      salary_visible: params["salary_visible"] == "true",
      required_skills: required_skills,
      experience_required: experience_required,
      responsibilities: [],
      requirements: [],
      benefits: [],
      application_deadline: params["application_deadline"],
      visibility: "public"
    }

    Logger.info("Job data: #{inspect(job_data)}")

    case Jobs.create_job_posting(poster_data, job_data) do
      {:ok, job_id} ->
        Logger.info("Job created successfully with ID: #{job_id}")
        Jobs.publish_job(job_id)

        jobs = Jobs.get_active_job_postings()

        {:noreply,
         socket
         |> assign(:jobs, jobs)
         |> assign(:filtered_jobs, jobs)
         |> assign(:show_create_job_modal, false)
         |> put_flash(:info, "Job posted successfully!")}

      {:error, reason} ->
        Logger.error("Failed to create job posting: #{inspect(reason)}")

        error_message =
          case reason do
            :invalid_poster -> "Invalid poster account. Please try again or contact support."
            :missing_title -> "Job title is required."
            :missing_description -> "Job description is required."
            other -> "Failed to create job posting: #{inspect(other)}"
          end

        {:noreply, put_flash(socket, :error, error_message)}
    end
  end

  defp parse_float(nil), do: 0.0
  defp parse_float(""), do: 0.0

  defp parse_float(value) when is_binary(value) do
    case Float.parse(value) do
      {float_val, _} ->
        float_val

      :error ->
        case Integer.parse(value) do
          {int_val, _} -> int_val * 1.0
          :error -> 0.0
        end
    end
  end

  defp parse_float(value) when is_number(value), do: value * 1.0
  defp parse_float(_), do: 0.0

  defp parse_integer(nil), do: 0
  defp parse_integer(""), do: 0

  defp parse_integer(value) when is_binary(value) do
    case Integer.parse(value) do
      {int_val, _} -> int_val
      :error -> 0
    end
  end

  defp parse_integer(value) when is_integer(value), do: value
  defp parse_integer(_), do: 0

  defp filter_jobs(jobs, query, _assigns) when query == "" do
    jobs
  end

  defp filter_jobs(jobs, query, _assigns) do
    search_term = String.downcase(query)

    Enum.filter(jobs, fn job ->
      String.contains?(String.downcase(job.title), search_term) ||
        String.contains?(String.downcase(job.description), search_term) ||
        String.contains?(String.downcase(job.industry), search_term)
    end)
  end

  defp apply_all_filters(jobs, search_query, employment_type, location_type, seniority, location) do
    jobs
    |> filter_by_search(search_query)
    |> filter_by_employment_type(employment_type)
    |> filter_by_location_type(location_type)
    |> filter_by_seniority(seniority)
    |> filter_by_location(location)
  end

  defp filter_by_search(jobs, ""), do: jobs

  defp filter_by_search(jobs, query) do
    search_term = String.downcase(query)

    Enum.filter(jobs, fn job ->
      String.contains?(String.downcase(job.title), search_term) ||
        String.contains?(String.downcase(job.description), search_term)
    end)
  end

  defp filter_by_employment_type(jobs, "all"), do: jobs

  defp filter_by_employment_type(jobs, type) do
    Enum.filter(jobs, fn job -> job.employment_type == type end)
  end

  defp filter_by_location_type(jobs, "all"), do: jobs

  defp filter_by_location_type(jobs, type) do
    Enum.filter(jobs, fn job -> job.location_type == type end)
  end

  defp filter_by_seniority(jobs, "all"), do: jobs

  defp filter_by_seniority(jobs, level) do
    Enum.filter(jobs, fn job -> job.seniority_level == level end)
  end

  defp filter_by_location(jobs, ""), do: jobs

  defp filter_by_location(jobs, location) do
    search_location = String.downcase(location)

    Enum.filter(jobs, fn job ->
      Enum.any?(job.locations, fn loc ->
        String.contains?(String.downcase(loc.city), search_location) ||
          String.contains?(String.downcase(loc.country), search_location)
      end)
    end)
  end

  defp format_salary_range(salary_range) do
    if salary_range.min > 0 && salary_range.max > 0 do
      min = format_currency(salary_range.min, salary_range.currency)
      max = format_currency(salary_range.max, salary_range.currency)
      "#{min} - #{max} / #{salary_range.period}"
    else
      "Competitive"
    end
  end

  defp format_currency(amount, currency) do
    formatted = :erlang.float_to_binary(amount / 1000, decimals: 0)
    "#{currency} #{formatted}k"
  end

  defp format_location(locations) when is_list(locations) and length(locations) > 0 do
    location = hd(locations)

    if location.city != "" && location.country != "" do
      "#{location.city}, #{location.country}"
    else
      location.country
    end
  end

  defp format_location(_), do: "Location not specified"

  defp format_date(nil), do: ""

  defp format_date({{year, month, day}, _time}) do
    "#{day}/#{month}/#{year}"
  end

  defp format_date(_), do: ""

  defp time_ago(nil), do: "Just now"
  defp time_ago(false), do: "Just now"
  defp time_ago(:undefined), do: "Just now"

  defp time_ago(date) when is_tuple(date) do
    now = :calendar.universal_time()

    diff_seconds =
      :calendar.datetime_to_gregorian_seconds(now) -
        :calendar.datetime_to_gregorian_seconds(date)

    cond do
      diff_seconds < 60 ->
        "Just now"

      diff_seconds < 3600 ->
        minutes = div(diff_seconds, 60)
        "#{minutes} #{if minutes == 1, do: "minute", else: "minutes"} ago"

      diff_seconds < 86400 ->
        hours = div(diff_seconds, 3600)
        "#{hours} #{if hours == 1, do: "hour", else: "hours"} ago"

      diff_seconds < 2_592_000 ->
        days = div(diff_seconds, 86400)
        "#{days} #{if days == 1, do: "day", else: "days"} ago"

      true ->
        format_date(date)
    end
  end

  defp time_ago(_), do: "Just now"
end
