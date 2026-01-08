defmodule MazarynWeb.JobLive.Show do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Jobs

  @impl true
  def mount(%{"id" => job_id}, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting JobLive.Show for job_id: #{job_id}")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        case Jobs.get_job_posting_by_id(job_id) do
          {:ok, job} ->
            Logger.info("Job date_created: #{inspect(job.date_created)}")
            Logger.info("Job date_published: #{inspect(job.date_published)}")

            Jobs.increment_view_count(job_id)

            is_saved = Jobs.is_job_saved?(current_user.id, job_id)

            {:ok,
             socket
             |> assign(:current_user, current_user)
             |> assign(:job, job)
             |> assign(:job_id, job_id)
             |> assign(:is_saved, is_saved)
             |> assign(:show_apply_modal, false)
             |> assign(:page_title, job.title)}

          {:error, _reason} ->
            {:ok,
             socket
             |> put_flash(:error, "Job not found")
             |> push_navigate(to: ~p"/en/jobs")}
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Invalid session")
         |> push_redirect(to: ~p"/en/login")}
    end
  end

  @impl true
  def handle_event("show_apply_modal", _params, socket) do
    {:noreply, assign(socket, :show_apply_modal, true)}
  end

  @impl true
  def handle_event("hide_apply_modal", _params, socket) do
    {:noreply, assign(socket, :show_apply_modal, false)}
  end

  @impl true
  def handle_event("save_job", _params, socket) do
    user_id = socket.assigns.current_user.id
    job_id = socket.assigns.job_id

    if socket.assigns.is_saved do
      case Jobs.unsave_job(user_id, job_id) do
        :ok ->
          {:noreply,
           socket
           |> assign(:is_saved, false)
           |> put_flash(:info, "Job removed from saved jobs")}

        {:error, reason} ->
          Logger.error("Failed to unsave job: #{inspect(reason)}")
          {:noreply, put_flash(socket, :error, "Failed to unsave job")}
      end
    else
      case Jobs.save_job(user_id, job_id) do
        {:ok, _} ->
          {:noreply,
           socket
           |> assign(:is_saved, true)
           |> put_flash(:info, "Job saved successfully!")}

        :ok ->
          {:noreply,
           socket
           |> assign(:is_saved, true)
           |> put_flash(:info, "Job saved successfully!")}

        {:error, reason} ->
          Logger.error("Failed to save job: #{inspect(reason)}")
          {:noreply, put_flash(socket, :error, "Failed to save job")}
      end
    end
  end

  @impl true
  def handle_event("apply_to_job", params, socket) do
    Logger.info("Applying to job with params: #{inspect(params)}")

    {:noreply,
     socket
     |> assign(:show_apply_modal, false)
     |> put_flash(:info, "Application submitted successfully! We'll notify the employer.")}
  end

  defp format_salary_range(salary_range) when is_map(salary_range) do
    if salary_range.min > 0 && salary_range.max > 0 do
      min = format_currency(salary_range.min, salary_range.currency)
      max = format_currency(salary_range.max, salary_range.currency)
      "#{min} - #{max} / #{salary_range.period}"
    else
      "Competitive salary"
    end
  end

  defp format_salary_range(_), do: "Competitive salary"

  defp format_currency(amount, currency) do
    formatted = :erlang.float_to_binary(amount / 1000, decimals: 0)
    "#{currency} #{formatted}k"
  end

  defp format_location(locations) when is_list(locations) and length(locations) > 0 do
    Enum.map(locations, fn location ->
      if location.city != "" && location.country != "" do
        "#{location.city}, #{location.country}"
      else
        location.country
      end
    end)
    |> Enum.join(" • ")
  end

  defp format_location(_), do: "Location not specified"

  defp format_date(nil), do: ""
  defp format_date(:undefined), do: ""
  defp format_date(false), do: ""

  defp format_date({{year, month, day}, _time})
       when is_integer(year) and is_integer(month) and is_integer(day) do
    months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    month_name = Enum.at(months, month - 1)
    "#{month_name} #{day}, #{year}"
  end

  defp format_date(_), do: ""

  defp format_datetime(nil), do: "Not specified"
  defp format_datetime(:undefined), do: "Not specified"
  defp format_datetime(false), do: "Not specified"

  defp format_datetime({{year, month, day}, {hour, minute, _second}})
       when is_integer(year) and is_integer(month) and is_integer(day) do
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

    month_name = Enum.at(months, month - 1)

    {display_hour, am_pm} =
      if hour >= 12 do
        {if(hour == 12, do: 12, else: hour - 12), "PM"}
      else
        {if(hour == 0, do: 12, else: hour), "AM"}
      end

    "#{month_name} #{day}, #{year} at #{display_hour}:#{String.pad_leading("#{minute}", 2, "0")} #{am_pm}"
  end

  defp format_datetime(_), do: "Not specified"

  defp time_ago(nil), do: "Recently"
  defp time_ago(false), do: "Recently"
  defp time_ago(:undefined), do: "Recently"
  defp time_ago([]), do: "Recently"

  defp time_ago(date) when is_tuple(date) and tuple_size(date) == 2 do
    try do
      now = :calendar.universal_time()

      case validate_datetime(date) do
        true ->
          diff_seconds =
            :calendar.datetime_to_gregorian_seconds(now) -
              :calendar.datetime_to_gregorian_seconds(date)

          if diff_seconds < 0 do
            "Recently"
          else
            format_time_difference(diff_seconds, date)
          end

        false ->
          "Recently"
      end
    rescue
      _ -> "Recently"
    end
  end

  defp time_ago(_), do: "Recently"

  defp validate_datetime({{year, month, day}, {hour, minute, second}})
       when is_integer(year) and is_integer(month) and is_integer(day) and
              is_integer(hour) and is_integer(minute) and is_integer(second) do
    year > 1970 and year < 2100 and
      month >= 1 and month <= 12 and
      day >= 1 and day <= 31 and
      hour >= 0 and hour < 24 and
      minute >= 0 and minute < 60 and
      second >= 0 and second < 60
  end

  defp validate_datetime(_), do: false

  defp format_time_difference(diff_seconds, original_date) do
    cond do
      diff_seconds < 60 ->
        "Just now"

      diff_seconds < 3600 ->
        minutes = div(diff_seconds, 60)
        "#{minutes} #{if minutes == 1, do: "minute", else: "minutes"} ago"

      diff_seconds < 86400 ->
        hours = div(diff_seconds, 3600)
        "#{hours} #{if hours == 1, do: "hour", else: "hours"} ago"

      diff_seconds < 604_800 ->
        # Less than 7 days
        days = div(diff_seconds, 86400)
        "#{days} #{if days == 1, do: "day", else: "days"} ago"

      diff_seconds < 2_592_000 ->
        # Less than 30 days
        weeks = div(diff_seconds, 604_800)
        "#{weeks} #{if weeks == 1, do: "week", else: "weeks"} ago"

      diff_seconds < 31_536_000 ->
        # Less than 1 year
        months = div(diff_seconds, 2_592_000)
        "#{months} #{if months == 1, do: "month", else: "months"} ago"

      true ->
        format_date(original_date)
    end
  end

  defp format_employment_type(nil), do: "Not specified"
  defp format_employment_type(""), do: "Not specified"
  defp format_employment_type([]), do: "Not specified"

  defp format_employment_type(type) when is_binary(type) do
    type
    |> String.replace("_", " ")
    |> String.split(" ")
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end

  defp format_employment_type(_), do: "Not specified"

  defp format_seniority(nil), do: "Not specified"
  defp format_seniority(""), do: "Not specified"
  defp format_seniority([]), do: "Not specified"

  defp format_seniority(level) when is_binary(level) do
    String.capitalize(level)
  end

  defp format_seniority(_), do: "Not specified"

  defp format_location_type(nil), do: "Not specified"
  defp format_location_type(""), do: "Not specified"
  defp format_location_type([]), do: "Not specified"

  defp format_location_type(type) when is_binary(type) do
    case type do
      "remote" -> "Remote"
      "on_site" -> "On-site"
      "hybrid" -> "Hybrid"
      _ -> String.capitalize(type)
    end
  end

  defp format_location_type(_), do: "Not specified"

  defp get_company_name(job) do
    cond do
      job.poster_type == "business" && job.business_id != "" ->
        "Company Name"

      job.poster_type == "individual" ->
        "Individual Poster"

      true ->
        "Company"
    end
  end
end
