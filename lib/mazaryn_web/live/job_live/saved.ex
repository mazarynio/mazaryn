defmodule MazarynWeb.JobLive.Saved do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Jobs

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting JobLive.Saved")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        saved_jobs = Jobs.get_saved_jobs(current_user.id)

        job_details =
          Enum.map(saved_jobs, fn saved_job ->
            case Jobs.get_job_posting_by_id(saved_job.job_posting_id) do
              {:ok, job} ->
                Map.put(saved_job, :job, job)

              {:error, _} ->
                Map.put(saved_job, :job, nil)
            end
          end)
          |> Enum.filter(fn saved_job -> saved_job.job != nil end)

        {:ok,
         socket
         |> assign(:current_user, current_user)
         |> assign(:saved_jobs, job_details)
         |> assign(:page_title, "Saved Jobs")}

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Invalid session")
         |> push_redirect(to: ~p"/en/login")}
    end
  end

  @impl true
  def handle_event("unsave_job", %{"id" => job_id}, socket) do
    user_id = socket.assigns.current_user.id

    case Jobs.unsave_job(user_id, job_id) do
      :ok ->
        saved_jobs =
          Enum.reject(socket.assigns.saved_jobs, fn saved_job ->
            saved_job.job_posting_id == job_id
          end)

        {:noreply,
         socket
         |> assign(:saved_jobs, saved_jobs)
         |> put_flash(:info, "Job removed from saved jobs")}

      {:error, reason} ->
        Logger.error("Failed to unsave job: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to remove job")}
    end
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
end
