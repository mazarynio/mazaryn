defmodule MazarynWeb.BusinessLive.Jobs do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Businesses
  alias Account.Jobs

  @impl true
  def mount(%{"id" => business_id}, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting BusinessLive.Jobs for business_id: #{business_id}")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        case Businesses.get_business_by_id(business_id) do
          {:ok, business} ->
            current_user_id = normalize_id(current_user.id)
            business_user_id = normalize_id(business.user_id)

            if current_user_id == business_user_id do
              job_postings = Jobs.get_job_postings_by_poster("business", business_id)

              {:ok,
               socket
               |> assign(:current_user, current_user)
               |> assign(:business, business)
               |> assign(:business_id, business_id)
               |> assign(:job_postings, job_postings)
               |> assign(:show_create_modal, false)
               |> assign(:editing_job, nil)
               |> assign(:page_title, "Jobs - #{business.company_name}")}
            else
              {:ok,
               socket
               |> put_flash(:error, "You don't have permission to manage this business jobs")
               |> push_navigate(to: ~p"/en/business")}
            end

          {:error, _reason} ->
            {:ok,
             socket
             |> put_flash(:error, "Business not found")
             |> push_navigate(to: ~p"/en/business")}
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Invalid session")
         |> push_redirect(to: ~p"/en/login")}
    end
  end

  @impl true
  def handle_event("show_create_modal", _params, socket) do
    {:noreply, assign(socket, :show_create_modal, true)}
  end

  @impl true
  def handle_event("hide_create_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_create_modal, false)
     |> assign(:editing_job, nil)}
  end

  @impl true
  def handle_event("create_job", params, socket) do
    business_id = socket.assigns.business_id
    user_id = socket.assigns.current_user.id

    poster_data = %{
      poster_type: "business",
      poster_id: business_id,
      posted_by_user_id: user_id,
      business_id: business_id
    }

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

    salary_range = %{
      min: String.to_float(params["salary_min"] || "0"),
      max: String.to_float(params["salary_max"] || "0"),
      currency: params["salary_currency"] || "USD",
      period: params["salary_period"] || "yearly"
    }

    experience_required = %{
      min_years: String.to_integer(params["experience_min"] || "0"),
      max_years: String.to_integer(params["experience_max"] || "0")
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

    Logger.info("Creating job posting for business: #{business_id}")

    case Jobs.create_job_posting(poster_data, job_data) do
      {:ok, job_id} ->
        job_postings = Jobs.get_job_postings_by_poster("business", business_id)

        {:noreply,
         socket
         |> assign(:job_postings, job_postings)
         |> assign(:show_create_modal, false)
         |> put_flash(:info, "Job posting created successfully!")}

      {:error, reason} ->
        Logger.error("Failed to create job posting: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to create job posting")}
    end
  end

  @impl true
  def handle_event("edit_job", %{"id" => job_id}, socket) do
    job_postings = socket.assigns.job_postings
    job = Enum.find(job_postings, fn j -> j.id == job_id end)

    {:noreply,
     socket
     |> assign(:editing_job, job)
     |> assign(:show_create_modal, true)}
  end

  @impl true
  def handle_event("update_job", %{"id" => job_id} = params, socket) do
    update_data = %{
      title: params["title"],
      description: params["description"],
      employment_type: params["employment_type"],
      location_type: params["location_type"],
      seniority_level: params["seniority_level"]
    }

    case Jobs.update_job_posting(job_id, update_data) do
      :ok ->
        job_postings = Jobs.get_job_postings_by_poster("business", socket.assigns.business_id)

        {:noreply,
         socket
         |> assign(:job_postings, job_postings)
         |> assign(:show_create_modal, false)
         |> assign(:editing_job, nil)
         |> put_flash(:info, "Job posting updated successfully!")}

      {:error, reason} ->
        Logger.error("Failed to update job posting: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to update job posting")}
    end
  end

  @impl true
  def handle_event("delete_job", %{"id" => job_id}, socket) do
    Logger.info("Deleting job posting: #{job_id}")

    case Jobs.delete_job_posting(job_id) do
      :ok ->
        job_postings = Jobs.get_job_postings_by_poster("business", socket.assigns.business_id)

        {:noreply,
         socket
         |> assign(:job_postings, job_postings)
         |> put_flash(:info, "Job posting deleted successfully")}

      {:error, reason} ->
        Logger.error("Failed to delete job posting: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to delete job posting")}
    end
  end

  @impl true
  def handle_event("toggle_status", %{"id" => job_id, "status" => current_status}, socket) do
    result =
      case current_status do
        "active" -> Jobs.close_job(job_id)
        "draft" -> Jobs.publish_job(job_id)
        _ -> Jobs.publish_job(job_id)
      end

    case result do
      :ok ->
        job_postings = Jobs.get_job_postings_by_poster("business", socket.assigns.business_id)

        {:noreply,
         socket
         |> assign(:job_postings, job_postings)
         |> put_flash(:info, "Job status updated")}

      {:error, reason} ->
        Logger.error("Failed to update job status: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to update job status")}
    end
  end

  defp normalize_id(id) when is_binary(id), do: id

  defp normalize_id(id) when is_list(id) do
    try do
      List.to_string(id)
    rescue
      _ -> ""
    end
  end

  defp normalize_id(_id), do: ""
end
