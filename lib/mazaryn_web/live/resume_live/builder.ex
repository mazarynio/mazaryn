defmodule MazarynWeb.ResumeLive.Builder do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Resumes

  @impl true
  def mount(params, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting ResumeLive.Builder")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        resume_id = Map.get(params, "id")

        {mode, resume} =
          if resume_id do
            case Resumes.get_resume_by_id(resume_id) do
              {:ok, resume} -> {:edit, resume}
              {:error, _} -> {:new, nil}
            end
          else
            {:new, nil}
          end

        {:ok,
         socket
         |> assign(:current_user, current_user)
         |> assign(:mode, mode)
         |> assign(:resume, resume)
         |> assign(:step, 1)
         |> assign(:form_data, initialize_form_data(resume))
         |> assign(:page_title, if(mode == :edit, do: "Edit Resume", else: "Create Resume"))}

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Invalid session")
         |> push_redirect(to: ~p"/en/login")}
    end
  end

  @impl true
  def handle_event("next_step", _params, socket) do
    {:noreply, assign(socket, :step, min(socket.assigns.step + 1, 5))}
  end

  @impl true
  def handle_event("prev_step", _params, socket) do
    {:noreply, assign(socket, :step, max(socket.assigns.step - 1, 1))}
  end

  @impl true
  def handle_event("go_to_step", %{"step" => step}, socket) do
    {:noreply, assign(socket, :step, String.to_integer(step))}
  end

  @impl true
  def handle_event("update_basic_info", params, socket) do
    form_data = socket.assigns.form_data

    updated_data =
      Map.merge(form_data, %{
        full_name: params["full_name"],
        professional_title: params["professional_title"],
        email: params["email"],
        phone: params["phone"],
        location_city: params["location_city"],
        location_country: params["location_country"],
        linkedin_url: params["linkedin_url"],
        github_url: params["github_url"],
        website: params["website"],
        summary: params["summary"]
      })

    {:noreply,
     socket
     |> assign(:form_data, updated_data)
     |> assign(:step, 2)}
  end

  @impl true
  def handle_event("add_work_experience", params, socket) do
    form_data = socket.assigns.form_data
    experiences = form_data[:work_experience] || []

    new_experience = %{
      id: generate_id(),
      company: params["company"],
      job_title: params["job_title"],
      location: params["location"],
      start_month: params["start_month"],
      start_year: params["start_year"],
      end_month: params["end_month"],
      end_year: params["end_year"],
      currently_working: params["currently_working"] == "true",
      description: params["description"]
    }

    updated_data = Map.put(form_data, :work_experience, [new_experience | experiences])

    {:noreply, assign(socket, :form_data, updated_data)}
  end

  @impl true
  def handle_event("remove_work_experience", %{"id" => id}, socket) do
    form_data = socket.assigns.form_data
    experiences = form_data[:work_experience] || []
    updated_experiences = Enum.reject(experiences, fn exp -> exp.id == id end)
    updated_data = Map.put(form_data, :work_experience, updated_experiences)

    {:noreply, assign(socket, :form_data, updated_data)}
  end

  @impl true
  def handle_event("add_education", params, socket) do
    form_data = socket.assigns.form_data
    education = form_data[:education] || []

    new_education = %{
      id: generate_id(),
      institution: params["institution"],
      degree: params["degree"],
      field_of_study: params["field_of_study"],
      start_month: params["start_month"],
      start_year: params["start_year"],
      end_month: params["end_month"],
      end_year: params["end_year"],
      gpa: params["gpa"],
      description: params["description"]
    }

    updated_data = Map.put(form_data, :education, [new_education | education])

    {:noreply, assign(socket, :form_data, updated_data)}
  end

  @impl true
  def handle_event("remove_education", %{"id" => id}, socket) do
    form_data = socket.assigns.form_data
    education = form_data[:education] || []
    updated_education = Enum.reject(education, fn edu -> edu.id == id end)
    updated_data = Map.put(form_data, :education, updated_education)

    {:noreply, assign(socket, :form_data, updated_data)}
  end

  @impl true
  def handle_event("add_skill", params, socket) do
    skill = params["skill"]

    if skill && String.trim(skill) != "" do
      form_data = socket.assigns.form_data
      skills = form_data[:technical_skills] || []
      updated_data = Map.put(form_data, :technical_skills, [String.trim(skill) | skills])

      {:noreply, assign(socket, :form_data, updated_data)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("remove_skill", %{"skill" => skill}, socket) do
    form_data = socket.assigns.form_data
    skills = form_data[:technical_skills] || []
    updated_skills = Enum.reject(skills, fn s -> s == skill end)
    updated_data = Map.put(form_data, :technical_skills, updated_skills)

    {:noreply, assign(socket, :form_data, updated_data)}
  end

  @impl true
  def handle_event("save_resume", _params, socket) do
    form_data = socket.assigns.form_data
    user_id = socket.assigns.current_user.id
    mode = socket.assigns.mode

    resume_data = build_resume_data(form_data)

    result =
      if mode == :edit do
        resume_id = socket.assigns.resume.id

        case Resumes.update_resume(resume_id, resume_data) do
          :ok -> {:ok, resume_id}
          error -> error
        end
      else
        Resumes.create_resume(user_id, resume_data)
      end

    case result do
      {:ok, resume_id} ->
        {:noreply,
         socket
         |> put_flash(
           :info,
           if(mode == :edit,
             do: "Resume updated successfully!",
             else: "Resume created successfully!"
           )
         )
         |> push_navigate(to: ~p"/en/resumes/#{resume_id}")}

      {:error, reason} ->
        Logger.error("Failed to save resume: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to save resume")}
    end
  end

  defp initialize_form_data(nil) do
    %{
      full_name: "",
      professional_title: "",
      email: "",
      phone: "",
      location_city: "",
      location_country: "",
      linkedin_url: "",
      github_url: "",
      website: "",
      summary: "",
      work_experience: [],
      education: [],
      technical_skills: [],
      soft_skills: []
    }
  end

  defp initialize_form_data(resume) do
    %{
      full_name: resume.full_name || "",
      professional_title: resume.professional_title || "",
      email: resume.email || "",
      phone: resume.phone || "",
      location_city: get_in(resume.location, [:city]) || "",
      location_country: get_in(resume.location, [:country]) || "",
      linkedin_url: resume.linkedin_url || "",
      github_url: resume.github_url || "",
      website: resume.website || "",
      summary: resume.summary || "",
      work_experience: resume.work_experience || [],
      education: resume.education || [],
      technical_skills: resume.technical_skills || [],
      soft_skills: resume.soft_skills || []
    }
  end

  defp build_resume_data(form_data) do
    %{
      full_name: form_data[:full_name],
      professional_title: form_data[:professional_title],
      email: form_data[:email],
      phone: form_data[:phone],
      location: %{
        city: form_data[:location_city],
        country: form_data[:location_country],
        state: "",
        postal_code: "",
        willing_to_relocate: false
      },
      linkedin_url: form_data[:linkedin_url],
      github_url: form_data[:github_url],
      website: form_data[:website],
      summary: form_data[:summary],
      technical_skills: form_data[:technical_skills] || [],
      soft_skills: form_data[:soft_skills] || [],
      visibility: "private",
      resume_type: "platform_generated"
    }
  end

  defp generate_id do
    :crypto.strong_rand_bytes(16) |> Base.url_encode64() |> binary_part(0, 16)
  end
end
