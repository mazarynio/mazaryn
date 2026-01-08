defmodule MazarynWeb.ResumeLive.Show do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Jobs

  @impl true
  def mount(%{"id" => resume_id}, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting ResumeLive.Show for resume_id: #{resume_id}")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        case Jobs.get_complete_resume(resume_id) do
          {:ok, resume} ->
            Jobs.increment_resume_views(resume_id)
            assign_resume(socket, current_user, resume, resume_id)

          {:error, :resume_not_found} ->
            Logger.warn("Complete resume not found, falling back to basic resume")
            fallback_to_basic_resume(socket, current_user, resume_id)

          {:error, reason} ->
            Logger.error("Error getting complete resume: #{inspect(reason)}")
            fallback_to_basic_resume(socket, current_user, resume_id)
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Invalid session")
         |> push_redirect(to: ~p"/en/login")}
    end
  end

  defp fallback_to_basic_resume(socket, current_user, resume_id) do
    case Jobs.get_resume_by_id(resume_id) do
      {:ok, resume} ->
        Jobs.increment_resume_views(resume_id)
        assign_resume(socket, current_user, resume, resume_id)

      {:error, _} ->
        {:ok,
         socket
         |> put_flash(:error, "Resume not found")
         |> push_navigate(to: ~p"/en/resumes")}
    end
  end

  defp assign_resume(socket, current_user, resume, resume_id) do
    {:ok,
     socket
     |> assign(:current_user, current_user)
     |> assign(:resume, resume)
     |> assign(:resume_id, resume_id)
     |> assign(
       :page_title,
       "#{resume.full_name || "Resume"} - #{resume.professional_title || ""}"
     )
     |> assign(:locale, "en")}
  end

  @impl true
  def handle_event("edit_resume", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/en/resumes/#{socket.assigns.resume_id}/edit")}
  end

  @impl true
  def handle_event("download_pdf", _params, socket) do
    {:noreply, put_flash(socket, :info, "PDF download feature coming soon!")}
  end

  @impl true
  def handle_event("set_primary", _params, socket) do
    user_id = socket.assigns.current_user.id
    resume_id = socket.assigns.resume_id

    case Jobs.set_primary_resume(user_id, resume_id) do
      :ok ->
        case Jobs.get_complete_resume(resume_id) do
          {:ok, updated_resume} ->
            {:noreply,
             socket
             |> assign(:resume, updated_resume)
             |> put_flash(:info, "This is now your primary resume")}

          {:error, _} ->
            case Jobs.get_resume_by_id(resume_id) do
              {:ok, updated_resume} ->
                {:noreply,
                 socket
                 |> assign(:resume, updated_resume)
                 |> put_flash(:info, "This is now your primary resume")}

              {:error, _} ->
                {:noreply, put_flash(socket, :error, "Failed to refresh resume")}
            end
        end

      {:error, reason} ->
        Logger.error("Failed to set primary resume: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to set primary resume")}
    end
  end

  defp format_date(nil), do: "Present"
  defp format_date(:undefined), do: "Present"
  defp format_date("present"), do: "Present"

  defp format_date({year, month, day})
       when is_integer(year) and is_integer(month) and is_integer(day) do
    months = [
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    ]

    "#{Enum.at(months, month - 1)} #{day}, #{year}"
  end

  defp format_date({year, month}) when is_integer(year) and is_integer(month) do
    months = [
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    ]

    "#{Enum.at(months, month - 1)} #{year}"
  end

  defp format_date(_), do: "Present"
end
