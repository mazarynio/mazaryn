defmodule MazarynWeb.ResumeLive.Index do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Resumes

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting ResumeLive.Index")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        resumes = Resumes.get_resumes_by_user_id(current_user.id)

        {:ok,
         socket
         |> assign(:current_user, current_user)
         |> assign(:resumes, resumes)
         |> assign(:show_delete_modal, false)
         |> assign(:resume_to_delete, nil)
         |> assign(:page_title, "My Resumes")}

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Invalid session")
         |> push_redirect(to: ~p"/en/login")}
    end
  end

  @impl true
  def handle_event("create_new_resume", _params, socket) do
    {:noreply, push_navigate(socket, to: ~p"/en/resumes/builder")}
  end

  @impl true
  def handle_event("edit_resume", %{"id" => resume_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/en/resumes/#{resume_id}/edit")}
  end

  @impl true
  def handle_event("view_resume", %{"id" => resume_id}, socket) do
    {:noreply, push_navigate(socket, to: ~p"/en/resumes/#{resume_id}")}
  end

  @impl true
  def handle_event("set_primary", %{"id" => resume_id}, socket) do
    user_id = socket.assigns.current_user.id

    case Resumes.set_primary_resume(user_id, resume_id) do
      :ok ->
        resumes = Resumes.get_resumes_by_user_id(user_id)

        {:noreply,
         socket
         |> assign(:resumes, resumes)
         |> put_flash(:info, "Primary resume updated successfully")}

      {:error, reason} ->
        Logger.error("Failed to set primary resume: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to update primary resume")}
    end
  end

  @impl true
  def handle_event("show_delete_modal", %{"id" => resume_id}, socket) do
    {:noreply,
     socket
     |> assign(:show_delete_modal, true)
     |> assign(:resume_to_delete, resume_id)}
  end

  @impl true
  def handle_event("hide_delete_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_delete_modal, false)
     |> assign(:resume_to_delete, nil)}
  end

  @impl true
  def handle_event("confirm_delete", _params, socket) do
    resume_id = socket.assigns.resume_to_delete
    user_id = socket.assigns.current_user.id

    case Resumes.delete_resume(resume_id) do
      :ok ->
        resumes = Resumes.get_resumes_by_user_id(user_id)

        {:noreply,
         socket
         |> assign(:resumes, resumes)
         |> assign(:show_delete_modal, false)
         |> assign(:resume_to_delete, nil)
         |> put_flash(:info, "Resume deleted successfully")}

      {:error, reason} ->
        Logger.error("Failed to delete resume: #{inspect(reason)}")

        {:noreply,
         socket
         |> assign(:show_delete_modal, false)
         |> assign(:resume_to_delete, nil)
         |> put_flash(:error, "Failed to delete resume")}
    end
  end

  defp format_date(nil), do: "Present"
  defp format_date(:undefined), do: "Present"

  defp format_date({{year, month, _day}, _time}) do
    months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    month_name = Enum.at(months, month - 1)
    "#{month_name} #{year}"
  end

  defp format_date(_), do: "Present"
end
