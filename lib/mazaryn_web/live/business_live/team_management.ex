defmodule MazarynWeb.BusinessLive.TeamManagement do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Businesses

  @impl true
  def mount(%{"id" => business_id}, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting BusinessLive.TeamManagement for business_id: #{business_id}")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        case Businesses.get_business_by_id(business_id) do
          {:ok, business} ->
            current_user_id = normalize_id(current_user.id)
            business_user_id = normalize_id(business.user_id)

            if current_user_id == business_user_id do
              team_members = Businesses.get_team_members(business_id)

              {:ok,
               socket
               |> assign(:current_user, current_user)
               |> assign(:business, business)
               |> assign(:business_id, business_id)
               |> assign(:team_members, team_members)
               |> assign(:show_invite_modal, false)
               |> assign(:invite_email, "")
               |> assign(:invite_role, "member")
               |> assign(:page_title, "Team Management - #{business.company_name}")}
            else
              {:ok,
               socket
               |> put_flash(:error, "You don't have permission to manage this business team")
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
  def handle_event("show_invite_modal", _params, socket) do
    {:noreply, assign(socket, :show_invite_modal, true)}
  end

  @impl true
  def handle_event("hide_invite_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_invite_modal, false)
     |> assign(:invite_email, "")
     |> assign(:invite_role, "member")}
  end

  @impl true
  def handle_event("invite_member", %{"email" => email, "role" => role}, socket) do
    business_id = socket.assigns.business_id

    Logger.info("Inviting member: #{email} with role: #{role}")

    case find_user_by_email(email) do
      {:ok, user} ->
        case Businesses.add_pending_invitation(business_id, user.id, role) do
          :ok ->
            {:noreply,
             socket
             |> assign(:show_invite_modal, false)
             |> put_flash(:info, "Invitation sent to #{email}")}

          {:error, reason} ->
            Logger.error("Failed to invite member: #{inspect(reason)}")
            {:noreply, put_flash(socket, :error, "Failed to send invitation")}
        end

      {:error, :not_found} ->
        {:noreply, put_flash(socket, :error, "User with email #{email} not found")}
    end
  end

  @impl true
  def handle_event("remove_member", %{"user_id" => user_id}, socket) do
    business_id = socket.assigns.business_id

    Logger.info("Removing team member: #{user_id}")

    case Businesses.remove_team_member(business_id, user_id) do
      :ok ->
        team_members = Businesses.get_team_members(business_id)

        {:noreply,
         socket
         |> assign(:team_members, team_members)
         |> put_flash(:info, "Team member removed successfully")}

      {:error, reason} ->
        Logger.error("Failed to remove team member: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to remove team member")}
    end
  end

  @impl true
  def handle_event("update_role", %{"user_id" => user_id, "role" => new_role}, socket) do
    business_id = socket.assigns.business_id

    Logger.info("Updating role for user #{user_id} to #{new_role}")

    case Businesses.update_team_member_role(business_id, user_id, new_role) do
      :ok ->
        team_members = Businesses.get_team_members(business_id)

        {:noreply,
         socket
         |> assign(:team_members, team_members)
         |> put_flash(:info, "Role updated successfully")}

      {:error, reason} ->
        Logger.error("Failed to update role: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to update role")}
    end
  end

  defp find_user_by_email(email) do
    case Users.get_by_email(email) do
      {:ok, user} -> {:ok, user}
      _ -> {:error, :not_found}
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
