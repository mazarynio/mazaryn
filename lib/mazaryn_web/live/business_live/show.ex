defmodule MazarynWeb.BusinessLive.Show do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Businesses

  @impl true
  def mount(%{"id" => business_id}, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting BusinessLive.Show for business_id: #{business_id}")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        current_user_id = normalize_id(current_user.id)
        Logger.info("Current user found: #{current_user_id}")

        case Businesses.get_business_by_id(business_id) do
          {:ok, business} ->
            business_user_id = normalize_id(business.user_id)
            Logger.info("Business found with owner: #{business_user_id}")

            Logger.info(
              "Comparing: current_user_id=#{current_user_id} vs business_user_id=#{business_user_id}"
            )

            if current_user_id == business_user_id do
              Logger.info("User owns this business, allowing access")

              {:ok,
               socket
               |> assign(:current_user, current_user)
               |> assign(:business, business)
               |> assign(:business_id, business_id)
               |> assign(:editing, false)
               |> assign(:show_delete_modal, false)
               |> assign(:page_title, business.company_name || "Business Profile")}
            else
              Logger.error(
                "User #{current_user_id} does not own business #{business_id} (owner: #{business_user_id})"
              )

              {:ok,
               socket
               |> put_flash(:error, "You don't have permission to view this business")
               |> push_navigate(to: ~p"/en/business")}
            end

          {:error, reason} ->
            Logger.error("Business not found: #{inspect(reason)}")

            {:ok,
             socket
             |> put_flash(:error, "Business not found")
             |> push_navigate(to: ~p"/en/business")}
        end

      {:error, reason} ->
        Logger.error("Invalid session: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Invalid session")
         |> push_redirect(to: ~p"/en/login")}
    end
  end

  @impl true
  def handle_event("edit", _params, socket) do
    {:noreply, assign(socket, :editing, true)}
  end

  @impl true
  def handle_event("cancel_edit", _params, socket) do
    {:noreply, assign(socket, :editing, false)}
  end

  @impl true
  def handle_event("save_business", %{"business" => params}, socket) do
    business_id = socket.assigns.business_id

    update_map = %{
      company_name: params["company_name"] || "",
      business_description: params["business_description"] || "",
      website: params["website"] || "",
      industry: params["industry"] || "",
      company_size: params["company_size"] || ""
    }

    Logger.info("Updating business #{business_id} with: #{inspect(update_map)}")

    case Businesses.update_business(business_id, update_map) do
      :ok ->
        case Businesses.get_business_by_id(business_id) do
          {:ok, updated_business} ->
            {:noreply,
             socket
             |> assign(:business, updated_business)
             |> assign(:editing, false)
             |> put_flash(:info, "Business updated successfully!")}

          {:error, reason} ->
            Logger.error("Failed to fetch updated business: #{inspect(reason)}")

            {:noreply,
             socket
             |> assign(:editing, false)
             |> put_flash(:info, "Business updated successfully!")}
        end

      {:error, reason} ->
        Logger.error("Failed to update business: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to update business")}
    end
  end

  @impl true
  def handle_event("delete_business", _params, socket) do
    business_id = socket.assigns.business_id

    Logger.info("Deleting business: #{business_id}")

    case Businesses.delete_business(business_id) do
      :ok ->
        {:noreply,
         socket
         |> put_flash(:info, "Business account deleted successfully")
         |> push_navigate(to: ~p"/en/business")}

      {:error, reason} ->
        Logger.error("Failed to delete business: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to delete business account")}
    end
  end

  @impl true
  def handle_event("show_delete_modal", _params, socket) do
    {:noreply, assign(socket, :show_delete_modal, true)}
  end

  @impl true
  def handle_event("hide_delete_modal", _params, socket) do
    {:noreply, assign(socket, :show_delete_modal, false)}
  end

  defp normalize_id(id) when is_binary(id), do: id

  defp normalize_id(id) when is_list(id) do
    try do
      List.to_string(id)
    rescue
      _ ->
        Logger.error("Failed to convert charlist to string: #{inspect(id)}")
        ""
    end
  end

  defp normalize_id(id) do
    Logger.error("Unexpected id type: #{inspect(id)}")
    ""
  end
end
