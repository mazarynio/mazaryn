defmodule MazarynWeb.BusinessLive.Analytics do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Businesses

  @impl true
  def mount(%{"id" => business_id}, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("Mounting BusinessLive.Analytics for business_id: #{business_id}")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        case Businesses.get_business_by_id(business_id) do
          {:ok, business} ->
            current_user_id = normalize_id(current_user.id)
            business_user_id = normalize_id(business.user_id)

            if current_user_id == business_user_id do
              analytics = Businesses.get_analytics(business_id)

              {:ok,
               socket
               |> assign(:current_user, current_user)
               |> assign(:business, business)
               |> assign(:business_id, business_id)
               |> assign(:analytics, analytics)
               |> assign(:time_range, "7d")
               |> assign(:page_title, "Analytics - #{business.company_name}")}
            else
              {:ok,
               socket
               |> put_flash(:error, "You don't have permission to view this business analytics")
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
  def handle_event("change_time_range", %{"range" => range}, socket) do
    {:noreply, assign(socket, :time_range, range)}
  end

  @impl true
  def handle_event("record_page_view", _params, socket) do
    business_id = socket.assigns.business_id

    visitor_data = %{
      timestamp: DateTime.utc_now(),
      user_id: socket.assigns.current_user.id,
      ip: get_connect_params(socket)["peer_data"]["address"] || "unknown"
    }

    case Businesses.record_page_view(business_id, visitor_data) do
      :ok ->
        analytics = Businesses.get_analytics(business_id)
        {:noreply, assign(socket, :analytics, analytics)}

      {:error, reason} ->
        Logger.error("Failed to record page view: #{inspect(reason)}")
        {:noreply, socket}
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

  defp format_timestamp(nil), do: "Unknown"
  defp format_timestamp(timestamp) when is_binary(timestamp), do: timestamp

  defp format_timestamp(%DateTime{} = timestamp) do
    Calendar.strftime(timestamp, "%b %d, %Y %I:%M %p")
  end

  defp format_timestamp(_), do: "Unknown"
end
