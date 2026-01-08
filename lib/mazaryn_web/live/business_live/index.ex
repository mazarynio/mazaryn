defmodule MazarynWeb.BusinessLive.Index do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Account.Businesses

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("Mounting BusinessLive.Index")

    case Users.get_by_session_uuid(session_uuid) do
      {:ok, current_user} ->
        Logger.info("Current user: #{current_user.id}")
        businesses = Businesses.get_businesses_by_user_id(current_user.id)
        Logger.info("Found #{length(businesses)} businesses for user")

        {:ok,
         socket
         |> assign(:current_user, current_user)
         |> assign(:session_uuid, session_uuid)
         |> assign(:businesses, businesses)
         |> assign(:show_create_form, false)
         |> assign(:page_title, "Business Account")}

      {:error, reason} ->
        Logger.error("Session not found: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session not found")
         |> push_redirect(to: ~p"/en/login")}
    end
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("show_create_form", _params, socket) do
    Logger.info("Showing create form")
    {:noreply, assign(socket, :show_create_form, true)}
  end

  @impl true
  def handle_event("hide_create_form", _params, socket) do
    Logger.info("Hiding create form")
    {:noreply, assign(socket, :show_create_form, false)}
  end

  @impl true
  def handle_event("create_business", %{"business" => business_params}, socket) do
    Logger.info("Creating business for user: #{socket.assigns.current_user.id}")
    Logger.info("Business params: #{inspect(business_params)}")

    case Businesses.create_business(socket.assigns.current_user.id, business_params) do
      {:ok, business_id} ->
        Logger.info("Business created successfully with ID: #{business_id}")
        businesses = Businesses.get_businesses_by_user_id(socket.assigns.current_user.id)

        {:noreply,
         socket
         |> assign(:businesses, businesses)
         |> assign(:show_create_form, false)
         |> put_flash(:info, "Business account created successfully!")
         |> push_navigate(to: ~p"/en/business/#{business_id}")}

      {:error, reason} ->
        Logger.error("Business creation failed: #{inspect(reason)}")

        {:noreply,
         socket
         |> put_flash(:error, "Failed to create business account: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("manage_business", %{"id" => business_id}, socket) do
    Logger.info("Managing business: #{business_id}")
    {:noreply, push_navigate(socket, to: ~p"/en/business/#{business_id}")}
  end

  @impl true
  def handle_event("validate_business", %{"business" => business_params}, socket) do
    Logger.debug("Validating business params: #{inspect(business_params)}")
    {:noreply, socket}
  end
end
