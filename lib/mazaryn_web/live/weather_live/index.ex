defmodule MazarynWeb.WeatherLive.Index do
  use MazarynWeb, :live_view
  alias MazarynWeb.HomeLive.WeatherHelper
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user_result =
      case session do
        %{"user_id" => user_email} -> Account.Users.one_by_email(user_email)
        %{"session_uuid" => uuid} -> Account.Users.get_by_session_uuid(uuid)
        _ -> {:error, :no_session}
      end

    case user_result do
      {:ok, user} ->
        {:ok,
         socket
         |> assign(:user, user)
         |> assign(:weather_data, nil)
         |> assign(:weather_loading, false)
         |> assign(:weather_error, nil)
         |> assign(:search_query, "")}

      _ ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_params(_params, url, socket) do
    socket = assign(socket, current_path: URI.parse(url).path)
    {:noreply, socket}
  end

  @impl true
  def handle_event("search_weather", %{"city" => city}, socket) do
    if String.trim(city) != "" do
      send(self(), {:fetch_weather, city})
      {:noreply, assign(socket, weather_loading: true, weather_error: nil, search_query: city)}
    else
      {:noreply, assign(socket, weather_error: "Please enter a city name")}
    end
  end

  @impl true
  def handle_info({:fetch_weather, city}, socket) do
    Logger.info("🌤️ Fetching weather data for #{city}")

    case WeatherHelper.fetch_complete_weather(city) do
      {:ok, weather_data} ->
        Logger.info("🌤️ Successfully fetched weather data for #{city}")

        {:noreply,
         assign(socket, weather_data: weather_data, weather_loading: false, weather_error: nil)}

      {:error, error_message} ->
        Logger.error("❌ Failed to fetch weather data for #{city}: #{error_message}")

        {:noreply,
         assign(socket, weather_data: nil, weather_loading: false, weather_error: error_message)}
    end
  end

  def handle_info(msg, socket) do
    Logger.debug("Received unexpected message: #{inspect(msg)}")
    {:noreply, socket}
  end
end
