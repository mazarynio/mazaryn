defmodule MazarynWeb.HomeLive.WeatherDebug do
  alias MazarynWeb.HomeLive.WeatherHelper
  require Logger

  def run_complete_test do
    Logger.info("🧪 Starting complete weather API test...")

    IO.puts("\n=== Configuration Test ===")
    config = WeatherHelper.debug_config()
    IO.inspect(config, label: "Config")

    IO.puts("\n=== API Connectivity Test ===")
    case WeatherHelper.test_api_key() do
      {:ok, message} ->
        IO.puts("✅ #{message}")
        test_real_weather_request()
      {:fallback, message} ->
        IO.puts("⚠️  #{message}")
        test_fallback_weather_request()
      {:error, reason} ->
        IO.puts("❌ API test failed: #{reason}")
        test_fallback_weather_request()
    end
  end

  defp test_real_weather_request do
    IO.puts("\n=== Real Weather Request Test (OpenWeatherMap) ===")
    test_cities = ["London", "Perth", "New York", "Tokyo"]

    Enum.each(test_cities, fn city ->
      case WeatherHelper.fetch_complete_weather(city) do
        {:ok, weather} ->
          IO.puts("✅ #{city}: #{weather.temperature}°C, #{weather.description}")
        {:error, reason} ->
          IO.puts("❌ #{city}: #{reason}")
      end
    end)
  end

  defp test_fallback_weather_request do
    IO.puts("\n=== Fallback Weather Request Test (Open-Meteo) ===")
    test_cities = ["London", "Perth", "New York", "Tokyo"]

    Enum.each(test_cities, fn city ->
      case WeatherHelper.fetch_complete_weather(city) do
        {:ok, weather} ->
          IO.puts("✅ #{city}: #{weather.temperature}°C, #{weather.description} (#{weather.api_source})")
        {:error, reason} ->
          IO.puts("❌ #{city}: #{reason}")
      end
    end)
  end

  def test_single_city(city \\ "London") do
    IO.puts("\n=== Single City Test: #{city} ===")

    case WeatherHelper.fetch_complete_weather(city) do
      {:ok, weather} ->
        IO.puts("✅ Success!")
        IO.puts("City: #{weather.city}")
        IO.puts("Temperature: #{weather.temperature}°C (feels like #{weather.feels_like}°C)")
        IO.puts("Condition: #{weather.main} - #{weather.description}")
        IO.puts("Humidity: #{weather.humidity}%")
        IO.puts("Wind: #{weather.wind_speed} m/s")
        IO.puts("API Source: #{Map.get(weather, :api_source, "Unknown")}")

        if forecast = weather.forecast do
          IO.puts("\n5-Day Forecast:")
          Enum.each(forecast, fn day ->
            IO.puts("  #{day.date}: #{day.temp_max}°/#{day.temp_min}° - #{day.description}")
          end)
        end
      {:error, reason} ->
        IO.puts("❌ Failed: #{reason}")
    end
  end

  def test_api_key_format do
    api_key = Application.get_env(:mazaryn, :weather_api)[:api_key]

    IO.puts("\n=== API Key Format Test ===")
    IO.puts("API Key present: #{not is_nil(api_key)}")
    IO.puts("API Key length: #{if api_key, do: byte_size(api_key), else: 0}")
    IO.puts("API Key (masked): #{if api_key, do: mask_api_key(api_key), else: "None"}")

    if api_key && byte_size(api_key) > 10 do
      test_url = "https://api.openweathermap.org/data/2.5/weather?q=London&appid=#{api_key}"

      IO.puts("\nTesting direct API call...")
      case HTTPoison.get(test_url, [], timeout: 5000) do
        {:ok, %HTTPoison.Response{status_code: 200}} ->
          IO.puts("✅ API key works!")
        {:ok, %HTTPoison.Response{status_code: 401, body: body}} ->
          IO.puts("❌ API key rejected: #{body}")
        {:ok, %HTTPoison.Response{status_code: status, body: body}} ->
          IO.puts("❌ HTTP #{status}: #{body}")
        {:error, %HTTPoison.Error{reason: reason}} ->
          IO.puts("❌ Connection error: #{inspect(reason)}")
      end
    else
      IO.puts("❌ API key missing or invalid format")
    end
  end

  defp mask_api_key(key) when byte_size(key) > 8 do
    first_part = String.slice(key, 0, 4)
    last_part = String.slice(key, -4, 4)
    middle_length = byte_size(key) - 8
    middle_mask = String.duplicate("*", middle_length)
    first_part <> middle_mask <> last_part
  end
  defp mask_api_key(key), do: String.duplicate("*", byte_size(key))
end
