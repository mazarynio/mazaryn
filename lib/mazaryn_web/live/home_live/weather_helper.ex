defmodule MazarynWeb.HomeLive.WeatherHelper do
  require Logger

  @openweather_api_key Application.compile_env(:mazaryn, [:weather_api, :api_key])
  @openweather_base_url "https://api.openweathermap.org/data/2.5"
  @openweather_geocoding_url "https://api.openweathermap.org/geo/1.0"

  @openmeteo_base_url "https://api.open-meteo.com/v1"
  @nominatim_base_url "https://nominatim.openstreetmap.org"

  def test_api_key do
    Logger.info("Testing OpenWeatherMap API key...")

    case test_openweather_api() do
      {:ok, _} ->
        Logger.info("✅ OpenWeatherMap API key is working!")
        {:ok, "OpenWeatherMap API working"}
      {:error, reason} ->
        Logger.warning("❌ OpenWeatherMap API failed: #{reason}")
        Logger.info("🔄 Falling back to Open-Meteo (free, no key required)")
        {:fallback, "Using Open-Meteo as fallback"}
    end
  end

  def fetch_complete_weather(city) do
    Logger.info("🌤️ Fetching complete weather data for city: #{city}")

    case test_api_key() do
      {:ok, _} ->
        fetch_openweather_complete(city)
      {:fallback, _} ->
        fetch_openmeteo_complete(city)
      {:error, reason} ->
        {:error, reason}
    end
  end

  def fetch_current_weather(city) do
    Logger.info("🌤️ Fetching current weather for city: #{city}")

    case test_api_key() do
      {:ok, _} ->
        fetch_openweather_current(city)
      {:fallback, _} ->
        fetch_openmeteo_current(city)
      {:error, reason} ->
        {:error, reason}
    end
  end

  defp test_openweather_api do
    if is_valid_api_key?(@openweather_api_key) do
      url = "#{@openweather_base_url}/weather?q=London&appid=#{@openweather_api_key}&units=metric"

      case HTTPoison.get(url, [], timeout: 5_000, recv_timeout: 5_000) do
        {:ok, %HTTPoison.Response{status_code: 200}} ->
          {:ok, "API key valid"}
        {:ok, %HTTPoison.Response{status_code: 401}} ->
          {:error, "API key invalid or not activated yet"}
        {:ok, %HTTPoison.Response{status_code: status}} ->
          {:error, "API error: #{status}"}
        {:error, %HTTPoison.Error{reason: reason}} ->
          {:error, "Connection error: #{inspect(reason)}"}
      end
    else
      {:error, "No valid API key configured"}
    end
  end

  defp fetch_openweather_complete(city) do
    with {:ok, coords} <- get_openweather_coordinates(city),
         {:ok, current} <- fetch_openweather_weather(coords),
         {:ok, forecast} <- fetch_openweather_forecast(coords),
         {:ok, uv_data} <- fetch_openweather_uv(coords) do
      weather_data = current
        |> Map.put(:forecast, forecast)
        |> Map.put(:uv_index, uv_data.uv_index)
      {:ok, weather_data}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp fetch_openweather_current(city) do
    with {:ok, coords} <- get_openweather_coordinates(city),
         {:ok, current} <- fetch_openweather_weather(coords),
         {:ok, uv_data} <- fetch_openweather_uv(coords) do
      weather_data = Map.put(current, :uv_index, uv_data.uv_index)
      {:ok, weather_data}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp get_openweather_coordinates(city) do
    url = "#{@openweather_geocoding_url}/direct?q=#{URI.encode(city)}&limit=1&appid=#{@openweather_api_key}"

    case HTTPoison.get(url, [], timeout: 10_000, recv_timeout: 10_000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, [location | _]} ->
            {:ok, %{
              lat: location["lat"],
              lon: location["lon"],
              name: location["name"],
              country: location["country"]
            }}
          {:ok, []} ->
            {:error, "City not found"}
          {:error, _} ->
            {:error, "Failed to parse location data"}
        end
      {:ok, %HTTPoison.Response{status_code: 401}} ->
        {:error, "API key invalid"}
      {:error, _} ->
        {:error, "Failed to get coordinates"}
    end
  end

  defp fetch_openweather_weather(coords) do
    url = "#{@openweather_base_url}/weather?lat=#{coords.lat}&lon=#{coords.lon}&appid=#{@openweather_api_key}&units=metric"

    case HTTPoison.get(url, [], timeout: 10_000, recv_timeout: 10_000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, data} -> {:ok, parse_openweather_current(data, coords)}
          {:error, _} -> {:error, "Failed to parse weather data"}
        end
      {:error, _} ->
        {:error, "Failed to fetch weather data"}
    end
  end

  defp fetch_openweather_forecast(coords) do
    url = "#{@openweather_base_url}/forecast?lat=#{coords.lat}&lon=#{coords.lon}&appid=#{@openweather_api_key}&units=metric"

    case HTTPoison.get(url, [], timeout: 10_000, recv_timeout: 10_000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, data} -> {:ok, parse_openweather_forecast(data)}
          {:error, _} -> {:error, "Failed to parse forecast data"}
        end
      {:error, _} ->
        {:error, "Failed to fetch forecast data"}
    end
  end

  defp fetch_openweather_uv(coords) do
    url = "#{@openweather_base_url}/uvi?lat=#{coords.lat}&lon=#{coords.lon}&appid=#{@openweather_api_key}"

    case HTTPoison.get(url, [], timeout: 10_000, recv_timeout: 10_000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, data} ->
            uv_index = data["value"] || 0
            {:ok, %{uv_index: round(uv_index * 10) / 10}}
          {:error, _} ->
            {:ok, %{uv_index: "N/A"}}
        end
      {:error, _} ->
        {:ok, %{uv_index: "N/A"}}
    end
  end

  defp fetch_openmeteo_complete(city) do
    with {:ok, coords} <- get_nominatim_coordinates(city),
         {:ok, current} <- fetch_openmeteo_weather(coords),
         {:ok, forecast} <- fetch_openmeteo_forecast(coords) do
      weather_data = Map.put(current, :forecast, forecast)
      {:ok, weather_data}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp fetch_openmeteo_current(city) do
    with {:ok, coords} <- get_nominatim_coordinates(city),
         {:ok, current} <- fetch_openmeteo_weather(coords) do
      {:ok, current}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp get_nominatim_coordinates(city) do
    url = "#{@nominatim_base_url}/search?q=#{URI.encode(city)}&format=json&limit=1"
    headers = [{"User-Agent", "Mazaryn Weather App"}]

    case HTTPoison.get(url, headers, timeout: 10_000, recv_timeout: 10_000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, [location | _]} ->
            {:ok, %{
              lat: String.to_float(location["lat"]),
              lon: String.to_float(location["lon"]),
              name: location["display_name"] |> String.split(",") |> List.first(),
              country: "Unknown"
            }}
          {:ok, []} ->
            {:error, "City not found"}
          {:error, _} ->
            {:error, "Failed to parse location data"}
        end
      {:error, _} ->
        {:error, "Failed to get coordinates"}
    end
  end

  defp fetch_openmeteo_weather(coords) do
    url = "#{@openmeteo_base_url}/forecast?latitude=#{coords.lat}&longitude=#{coords.lon}&current=temperature_2m,relative_humidity_2m,apparent_temperature,precipitation,weather_code,surface_pressure,wind_speed_10m,wind_direction_10m,uv_index&timezone=auto"

    case HTTPoison.get(url, [], timeout: 10_000, recv_timeout: 10_000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, data} -> {:ok, parse_openmeteo_current(data, coords)}
          {:error, _} -> {:error, "Failed to parse weather data"}
        end
      {:error, _} ->
        {:error, "Failed to fetch weather data"}
    end
  end

  defp fetch_openmeteo_forecast(coords) do
    url = "#{@openmeteo_base_url}/forecast?latitude=#{coords.lat}&longitude=#{coords.lon}&daily=weather_code,temperature_2m_max,temperature_2m_min,precipitation_sum,wind_speed_10m_max,uv_index_max&timezone=auto&forecast_days=5"

    case HTTPoison.get(url, [], timeout: 10_000, recv_timeout: 10_000) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, data} -> {:ok, parse_openmeteo_forecast(data)}
          {:error, _} -> {:error, "Failed to parse forecast data"}
        end
      {:error, _} ->
        {:error, "Failed to fetch forecast data"}
    end
  end

  defp parse_openweather_current(data, coords) do
    main = data["main"] || %{}
    weather = List.first(data["weather"]) || %{}
    wind = data["wind"] || %{}
    sys = data["sys"] || %{}
    clouds = data["clouds"] || %{}

    %{
      city: coords.name,
      country: coords.country,
      temperature: round(main["temp"] || 0),
      feels_like: round(main["feels_like"] || 0),
      temp_min: round(main["temp_min"] || 0),
      temp_max: round(main["temp_max"] || 0),
      humidity: main["humidity"] || 0,
      pressure: main["pressure"] || 0,
      visibility: round((data["visibility"] || 0) / 1000),
      cloudiness: clouds["all"] || 0,
      main: weather["main"] || "Clear",
      description: weather["description"] || "clear sky",
      icon: weather["icon"] || "01d",
      wind_speed: format_wind_speed(wind["speed"]),
      wind_direction: wind["deg"] || 0,
      wind_gust: format_wind_speed(wind["gust"]),
      sunrise: format_time(sys["sunrise"]),
      sunset: format_time(sys["sunset"]),
      updated_at: format_current_time(),
      api_source: "OpenWeatherMap",
      uv_index: "N/A"
    }
  end

  defp parse_openmeteo_current(data, coords) do
    current = data["current"] || %{}

    weather_code = current["weather_code"] || 0
    {main, description, icon} = get_weather_from_code(weather_code)

    %{
      city: coords.name,
      country: coords.country,
      temperature: round(current["temperature_2m"] || 0),
      feels_like: round(current["apparent_temperature"] || 0),
      temp_min: round(current["temperature_2m"] || 0),
      temp_max: round(current["temperature_2m"] || 0),
      humidity: round(current["relative_humidity_2m"] || 0),
      pressure: round(current["surface_pressure"] || 0),
      visibility: 10,
      cloudiness: 0,
      main: main,
      description: description,
      icon: icon,
      wind_speed: format_wind_speed(current["wind_speed_10m"]),
      wind_direction: current["wind_direction_10m"] || 0,
      wind_gust: "0.0",
      sunrise: "06:00",
      sunset: "18:00",
      updated_at: format_current_time(),
      api_source: "Open-Meteo",
      uv_index: current["uv_index"] || "N/A"
    }
  end

  defp parse_openweather_forecast(data) do
    list = data["list"] || []

    list
    |> Enum.group_by(fn item ->
      timestamp = item["dt"]
      DateTime.from_unix!(timestamp) |> DateTime.to_date()
    end)
    |> Enum.to_list()
    |> Enum.sort_by(fn {date, _} -> date end)
    |> Enum.take(5)
    |> Enum.map(fn {date, day_items} ->
      forecast_item = Enum.find(day_items, fn item ->
        timestamp = item["dt"]
        time = DateTime.from_unix!(timestamp) |> DateTime.to_time()
        time.hour >= 11 && time.hour <= 15
      end) || List.first(day_items)

      weather = List.first(forecast_item["weather"]) || %{}
      main = forecast_item["main"] || %{}

      %{
        date: format_forecast_date(date),
        temp_max: round(main["temp_max"] || 0),
        temp_min: round(main["temp_min"] || 0),
        main: weather["main"] || "Clear",
        description: weather["description"] || "clear sky",
        icon: weather["icon"] || "01d",
        humidity: main["humidity"] || 0
      }
    end)
  end

  defp parse_openmeteo_forecast(data) do
    daily = data["daily"] || %{}
    dates = daily["time"] || []
    weather_codes = daily["weather_code"] || []
    temp_max = daily["temperature_2m_max"] || []
    temp_min = daily["temperature_2m_min"] || []

    dates
    |> Enum.with_index()
    |> Enum.take(5)
    |> Enum.map(fn {date_str, index} ->
      weather_code = Enum.at(weather_codes, index, 0)
      {main, description, icon} = get_weather_from_code(weather_code)

      {:ok, date} = Date.from_iso8601(date_str)

      %{
        date: format_forecast_date(date),
        temp_max: round(Enum.at(temp_max, index, 0)),
        temp_min: round(Enum.at(temp_min, index, 0)),
        main: main,
        description: description,
        icon: icon,
        humidity: 50
      }
    end)
  end

  defp get_weather_from_code(code) do
    case code do
      0 -> {"Clear", "clear sky", "01d"}
      1 -> {"Clear", "mainly clear", "01d"}
      2 -> {"Clouds", "partly cloudy", "02d"}
      3 -> {"Clouds", "overcast", "04d"}
      45 -> {"Fog", "fog", "50d"}
      48 -> {"Fog", "depositing rime fog", "50d"}
      51 -> {"Drizzle", "light drizzle", "09d"}
      53 -> {"Drizzle", "moderate drizzle", "09d"}
      55 -> {"Drizzle", "dense drizzle", "09d"}
      61 -> {"Rain", "slight rain", "10d"}
      63 -> {"Rain", "moderate rain", "10d"}
      65 -> {"Rain", "heavy rain", "10d"}
      71 -> {"Snow", "slight snow", "13d"}
      73 -> {"Snow", "moderate snow", "13d"}
      75 -> {"Snow", "heavy snow", "13d"}
      80 -> {"Rain", "slight rain showers", "10d"}
      81 -> {"Rain", "moderate rain showers", "10d"}
      82 -> {"Rain", "violent rain showers", "10d"}
      95 -> {"Thunderstorm", "thunderstorm", "11d"}
      96 -> {"Thunderstorm", "thunderstorm with slight hail", "11d"}
      99 -> {"Thunderstorm", "thunderstorm with heavy hail", "11d"}
      _ -> {"Clear", "clear sky", "01d"}
    end
  end

  defp is_valid_api_key?(nil), do: false
  defp is_valid_api_key?(""), do: false
  defp is_valid_api_key?("demo_key"), do: false
  defp is_valid_api_key?("your_api_key_here"), do: false
  defp is_valid_api_key?(key) when is_binary(key) and byte_size(key) > 10, do: true
  defp is_valid_api_key?(_), do: false

  defp format_wind_speed(nil), do: "0.0"
  defp format_wind_speed(speed) when is_number(speed) do
    speed |> Float.round(1) |> to_string()
  end
  defp format_wind_speed(_), do: "0.0"

  defp format_time(nil), do: "N/A"
  defp format_time(timestamp) when is_integer(timestamp) do
    DateTime.from_unix!(timestamp)
    |> DateTime.to_time()
    |> Time.to_string()
    |> String.slice(0, 5)
  rescue
    _ -> "N/A"
  end
  defp format_time(_), do: "N/A"

  defp format_current_time do
    DateTime.utc_now()
    |> DateTime.to_string()
    |> String.slice(0, 19)
    |> String.replace("T", " ")
  end

  defp format_forecast_date(%Date{} = date) do
    today = Date.utc_today()

    case Date.compare(date, today) do
      :eq -> "Today"
      :gt ->
        case Date.diff(date, today) do
          1 -> "Tomorrow"
          _ -> Calendar.strftime(date, "%a")
        end
      _ -> Calendar.strftime(date, "%a")
    end
  end

  def get_weather_advice(weather_data) do
    temp = weather_data.temperature
    description = String.downcase(weather_data.description || "")
    wind_speed = case Float.parse(weather_data.wind_speed || "0") do
      {speed, _} -> speed
      :error -> 0
    end

    advice = cond do
      temp >= 35 ->
        "Extremely hot! Stay hydrated, seek shade, and avoid prolonged sun exposure."
      temp >= 30 ->
        "It's quite hot! Stay hydrated and wear light, breathable clothes."
      temp <= 0 ->
        "Freezing temperatures! Dress in layers and watch for ice."
      temp <= 5 ->
        "Very cold! Bundle up and wear warm layers."
      String.contains?(description, "thunderstorm") ->
        "Thunderstorms expected! Stay indoors and avoid outdoor activities."
      String.contains?(description, "snow") ->
        "Snow expected! Drive carefully, dress warmly, and allow extra travel time."
      String.contains?(description, "rain") ->
        "Rain expected! Don't forget your umbrella and drive safely."
      wind_speed >= 15 ->
        "Very windy conditions! Secure loose objects and be cautious outdoors."
      wind_speed >= 10 ->
        "It's quite windy today. Hold onto your hat!"
      String.contains?(description, "clear") && temp >= 20 && temp <= 28 ->
        "Perfect weather! Great day for outdoor activities."
      String.contains?(description, "clear") ->
        "Beautiful clear skies! Enjoy the sunshine."
      true ->
        "Have a wonderful day!"
    end

    source_info = case Map.get(weather_data, :api_source) do
      "Open-Meteo" -> " (Data from Open-Meteo)"
      _ -> ""
    end

    advice <> source_info
  rescue
    _ -> "Have a wonderful day!"
  end

  def debug_config do
    %{
      api_key_configured: is_valid_api_key?(@openweather_api_key),
      api_key_length: if(@openweather_api_key, do: byte_size(@openweather_api_key), else: 0),
      openweather_url: @openweather_base_url,
      openmeteo_url: @openmeteo_base_url
    }
  end
end
