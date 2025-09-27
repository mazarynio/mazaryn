defmodule MazarynWeb.HomeLive.WeatherComponent do
  use MazarynWeb, :live_component
  alias Phoenix.LiveView.JS
  require Logger

  def mount(socket) do
    {:ok, assign(socket, weather_data: nil, loading: false, error: nil)}
  end

  def update(assigns, socket) do
    {:ok, assign(socket, assigns)}
  end

  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("search_weather", %{"city" => city}, socket) do
    if String.trim(city) != "" do
      send(self(), {:fetch_weather, city})
      {:noreply, assign(socket, loading: true, error: nil)}
    else
      {:noreply, assign(socket, error: "Please enter a city name")}
    end
  end

  def handle_event("close_weather", _params, socket) do
    send(self(), :close_weather_modal)
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div
      class="fixed inset-0 bg-black/50 backdrop-blur-sm z-50 flex items-center justify-center p-4"
      phx-click="close_weather"
      phx-target={@myself}
    >
      <div
        class="bg-white rounded-3xl shadow-2xl max-w-4xl w-full max-h-[90vh] overflow-y-auto"
        phx-click="stop_propagation"
      >
        <div class="relative bg-gradient-to-r from-blue-500 via-purple-500 to-indigo-600 rounded-t-3xl p-6">
          <div class="absolute inset-0 bg-gradient-to-r from-blue-600/20 to-purple-600/20 rounded-t-3xl"></div>
          <div class="relative flex items-center justify-between">
            <div class="flex items-center space-x-3">
              <div class="w-12 h-12 bg-white/20 backdrop-blur-sm rounded-2xl flex items-center justify-center">
                <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 15a4 4 0 004 4h9a5 5 0 10-.1-9.999 5.002 5.002 0 10-9.78 2.096A4.002 4.002 0 003 15z"></path>
                </svg>
              </div>
              <div>
                <h2 class="text-2xl font-bold text-white">Weather Forecast</h2>
                <p class="text-white/80">Get accurate weather information worldwide</p>
              </div>
            </div>
            <button
              phx-click="close_weather"
              phx-target={@myself}
              class="w-10 h-10 bg-white/20 hover:bg-white/30 rounded-xl flex items-center justify-center transition-colors"
            >
              <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path>
              </svg>
            </button>
          </div>
        </div>

        <div class="p-6 border-b border-gray-100">
          <form phx-submit="search_weather" phx-target={@myself} class="flex space-x-4">
            <div class="flex-1 relative">
              <input
                type="text"
                name="city"
                placeholder="Enter city name (e.g., New York, London, Tokyo)"
                class="w-full h-14 pl-12 pr-4 bg-gray-50 border-2 border-gray-200 rounded-2xl focus:border-blue-500 focus:bg-white transition-all duration-200 text-gray-900 placeholder-gray-500"
                required
              />
              <svg class="absolute left-4 top-4 w-6 h-6 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path>
              </svg>
            </div>
            <button
              type="submit"
              disabled={@loading}
              class="h-14 px-8 bg-gradient-to-r from-blue-500 to-purple-600 hover:from-blue-600 hover:to-purple-700 disabled:opacity-50 disabled:cursor-not-allowed text-white font-semibold rounded-2xl transition-all duration-200 shadow-lg hover:shadow-xl"
            >
              <%= if @loading do %>
                <div class="flex items-center space-x-2">
                  <svg class="animate-spin w-5 h-5" fill="none" viewBox="0 0 24 24">
                    <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                    <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                  </svg>
                  <span>Searching...</span>
                </div>
              <% else %>
                <div class="flex items-center space-x-2">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"></path>
                  </svg>
                  <span>Search</span>
                </div>
              <% end %>
            </button>
          </form>

          <%= if @error do %>
            <div class="mt-4 p-4 bg-red-50 border border-red-200 rounded-xl">
              <div class="flex items-center space-x-2">
                <svg class="w-5 h-5 text-red-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>
                </svg>
                <p class="text-red-700 font-medium"><%= @error %></p>
              </div>
            </div>
          <% end %>
        </div>

        <div class="p-6">
          <%= if @weather_data do %>
            <div class="space-y-8">
              <div class="bg-gradient-to-br from-blue-50 via-indigo-50 to-purple-50 rounded-2xl p-8 border border-blue-100">
                <div class="flex items-center justify-between mb-6">
                  <div>
                    <h3 class="text-3xl font-bold text-gray-900"><%= @weather_data.city %></h3>
                    <p class="text-gray-600"><%= @weather_data.country %></p>
                    <p class="text-sm text-gray-500 mt-1">Updated: <%= @weather_data.updated_at %></p>
                  </div>
                  <div class="text-right">
                    <div class="text-6xl font-bold text-gray-900"><%= @weather_data.temperature %>°</div>
                    <div class="text-gray-600 font-medium">Feels like <%= @weather_data.feels_like %>°</div>
                  </div>
                </div>

                <div class="flex items-center justify-between">
                  <div class="flex items-center space-x-4">
                    <div class="w-20 h-20 relative">
                      <img
                        src={"https://openweathermap.org/img/wn/#{@weather_data.icon}@4x.png"}
                        alt={@weather_data.description}
                        class="w-full h-full object-contain"
                      />
                    </div>
                    <div>
                      <p class="text-2xl font-semibold text-gray-900 capitalize"><%= @weather_data.main %></p>
                      <p class="text-gray-600 capitalize"><%= @weather_data.description %></p>
                    </div>
                  </div>

                  <div class="grid grid-cols-2 gap-4 text-center">
                    <div class="bg-white/60 backdrop-blur-sm rounded-xl p-3 border border-white/30">
                      <div class="text-2xl font-bold text-blue-600"><%= @weather_data.temp_max %>°</div>
                      <div class="text-xs text-gray-600 font-medium">High</div>
                    </div>
                    <div class="bg-white/60 backdrop-blur-sm rounded-xl p-3 border border-white/30">
                      <div class="text-2xl font-bold text-indigo-600"><%= @weather_data.temp_min %>°</div>
                      <div class="text-xs text-gray-600 font-medium">Low</div>
                    </div>
                  </div>
                </div>
              </div>

              <div class="grid grid-cols-2 md:grid-cols-4 gap-4">
                <div class="bg-white rounded-xl p-4 border border-gray-100 shadow-sm hover:shadow-md transition-shadow">
                  <div class="flex items-center space-x-3">
                    <div class="w-10 h-10 bg-blue-100 rounded-lg flex items-center justify-center">
                      <svg class="w-5 h-5 text-blue-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 15a4 4 0 004 4h9a5 5 0 10-.1-9.999 5.002 5.002 0 10-9.78 2.096A4.002 4.002 0 003 15z"></path>
                      </svg>
                    </div>
                    <div>
                      <p class="text-2xl font-bold text-gray-900"><%= @weather_data.humidity %>%</p>
                      <p class="text-sm text-gray-600">Humidity</p>
                    </div>
                  </div>
                </div>

                <div class="bg-white rounded-xl p-4 border border-gray-100 shadow-sm hover:shadow-md transition-shadow">
                  <div class="flex items-center space-x-3">
                    <div class="w-10 h-10 bg-green-100 rounded-lg flex items-center justify-center">
                      <svg class="w-5 h-5 text-green-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 4V2a1 1 0 011-1h8a1 1 0 011 1v2m0 0V3a1 1 0 011 1v10a1 1 0 01-1 1H8a1 1 0 01-1-1V4z"></path>
                      </svg>
                    </div>
                    <div>
                      <p class="text-2xl font-bold text-gray-900"><%= @weather_data.pressure %></p>
                      <p class="text-sm text-gray-600">hPa</p>
                    </div>
                  </div>
                </div>

                <div class="bg-white rounded-xl p-4 border border-gray-100 shadow-sm hover:shadow-md transition-shadow">
                  <div class="flex items-center space-x-3">
                    <div class="w-10 h-10 bg-purple-100 rounded-lg flex items-center justify-center">
                      <svg class="w-5 h-5 text-purple-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945M8 3.935V5.5A2.5 2.5 0 0010.5 8h.5a2 2 0 012 2 2 2 0 104 0 2 2 0 012-2h1.064M15 20.488V18a2 2 0 012-2h3.064"></path>
                      </svg>
                    </div>
                    <div>
                      <p class="text-2xl font-bold text-gray-900"><%= @weather_data.wind_speed %></p>
                      <p class="text-sm text-gray-600">m/s</p>
                    </div>
                  </div>
                </div>

                <div class="bg-white rounded-xl p-4 border border-gray-100 shadow-sm hover:shadow-md transition-shadow">
                  <div class="flex items-center space-x-3">
                    <div class="w-10 h-10 bg-orange-100 rounded-lg flex items-center justify-center">
                      <svg class="w-5 h-5 text-orange-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"></path>
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"></path>
                      </svg>
                    </div>
                    <div>
                      <p class="text-2xl font-bold text-gray-900"><%= @weather_data.visibility %></p>
                      <p class="text-sm text-gray-600">km</p>
                    </div>
                  </div>
                </div>
              </div>

              <%= if @weather_data.forecast do %>
                <div class="bg-white rounded-2xl p-6 border border-gray-100 shadow-sm">
                  <h4 class="text-xl font-bold text-gray-900 mb-6">5-Day Forecast</h4>
                  <div class="grid grid-cols-1 md:grid-cols-5 gap-4">
                    <%= for day <- @weather_data.forecast do %>
                      <div class="bg-gray-50 hover:bg-gray-100 rounded-xl p-4 transition-colors text-center">
                        <p class="font-semibold text-gray-900 mb-2"><%= day.date %></p>
                        <div class="w-12 h-12 mx-auto mb-2">
                          <img
                            src={"https://openweathermap.org/img/wn/#{day.icon}@2x.png"}
                            alt={day.description}
                            class="w-full h-full object-contain"
                          />
                        </div>
                        <p class="text-sm text-gray-600 capitalize mb-2"><%= day.main %></p>
                        <div class="space-y-1">
                          <p class="text-lg font-bold text-gray-900"><%= day.temp_max %>°</p>
                          <p class="text-sm text-gray-600"><%= day.temp_min %>°</p>
                        </div>
                      </div>
                    <% end %>
                  </div>
                </div>
              <% end %>

              <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div class="bg-gradient-to-r from-yellow-50 to-orange-50 rounded-xl p-6 border border-yellow-100">
                  <div class="flex items-center space-x-3 mb-4">
                    <div class="w-10 h-10 bg-yellow-500 rounded-lg flex items-center justify-center">
                      <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z"></path>
                      </svg>
                    </div>
                    <h5 class="text-lg font-bold text-gray-900">Sun & UV Index</h5>
                  </div>
                  <div class="space-y-2">
                    <div class="flex justify-between">
                      <span class="text-gray-600">Sunrise</span>
                      <span class="font-semibold text-gray-900"><%= @weather_data.sunrise %></span>
                    </div>
                    <div class="flex justify-between">
                      <span class="text-gray-600">Sunset</span>
                      <span class="font-semibold text-gray-900"><%= @weather_data.sunset %></span>
                    </div>
                    <div class="flex justify-between">
                      <span class="text-gray-600">UV Index</span>
                      <span class="font-semibold text-orange-600"><%= @weather_data.uv_index || "N/A" %></span>
                    </div>
                  </div>
                </div>

                <div class="bg-gradient-to-r from-blue-50 to-indigo-50 rounded-xl p-6 border border-blue-100">
                  <div class="flex items-center space-x-3 mb-4">
                    <div class="w-10 h-10 bg-blue-500 rounded-lg flex items-center justify-center">
                      <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945"></path>
                      </svg>
                    </div>
                    <h5 class="text-lg font-bold text-gray-900">Wind Information</h5>
                  </div>
                  <div class="space-y-2">
                    <div class="flex justify-between">
                      <span class="text-gray-600">Speed</span>
                      <span class="font-semibold text-gray-900"><%= @weather_data.wind_speed %> m/s</span>
                    </div>
                    <div class="flex justify-between">
                      <span class="text-gray-600">Direction</span>
                      <span class="font-semibold text-gray-900"><%= @weather_data.wind_direction %>°</span>
                    </div>
                    <%= if @weather_data.wind_gust do %>
                      <div class="flex justify-between">
                        <span class="text-gray-600">Gusts</span>
                        <span class="font-semibold text-blue-600"><%= @weather_data.wind_gust %> m/s</span>
                      </div>
                    <% end %>
                  </div>
                </div>
              </div>
            </div>
          <% else %>
            <div class="text-center py-16">
              <div class="w-24 h-24 bg-gradient-to-r from-blue-500 to-purple-600 rounded-3xl mx-auto mb-6 flex items-center justify-center">
                <svg class="w-12 h-12 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 15a4 4 0 004 4h9a5 5 0 10-.1-9.999 5.002 5.002 0 10-9.78 2.096A4.002 4.002 0 003 15z"></path>
                </svg>
              </div>
              <h3 class="text-2xl font-bold text-gray-900 mb-4">Welcome to Weather Forecast</h3>
              <p class="text-gray-600 mb-8 max-w-md mx-auto">
                Search for any city to get detailed weather information including current conditions,
                5-day forecast, and comprehensive weather data.
              </p>
              <div class="grid grid-cols-2 md:grid-cols-4 gap-4 max-w-2xl mx-auto text-sm">
                <div class="bg-blue-50 rounded-lg p-4">
                  <svg class="w-6 h-6 text-blue-600 mx-auto mb-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z"></path>
                  </svg>
                  <p class="font-medium text-gray-900">Temperature</p>
                </div>
                <div class="bg-green-50 rounded-lg p-4">
                  <svg class="w-6 h-6 text-green-600 mx-auto mb-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 15a4 4 0 004 4h9a5 5 0 10-.1-9.999 5.002 5.002 0 10-9.78 2.096A4.002 4.002 0 003 15z"></path>
                  </svg>
                  <p class="font-medium text-gray-900">Humidity</p>
                </div>
                <div class="bg-purple-50 rounded-lg p-4">
                  <svg class="w-6 h-6 text-purple-600 mx-auto mb-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945"></path>
                  </svg>
                  <p class="font-medium text-gray-900">Wind Speed</p>
                </div>
                <div class="bg-orange-50 rounded-lg p-4">
                  <svg class="w-6 h-6 text-orange-600 mx-auto mb-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19V6l12-3v13M9 19c0 1.105-1.343 2-3 2s-3-.895-3-2 1.343-2 3-2 3 .895 3 2zm12-3c0 1.105-1.343 2-3 2s-3-.895-3-2 1.343-2 3-2 3 .895 3 2zM9 10l12-3"></path>
                  </svg>
                  <p class="font-medium text-gray-900">5-Day Forecast</p>
                </div>
              </div>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end
end
