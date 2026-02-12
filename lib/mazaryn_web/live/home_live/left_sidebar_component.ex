defmodule MazarynWeb.HomeLive.LeftSidebarComponent do
  use MazarynWeb, :live_component
  use Phoenix.VerifiedRoutes, endpoint: MazarynWeb.Endpoint, router: MazarynWeb.Router

  @impl true
  def mount(socket) do
    {:ok, assign(socket, :time_of_day, get_time_of_day())}
  end

  defp get_time_of_day do
    hour = :calendar.local_time() |> elem(1) |> elem(0)

    cond do
      hour < 6 -> :night
      hour < 12 -> :morning
      hour < 18 -> :afternoon
      true -> :evening
    end
  end

  defp get_daily_quote do
    quotes = [
      "The only way to do great work is to love what you do. - Steve Jobs",
      "Innovation distinguishes between a leader and a follower. - Steve Jobs",
      "Your time is limited, don't waste it living someone else's life. - Steve Jobs",
      "Stay hungry, stay foolish. - Steve Jobs",
      "The future belongs to those who believe in the beauty of their dreams. - Eleanor Roosevelt",
      "The best way to predict the future is to create it. - Peter Drucker",
      "Success is not final, failure is not fatal: it is the courage to continue that counts. - Winston Churchill",
      "Don't watch the clock; do what it does. Keep going. - Sam Levenson",
      "The only limit to our realization of tomorrow will be our doubts of today. - Franklin D. Roosevelt",
      "It does not matter how slowly you go as long as you do not stop. - Confucius",
      "Quality is not an act, it is a habit. - Aristotle",
      "The mind is everything. What you think you become. - Buddha",
      "Life is 10% what happens to us and 90% how we react to it. - Charles R. Swindoll",
      "The way to get started is to quit talking and begin doing. - Walt Disney",
      "The journey of a thousand miles begins with one step. - Lao Tzu",
      "Believe you can and you're halfway there. - Theodore Roosevelt",
      "I have not failed. I've just found 10,000 ways that won't work. - Thomas Edison",
      "Everything you've ever wanted is on the other side of fear. - George Addair",
      "The harder I work, the more luck I seem to have. - Thomas Jefferson",
      "Don't be afraid to give up the good to go for the great. - John D. Rockefeller"
    ]

    today = Date.utc_today()
    day_of_year = today.day + today.month * 31
    Enum.at(quotes, rem(day_of_year, length(quotes)))
  end

  @impl true
  def update(assigns, socket) do
    time_of_day = assigns[:time_of_day] || socket.assigns[:time_of_day] || :afternoon

    colors =
      case time_of_day do
        :morning ->
          %{
            theme: "morning-theme",
            gradient: "bg-gradient-to-br from-amber-50 via-rose-100 to-blue-100",
            card_bg: "bg-gradient-to-br from-white/90 via-amber-50/90 to-rose-50/90",
            border:
              "border-2 border-white/30 border-gradient-to-r from-amber-200/50 via-rose-200/50 to-blue-200/50",
            accent: "bg-gradient-to-r from-amber-400 via-rose-400 to-blue-400",
            icon: "🌅",
            happy_icon: "✨"
          }

        :afternoon ->
          %{
            theme: "afternoon-theme",
            gradient: "bg-gradient-to-br from-cyan-50/80 via-fuchsia-100/80 to-pink-100/80",
            card_bg: "bg-gradient-to-br from-white/90 via-cyan-50/90 to-fuchsia-50/90",
            border:
              "border-2 border-white/30 border-gradient-to-r from-cyan-200/50 via-fuchsia-200/50 to-pink-200/50",
            accent: "bg-gradient-to-r from-cyan-400 via-fuchsia-400 to-pink-400",
            icon: "☀️",
            happy_icon: "🌟"
          }

        :evening ->
          %{
            theme: "evening-theme",
            gradient: "bg-gradient-to-br from-violet-50/80 via-purple-100/80 to-rose-100/80",
            card_bg: "bg-gradient-to-br from-white/90 via-violet-50/90 to-purple-50/90",
            border:
              "border-2 border-white/30 border-gradient-to-r from-violet-200/50 via-purple-200/50 to-rose-200/50",
            accent: "bg-gradient-to-r from-violet-400 via-purple-400 to-rose-400",
            icon: "🌇",
            happy_icon: "🌠"
          }

        :night ->
          %{
            theme: "night-theme",
            gradient: "bg-gradient-to-br from-indigo-900/30 via-purple-900/30 to-blue-900/30",
            card_bg: "bg-gradient-to-br from-gray-900/90 via-indigo-900/90 to-purple-900/90",
            border:
              "border-2 border-white/10 border-gradient-to-r from-indigo-500/20 via-purple-500/20 to-blue-500/20",
            accent: "bg-gradient-to-r from-indigo-500 via-purple-500 to-blue-500",
            icon: "🌙",
            happy_icon: "🌌"
          }
      end

    socket =
      socket
      |> assign(assigns)
      |> assign(:time_of_day, time_of_day)
      |> assign(:colors, colors)
      |> assign(:daily_quote, get_daily_quote())

    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    <div
      class="relative w-64 h-full overflow-hidden"
      id="revolutionary-sidebar"
      phx-hook="GlassSidebar"
    >
      <div class="absolute inset-0 glass-background">
        <div class="floating-blob blob-1"></div>
        <div class="floating-blob blob-2"></div>
        <div class="floating-blob blob-3"></div>
        <div class="floating-blob blob-4"></div>

        <div class="grid-lines"></div>

        <div class="particle-system"></div>
      </div>

      <div class="relative z-10 h-full flex flex-col p-4 space-y-4 overflow-y-auto scrollbar-hidden">

        <div class="quote-card group">
          <div class="quote-overlay"></div>
          <div class="relative z-10 p-4">
            <div class="flex items-center mb-3">
              <div class="quote-icon-container">
                <svg class="quote-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path d="M3 21C3 17.6863 5.68629 15 9 15H11.5M3 21V13C3 8.58172 6.58172 5 11 5H13C17.4183 5 21 8.58172 21 13V21"
                        stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M11.5 21C11.5 17.6863 14.1863 15 17.5 15H20M11.5 21V13C11.5 8.58172 15.0817 5 19.5 5H21.5"
                        stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M12.5 10C12.5 9.17157 13.1716 8.5 14 8.5H16C16.8284 8.5 17.5 9.17157 17.5 10C17.5 10.8284 16.8284 11.5 16 11.5H14C13.1716 11.5 12.5 10.8284 12.5 10Z"
                        stroke="currentColor" stroke-width="1.5"/>
                  <path d="M4.5 10C4.5 9.17157 5.17157 8.5 6 8.5H8C8.82843 8.5 9.5 9.17157 9.5 10C9.5 10.8284 8.82843 11.5 8 11.5H6C5.17157 11.5 4.5 10.8284 4.5 10Z"
                        stroke="currentColor" stroke-width="1.5"/>
                </svg>
              </div>
              <div class="ml-3">
                <h3 class="font-bold text-sm text-gray-900">
                  <span class="gradient-text">Daily Wisdom</span>
                </h3>
                <p class="text-gray-600 text-xs mt-0.5">
                  <%= Timex.format!(Timex.now(), "%A, %B %d", :strftime) %>
                </p>
              </div>
            </div>

            <div class="mt-3">
              <div class="quote-text">
                <%= @daily_quote %>
              </div>
              <div class="flex items-center justify-between mt-3">
                <div class="quote-author">
                  <svg class="w-4 h-4 text-gray-400" fill="currentColor" viewBox="0 0 20 20">
                    <path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clip-rule="evenodd"/>
                  </svg>
                  <span class="text-xs text-gray-600 ml-1">Inspiring</span>
                </div>
                <button class="refresh-quote-btn" title="New Quote">
                  <svg class="w-3.5 h-3.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" />
                  </svg>
                </button>
              </div>
            </div>
          </div>
        </div>

        <div class="navigation-grid">
          <div class="grid-header">
            <h4 class="text-sm font-bold text-gray-900 truncate">
              <span class="text-base">⚡</span> Navigation
            </h4>
          </div>

          <div class="grid-layout">
            <.link navigate={~p"/#{@locale}/home"} class="nav-grid-item group" title="Home">
              <div class="nav-icon-container">
                <svg class="nav-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path d="M3 9L12 2L21 9V20C21 20.5304 20.7893 21.0391 20.4142 21.4142C20.0391 21.7893 19.5304 22 19 22H5C4.46957 22 3.96086 21.7893 3.58579 21.4142C3.21071 21.0391 3 20.5304 3 20V9Z"
                        stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                  <path d="M9 22V12H15V22" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
              </div>
              <span class="nav-label">Home</span>
            </.link>

            <!-- Chat -->
            <.link navigate={~p"/chats"} class="nav-grid-item group" title="Chat">
              <div class="nav-icon-container">
                <svg class="nav-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path d="M21 11.5C21.0034 12.8199 20.6951 14.1219 20.1 15.3C19.3944 16.7118 18.3098 17.8992 16.9674 18.7293C15.6251 19.5594 14.0782 19.9994 12.5 20C11.1801 20.0034 9.87812 19.6951 8.7 19.1L3 21L4.9 15.3C4.30493 14.1219 3.99656 12.8199 4 11.5C4.00061 9.92179 4.44061 8.37488 5.27072 7.03258C6.10083 5.69028 7.28825 4.6056 8.7 3.90003C9.87812 3.30496 11.1801 2.99659 12.5 3.00003H13C15.0843 3.11502 17.053 3.99479 18.5291 5.47089C20.0052 6.94699 20.885 8.91568 21 11V11.5Z"
                        stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
              </div>
              <span class="nav-label">Chat</span>
              <div class="nav-indicator pulse"></div>
            </.link>

            <.link navigate={~p"/#{@locale}/groups"} class="nav-grid-item group" title="Groups">
              <div class="nav-icon-container">
                <svg class="nav-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path d="M17 21V19C17 17.9391 16.5786 16.9217 15.8284 16.1716C15.0783 15.4214 14.0609 15 13 15H5C3.93913 15 2.92172 15.4214 2.17157 16.1716C1.42143 16.9217 1 17.9391 1 19V21"
                        stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                  <path d="M9 11C11.2091 11 13 9.20914 13 7C13 4.79086 11.2091 3 9 3C6.79086 3 5 4.79086 5 7C5 9.20914 6.79086 11 9 11Z"
                        stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                  <path d="M23 21V19C22.9993 18.1137 22.7044 17.2528 22.1614 16.5523C21.6184 15.8519 20.8581 15.3516 20 15.13"
                        stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                  <path d="M16 3.13C16.8604 3.35031 17.623 3.85071 18.1676 4.55232C18.7122 5.25392 19.0078 6.11683 19.0078 7.005C19.0078 7.89318 18.7122 8.75608 18.1676 9.45769C17.623 10.1593 16.8604 10.6597 16 10.88"
                        stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
              </div>
              <span class="nav-label">Groups</span>
            </.link>

            <!-- Video -->
            <.link navigate={~p"/#{@locale}/videos"} class="nav-grid-item group" title="Video">
              <div class="nav-icon-container">
                <svg class="nav-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path d="M23 7L16 12L23 17V7Z" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                  <rect x="1" y="5" width="15" height="14" rx="2" stroke="currentColor" stroke-width="1.5"/>
                </svg>
              </div>
              <span class="nav-label">Video</span>
            </.link>

            <!-- Music -->
            <.link navigate={~p"/#{@locale}/music"} class="nav-grid-item group" title="Music">
              <div class="nav-icon-container">
                <svg class="nav-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path d="M9 18V5L21 3V16" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                  <circle cx="6" cy="18" r="3" stroke="currentColor" stroke-width="1.5"/>
                  <circle cx="18" cy="16" r="3" stroke="currentColor" stroke-width="1.5"/>
                </svg>
              </div>
              <span class="nav-label">Music</span>
            </.link>

            <!-- Weather -->
            <.link navigate={~p"/#{@locale}/weather"} class="nav-grid-item group" title="Weather">
              <div class="nav-icon-container">
                <svg class="nav-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path d="M12 2V4" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M12 20V22" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M4 12H2" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M22 12H20" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M19.071 19.071L17.657 17.657" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M6.343 6.343L4.929 4.929" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M19.071 4.929L17.657 6.343" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M6.343 17.657L4.929 19.071" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"/>
                  <path d="M8 12C8 9.79086 9.79086 8 12 8C14.2091 8 16 9.79086 16 12C16 14.2091 14.2091 16 12 16C9.79086 16 8 14.2091 8 12Z"
                        stroke="currentColor" stroke-width="1.5"/>
                </svg>
              </div>
              <span class="nav-label">Weather</span>
            </.link>

            <!-- AI Assistant -->
            <.link navigate={~p"/#{@locale}/ai"} class="nav-grid-item group featured" title="AI Assistant">
              <div class="nav-icon-container">
                <svg class="nav-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <path d="M12 2L2 7L12 12L22 7L12 2Z" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                  <path d="M2 17L12 22L22 17" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                  <path d="M2 12L12 17L22 12" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"/>
                </svg>
              </div>
              <span class="nav-label">AI</span>
              <div class="nav-badge featured">PRO</div>
            </.link>

            <!-- Jobs -->
            <.link navigate={~p"/#{@locale}/jobs"} class="nav-grid-item group" title="Jobs">
              <div class="nav-icon-container">
                <svg class="nav-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                  <rect x="2" y="7" width="20" height="14" rx="2" stroke="currentColor" stroke-width="1.5"/>
                  <path d="M16 21V5C16 3.89543 15.1046 3 14 3H10C8.89543 3 8 3.89543 8 5V7" stroke="currentColor" stroke-width="1.5"/>
                </svg>
              </div>
              <span class="nav-label">Jobs</span>
            </.link>
          </div>
        </div>

        <!-- Quick Actions Panel -->
        <div class="quick-actions">
          <div class="actions-header">
            <h4 class="text-xs font-semibold text-gray-900">Quick Actions</h4>
            <button class="action-btn" title="Add new">
              <svg class="w-3.5 h-3.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4" />
              </svg>
            </button>
          </div>
          <div class="actions-grid">
            <.link navigate={~p"/dashboard"} class="action-item" title="Dashboard">
              <div class="action-icon">📊</div>
              <span class="action-text">Dashboard</span>
            </.link>
            <.link navigate={~p"/#{@locale}/settings"} class="action-item" title="Settings">
              <div class="action-icon">⚙️</div>
              <span class="action-text">Settings</span>
            </.link>
            <.link navigate={~p"/notifications"} class="action-item" title="Notifications">
              <div class="action-icon">🔔</div>
              <span class="action-text">Alerts</span>
              <div class="notification-badge"></div>
            </.link>
            <.link navigate={~p"/help"} class="action-item" title="Help">
              <div class="action-icon">❓</div>
              <span class="action-text">Help</span>
            </.link>
          </div>
        </div>

        <!-- Admin Panel -->
        <%= if Enum.member?(ManageUser.get_admin_list(), String.to_charlist(@user.username)) do %>
          <div class="admin-panel">
            <div class="admin-header">
              <div class="admin-badge">ADMIN</div>
              <h4 class="text-xs font-semibold text-gray-900 truncate">Control Panel</h4>
            </div>
            <.link navigate={~p"/manage"} class="admin-action" title="Manage Users">
              <svg class="admin-icon" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
                <path d="M12 15C13.6569 15 15 13.6569 15 12C15 10.3431 13.6569 9 12 9C10.3431 9 9 10.3431 9 12C9 13.6569 10.3431 15 12 15Z"
                      stroke="currentColor" stroke-width="1.5"/>
                <path d="M19.4 15C19.2669 15.3031 19.1337 15.6062 19.0006 15.9094C18.5363 16.9373 18.3041 17.4512 17.8906 17.8171C17.4771 18.183 16.921 18.366 15.8087 18.732L15 19C13.5 19.5 12 20 10.5 19.5L9.19133 19.068C8.07898 18.702 7.5228 18.519 7.1093 18.1531C6.6958 17.7872 6.46361 17.2733 5.99922 16.2454C5.86441 15.9394 5.72959 15.6333 5.59478 15.3273C5.22222 14.4972 5.03594 14.0821 5.03723 13.6466C5.03851 13.211 5.22728 12.7971 5.60482 11.9692C5.74132 11.6662 5.87782 11.3632 6.01432 11.0602C6.47871 10.0323 6.7109 9.51837 7.1244 9.15248C7.5379 8.78658 8.09409 8.60361 9.20644 8.23766L10 8C11.5 7.5 13 7 14.5 7.5L15.8087 7.93202C16.921 8.29798 17.4772 8.48095 17.8907 8.84685C18.3042 9.21274 18.5364 9.72666 19.0008 10.7546C19.1373 11.0599 19.2738 11.3652 19.4103 11.6705C19.7822 12.4988 19.9681 12.9129 19.9668 13.3485C19.9655 13.7841 19.7771 14.1983 19.4 15Z"
                      stroke="currentColor" stroke-width="1.5"/>
              </svg>
              <span class="truncate">Manage Users</span>
              <svg class="w-3.5 h-3.5 ml-auto flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7" />
              </svg>
            </.link>
          </div>
        <% end %>

        <!-- Time & Status -->
        <div class="status-bar">
          <div class="flex items-center justify-between">
            <div class="flex items-center space-x-2">
              <div class="network-indicator">
                <div class="network-dot"></div>
                <span class="text-xs text-gray-600">Online</span>
              </div>
              <div class="battery-indicator">
                <div class="battery-level" style="width: 85%"></div>
              </div>
            </div>
            <div class="digital-clock">
              <span class="time-text"></span>
            </div>
          </div>
        </div>
      </div>

      <!-- Inline Styles -->
      <style>
        /* Revolutionary Base Styles */
        #revolutionary-sidebar {
          width: 256px;
          min-width: 256px;
          background: rgba(255, 255, 255, 0.1);
          backdrop-filter: blur(20px);
          -webkit-backdrop-filter: blur(20px);
          border-right: 1px solid rgba(255, 255, 255, 0.2);
          box-shadow:
            10px 0 40px rgba(0, 0, 0, 0.1),
            inset 1px 0 0 rgba(255, 255, 255, 0.1);
        }

        /* Glass Background */
        .glass-background {
          background: linear-gradient(
            135deg,
            rgba(255, 255, 255, 0.1) 0%,
            rgba(255, 255, 255, 0.05) 100%
          );
          backdrop-filter: blur(25px);
          -webkit-backdrop-filter: blur(25px);
        }

        /* Floating Blobs */
        .floating-blob {
          position: absolute;
          border-radius: 50%;
          filter: blur(30px);
          opacity: 0.25;
          animation: float-blob 20s ease-in-out infinite;
        }

        .blob-1 {
          width: 120px;
          height: 120px;
          background: linear-gradient(45deg, #3b82f6, #8b5cf6);
          top: -50px;
          left: -60px;
          animation-delay: 0s;
        }

        .blob-2 {
          width: 90px;
          height: 90px;
          background: linear-gradient(45deg, #ec4899, #f59e0b);
          top: 60%;
          right: -30px;
          animation-delay: 5s;
        }

        .blob-3 {
          width: 70px;
          height: 70px;
          background: linear-gradient(45deg, #10b981, #06b6d4);
          bottom: -30px;
          left: 50%;
          animation-delay: 10s;
        }

        .blob-4 {
          width: 80px;
          height: 80px;
          background: linear-gradient(45deg, #f97316, #eab308);
          top: 30%;
          left: 20%;
          animation-delay: 15s;
        }

        @keyframes float-blob {
          0%, 100% {
            transform: translate(0, 0) scale(1);
          }
          33% {
            transform: translate(20px, -20px) scale(1.05);
          }
          66% {
            transform: translate(-15px, 15px) scale(0.95);
          }
        }

        /* Grid Lines */
        .grid-lines {
          position: absolute;
          inset: 0;
          background-image:
            linear-gradient(rgba(255, 255, 255, 0.05) 1px, transparent 1px),
            linear-gradient(90deg, rgba(255, 255, 255, 0.05) 1px, transparent 1px);
          background-size: 16px 16px;
          mask-image: radial-gradient(ellipse at center, black 30%, transparent 70%);
          -webkit-mask-image: radial-gradient(ellipse at center, black 30%, transparent 70%);
        }

        /* Particle System */
        .particle-system {
          position: absolute;
          width: 100%;
          height: 100%;
        }

        .particle-system::before {
          content: '';
          position: absolute;
          width: 100%;
          height: 100%;
          background-image:
            radial-gradient(circle at 20% 30%, rgba(255, 255, 255, 0.08) 0.5px, transparent 0.5px),
            radial-gradient(circle at 60% 70%, rgba(255, 255, 255, 0.08) 0.5px, transparent 0.5px);
          background-size: 80px 80px;
          animation: particle-float 80s linear infinite;
        }

        @keyframes particle-float {
          from {
            background-position: 0 0;
          }
          to {
            background-position: 80px 80px;
          }
        }

        /* Quote Card */
        .quote-card {
          position: relative;
          background: rgba(255, 255, 255, 0.12);
          backdrop-filter: blur(8px);
          border-radius: 14px;
          border: 1px solid rgba(255, 255, 255, 0.15);
          overflow: hidden;
          transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1);
        }

        .quote-card:hover {
          transform: translateY(-1px);
          box-shadow:
            0 10px 25px rgba(0, 0, 0, 0.08),
            0 0 0 1px rgba(255, 255, 255, 0.25);
        }

        .quote-overlay {
          position: absolute;
          inset: 0;
          background: linear-gradient(
            45deg,
            transparent 0%,
            rgba(255, 255, 255, 0.08) 50%,
            transparent 100%
          );
          animation: hologram-scan 3s linear infinite;
        }

        @keyframes hologram-scan {
          0% {
            transform: translateX(-100%);
          }
          100% {
            transform: translateX(100%);
          }
        }

        .quote-icon-container {
          width: 40px;
          height: 40px;
          border-radius: 10px;
          background: linear-gradient(135deg, rgba(59, 130, 246, 0.1), rgba(139, 92, 246, 0.1));
          display: flex;
          align-items: center;
          justify-content: center;
        }

        .quote-icon {
          width: 24px;
          height: 24px;
          color: #3b82f6;
        }

        .quote-text {
          font-size: 13px;
          line-height: 1.4;
          color: #374151;
          font-style: italic;
          position: relative;
          padding-left: 16px;
          border-left: 2px solid rgba(59, 130, 246, 0.3);
        }

        .quote-text::before {
          content: "❝";
          position: absolute;
          left: 0;
          top: -4px;
          color: rgba(59, 130, 246, 0.5);
          font-size: 18px;
        }

        .quote-author {
          display: flex;
          align-items: center;
        }

        .refresh-quote-btn {
          width: 28px;
          height: 28px;
          border-radius: 8px;
          background: rgba(255, 255, 255, 0.08);
          border: 1px solid rgba(255, 255, 255, 0.15);
          display: flex;
          align-items: center;
          justify-content: center;
          transition: all 0.2s;
        }

        .refresh-quote-btn:hover {
          background: rgba(255, 255, 255, 0.15);
          transform: rotate(45deg);
        }

        .gradient-text {
          background: linear-gradient(45deg, #3b82f6, #8b5cf6, #ec4899);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
          animation: gradient-shift 5s ease infinite;
          background-size: 200% 200%;
        }

        @keyframes gradient-shift {
          0%, 100% {
            background-position: 0% 50%;
          }
          50% {
            background-position: 100% 50%;
          }
        }

        .navigation-grid {
          background: rgba(255, 255, 255, 0.08);
          backdrop-filter: blur(8px);
          border-radius: 14px;
          border: 1px solid rgba(255, 255, 255, 0.1);
          padding: 12px;
        }

        .grid-header {
          margin-bottom: 12px;
          padding: 0 4px;
        }

        .grid-layout {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          gap: 10px;
        }

        .nav-grid-item {
          position: relative;
          display: flex;
          flex-direction: column;
          align-items: center;
          padding: 12px 6px;
          border-radius: 10px;
          background: rgba(255, 255, 255, 0.05);
          border: 1px solid transparent;
          transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1);
          text-decoration: none;
          color: #374151;
        }

        .nav-grid-item:hover {
          background: rgba(255, 255, 255, 0.12);
          border-color: rgba(255, 255, 255, 0.18);
          transform: translateY(-1px) scale(1.02);
          box-shadow:
            0 6px 12px rgba(0, 0, 0, 0.08),
            inset 0 1px 0 rgba(255, 255, 255, 0.08);
        }

        .nav-grid-item.featured {
          background: linear-gradient(
            135deg,
            rgba(59, 130, 246, 0.08),
            rgba(139, 92, 246, 0.08)
          );
          border-color: rgba(59, 130, 246, 0.15);
        }

        .nav-grid-item.featured:hover {
          background: linear-gradient(
            135deg,
            rgba(59, 130, 246, 0.15),
            rgba(139, 92, 246, 0.15)
          );
        }

        .nav-icon-container {
          width: 40px;
          height: 40px;
          border-radius: 10px;
          background: rgba(255, 255, 255, 0.08);
          display: flex;
          align-items: center;
          justify-content: center;
          margin-bottom: 6px;
          transition: all 0.25s ease;
        }

        .nav-grid-item:hover .nav-icon-container {
          background: rgba(255, 255, 255, 0.15);
          transform: scale(1.08) rotate(3deg);
        }

        .nav-icon {
          width: 20px;
          height: 20px;
          color: #4b5563;
        }

        .nav-label {
          font-size: 11px;
          font-weight: 600;
          color: #374151;
          transition: all 0.25s ease;
        }

        .nav-grid-item:hover .nav-label {
          color: #1f2937;
        }

        .nav-badge {
          position: absolute;
          top: 6px;
          right: 6px;
          padding: 1px 4px;
          border-radius: 8px;
          background: #3b82f6;
          color: white;
          font-size: 9px;
          font-weight: bold;
          line-height: 1.2;
        }

        .nav-badge.featured {
          background: linear-gradient(45deg, #3b82f6, #8b5cf6);
        }

        .nav-indicator {
          position: absolute;
          top: 6px;
          left: 6px;
          width: 5px;
          height: 5px;
          border-radius: 50%;
          background: #10b981;
        }

        .nav-indicator.pulse::before {
          content: '';
          position: absolute;
          inset: -1px;
          border-radius: 50%;
          background: #10b981;
          animation: pulse 2s ease-out infinite;
        }

        @keyframes pulse {
          0% {
            transform: scale(0.8);
            opacity: 0.4;
          }
          70% {
            transform: scale(1.3);
            opacity: 0;
          }
          100% {
            transform: scale(0.8);
            opacity: 0;
          }
        }

        /* Quick Actions */
        .quick-actions {
          background: rgba(255, 255, 255, 0.08);
          backdrop-filter: blur(8px);
          border-radius: 14px;
          border: 1px solid rgba(255, 255, 255, 0.1);
          padding: 12px;
        }

        .actions-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 10px;
          padding: 0 2px;
        }

        .action-btn {
          width: 24px;
          height: 24px;
          border-radius: 6px;
          background: rgba(255, 255, 255, 0.08);
          border: 1px solid rgba(255, 255, 255, 0.15);
          display: flex;
          align-items: center;
          justify-content: center;
          transition: all 0.2s;
        }

        .action-btn:hover {
          background: rgba(255, 255, 255, 0.15);
        }

        .actions-grid {
          display: grid;
          grid-template-columns: repeat(2, 1fr);
          gap: 6px;
        }

        .action-item {
          display: flex;
          align-items: center;
          padding: 10px;
          border-radius: 10px;
          background: rgba(255, 255, 255, 0.05);
          border: 1px solid transparent;
          transition: all 0.2s;
          text-decoration: none;
          color: #374151;
          position: relative;
        }

        .action-item:hover {
          background: rgba(255, 255, 255, 0.1);
          border-color: rgba(255, 255, 255, 0.15);
          transform: translateX(1px);
        }

        .action-icon {
          margin-right: 10px;
          font-size: 16px;
          flex-shrink: 0;
        }

        .action-text {
          font-size: 13px;
          font-weight: 500;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }

        .notification-badge {
          position: absolute;
          top: 6px;
          right: 6px;
          width: 16px;
          height: 16px;
          border-radius: 50%;
          background: #ef4444;
          color: white;
          font-size: 9px;
          display: flex;
          align-items: center;
          justify-content: center;
          font-weight: bold;
        }

        /* Admin Panel */
        .admin-panel {
          background: linear-gradient(
            135deg,
            rgba(245, 158, 11, 0.08),
            rgba(217, 70, 239, 0.08)
          );
          backdrop-filter: blur(8px);
          border-radius: 14px;
          border: 1px solid rgba(245, 158, 11, 0.15);
          padding: 12px;
        }

        .admin-header {
          display: flex;
          align-items: center;
          gap: 6px;
          margin-bottom: 10px;
          padding: 0 2px;
        }

        .admin-badge {
          padding: 2px 6px;
          border-radius: 6px;
          background: linear-gradient(45deg, #f59e0b, #d946ef);
          color: white;
          font-size: 9px;
          font-weight: bold;
          flex-shrink: 0;
        }

        .admin-action {
          display: flex;
          align-items: center;
          padding: 10px;
          border-radius: 10px;
          background: rgba(255, 255, 255, 0.05);
          border: 1px solid transparent;
          transition: all 0.2s;
          text-decoration: none;
          color: #374151;
          gap: 8px;
        }

        .admin-action:hover {
          background: rgba(255, 255, 255, 0.1);
          border-color: rgba(255, 255, 255, 0.15);
          transform: translateX(2px);
        }

        .admin-icon {
          width: 16px;
          height: 16px;
          color: #f59e0b;
          flex-shrink: 0;
        }

        /* Status Bar */
        .status-bar {
          background: rgba(255, 255, 255, 0.08);
          backdrop-filter: blur(8px);
          border-radius: 14px;
          border: 1px solid rgba(255, 255, 255, 0.1);
          padding: 10px 12px;
        }

        .network-indicator {
          display: flex;
          align-items: center;
          gap: 4px;
        }

        .network-dot {
          width: 5px;
          height: 5px;
          border-radius: 50%;
          background: #10b981;
        }

        .battery-indicator {
          width: 36px;
          height: 14px;
          border: 1px solid rgba(0, 0, 0, 0.15);
          border-radius: 3px;
          padding: 2px;
          position: relative;
        }

        .battery-level {
          height: 100%;
          background: linear-gradient(90deg, #10b981, #3b82f6);
          border-radius: 2px;
          transition: width 0.3s ease;
        }

        .digital-clock {
          font-family: 'JetBrains Mono', monospace;
          font-size: 13px;
          font-weight: 600;
          color: #374151;
        }

        .time-text::after {
          content: attr(data-time);
        }

        /* Scrollbar Hidden */
        .scrollbar-hidden {
          scrollbar-width: none;
          -ms-overflow-style: none;
        }

        .scrollbar-hidden::-webkit-scrollbar {
          display: none;
        }

        /* Responsive Design */
        @media (max-width: 1024px) {
          #revolutionary-sidebar {
            width: 240px;
            min-width: 240px;
          }

          .grid-layout {
            gap: 8px;
          }

          .nav-grid-item {
            padding: 10px 4px;
          }
        }

        @media (max-width: 768px) {
          #revolutionary-sidebar {
            width: 100%;
            min-width: 100%;
            height: auto;
            position: fixed;
            bottom: 0;
            left: 0;
            right: 0;
            top: auto;
            z-index: 50;
            backdrop-filter: blur(30px);
            border-right: none;
            border-top: 1px solid rgba(255, 255, 255, 0.2);
          }

          .grid-layout {
            grid-template-columns: repeat(4, 1fr);
            overflow-x: auto;
            padding-bottom: 8px;
          }

          .quote-card,
          .quick-actions,
          .admin-panel {
            display: none;
          }
        }

        /* Compact Mobile View */
        @media (max-width: 640px) {
          #revolutionary-sidebar {
            padding: 0;
          }

          .grid-layout {
            grid-template-columns: repeat(8, 1fr);
            gap: 4px;
          }

          .nav-grid-item {
            padding: 8px 2px;
          }

          .nav-icon-container {
            width: 32px;
            height: 32px;
            margin-bottom: 4px;
          }

          .nav-icon {
            width: 16px;
            height: 16px;
          }

          .nav-label {
            font-size: 10px;
          }
        }
      </style>

      <script>
        class GlassSidebar {
          constructor() {
            this.sidebar = document.getElementById('revolutionary-sidebar');
            this.particleSystem = this.sidebar.querySelector('.particle-system');
            this.init();
          }

          init() {
            this.createParticles();
            this.updateTime();
            this.setupInteractions();
          }

          createParticles() {
            const particles = 15;
            for (let i = 0; i < particles; i++) {
              const particle = document.createElement('div');
              particle.className = 'particle';

              const size = Math.random() * 3 + 1;
              const x = Math.random() * 100;
              const y = Math.random() * 100;
              const duration = Math.random() * 8 + 8;
              const delay = Math.random() * 4;

              particle.style.cssText = `
                width: ${size}px;
                height: ${size}px;
                left: ${x}%;
                top: ${y}%;
                background: rgba(255, 255, 255, ${Math.random() * 0.15});
                animation: float ${duration}s ease-in-out ${delay}s infinite;
                position: absolute;
                border-radius: 50%;
              `;

              this.particleSystem.appendChild(particle);
            }
          }

          updateTime() {
            const timeElement = document.querySelector('.time-text');
            if (!timeElement) return;

            const update = () => {
              const now = new Date();
              const hours = String(now.getHours()).padStart(2, '0');
              const minutes = String(now.getMinutes()).padStart(2, '0');
              const timeString = `${hours}:${minutes}`;

              timeElement.textContent = timeString;
              timeElement.setAttribute('data-time', timeString);
            };

            update();
            setInterval(update, 60000);
          }

          setupInteractions() {
            // Quote refresh button
            const refreshBtn = document.querySelector('.refresh-quote-btn');
            if (refreshBtn) {
              refreshBtn.addEventListener('click', (e) => {
                e.preventDefault();
                refreshBtn.style.transform = 'rotate(180deg)';
                setTimeout(() => {
                  refreshBtn.style.transform = 'rotate(0deg)';
                }, 300);
              });
            }

            // Hover effects for grid items
            const gridItems = document.querySelectorAll('.nav-grid-item');
            gridItems.forEach(item => {
              item.addEventListener('mouseenter', () => {
                const icon = item.querySelector('.nav-icon-container');
                if (icon) {
                  icon.style.transform = 'scale(1.08) rotate(3deg)';
                }
              });

              item.addEventListener('mouseleave', () => {
                const icon = item.querySelector('.nav-icon-container');
                if (icon) {
                  icon.style.transform = 'scale(1) rotate(0deg)';
                }
              });
            });

            // Subtle parallax effect
            this.sidebar.addEventListener('mousemove', (e) => {
              const { left, top, width, height } = this.sidebar.getBoundingClientRect();
              const x = (e.clientX - left) / width;
              const y = (e.clientY - top) / height;

              const moveX = (x - 0.5) * 6;
              const moveY = (y - 0.5) * 6;

              this.sidebar.style.transform = `perspective(800px) rotateY(${moveX}deg) rotateX(${-moveY}deg)`;
            });

            this.sidebar.addEventListener('mouseleave', () => {
              this.sidebar.style.transform = 'perspective(800px) rotateY(0deg) rotateX(0deg)';
            });
          }
        }

        document.addEventListener('DOMContentLoaded', () => {
          new GlassSidebar();
        });

        window.GlassSidebar = GlassSidebar;
      </script>
    </div>
    """
  end
end
