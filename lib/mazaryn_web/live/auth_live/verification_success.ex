defmodule MazarynWeb.AuthLive.VerificationSuccess do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, _session, socket) do
    Logger.debug("VerificationSuccess mount")
    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="min-h-screen bg-gradient-to-br from-pink-50 via-white to-rose-50 flex items-center justify-center p-4">
      <div class="absolute inset-0">
        <div class="absolute top-1/4 left-1/4 w-64 h-64 bg-gradient-to-r from-pink-300 to-transparent rounded-full blur-3xl opacity-20"></div>
        <div class="absolute bottom-1/4 right-1/4 w-96 h-96 bg-gradient-to-l from-rose-300 to-transparent rounded-full blur-3xl opacity-20"></div>
      </div>

      <div class="relative z-10 w-full max-w-md">
        <div class="bg-white/80 backdrop-blur-sm rounded-2xl shadow-xl border border-pink-100 p-8 md:p-12">
          <div class="text-center">
            <div class="mx-auto w-20 h-20 bg-gradient-to-br from-green-400 to-green-600 rounded-full flex items-center justify-center mb-6 shadow-lg">
              <svg class="w-12 h-12 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path>
              </svg>
            </div>

            <h1 class="text-3xl font-bold text-gray-900 mb-4">
              <span class="bg-gradient-to-r from-pink-600 to-rose-600 bg-clip-text text-transparent">
                <%= gettext("Email Verified!") %>
              </span>
            </h1>

            <p class="text-lg text-gray-600 mb-8">
              <%= gettext("Your account has been successfully verified. You can now log in and start using Mazaryn.") %>
            </p>

            <div class="space-y-4">
              <.link
                navigate={~p"/#{@locale}/login"}
                class="block w-full py-3 px-4 bg-gradient-to-r from-pink-500 to-rose-500 text-white rounded-lg hover:shadow-lg hover:scale-[1.02] transition-all duration-300 font-medium shadow-md text-center"
              >
                <%= gettext("Go to Login") %>
              </.link>

              <.link
                navigate={~p"/#{@locale}"}
                class="block w-full py-3 px-4 border-2 border-pink-200 text-pink-600 rounded-lg hover:bg-pink-50 transition-all duration-300 font-medium text-center"
              >
                <%= gettext("Back to Home") %>
              </.link>
            </div>
          </div>

          <div class="mt-8 pt-6 border-t border-pink-100">
            <div class="flex items-center justify-center space-x-2 text-gray-600">
              <div class="w-2 h-2 bg-gradient-to-r from-pink-500 to-rose-500 rounded-full"></div>
              <span class="text-sm"><%= gettext("Welcome to the Mazaryn community!") %></span>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
