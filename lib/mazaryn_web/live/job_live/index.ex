defmodule MazarynWeb.JobLive.Index do
  use MazarynWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="max-w-4xl mx-auto py-12 px-6 text-center">
      <h1 class="text-4xl font-bold text-gray-900 mb-6">
        <%= gettext("Job Platform") %>
      </h1>

      <div class="bg-gradient-to-br from-indigo-100 via-purple-100 to-pink-100 rounded-2xl p-12 shadow-lg">
        <svg class="w-24 h-24 mx-auto mb-8 text-indigo-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2"
            d="M21 13.255A23.931 23.931 0 0112 15c-3.183 0-6.22-.62-9-1.745M16 6V4a2 2 0 00-2-2h-4a2 2 0 00-2 2v2m4 6h.01M5 20h14a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z">
          </path>
        </svg>

        <p class="text-2xl font-semibold text-gray-800 mb-4">
          <%= gettext("Coming Soon!") %>
        </p>

        <p class="text-lg text-gray-600 max-w-2xl mx-auto">
          <%= gettext("We are working hard to bring you a powerful job platform where you can find opportunities, post jobs, and connect with talent across the Mazaryn community.") %>
        </p>

        <div class="mt-10">
          <span class="inline-block px-6 py-3 bg-gradient-to-r from-indigo-600 to-purple-600 text-white font-bold rounded-full text-lg shadow-md">
            <%= gettext("Stay tuned") %> 🚀
          </span>
        </div>
      </div>
    </div>
    """
  end
end
