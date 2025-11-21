defmodule MazarynWeb.AiLive.CompetitionsComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="space-y-6">
      <div class="bg-gradient-to-r from-violet-600 to-indigo-600 rounded-2xl p-8 text-white relative overflow-hidden">
        <div class="absolute top-0 right-0 w-64 h-64 bg-white/10 rounded-full -translate-y-32 translate-x-32"></div>
        <div class="absolute bottom-0 left-0 w-48 h-48 bg-white/10 rounded-full translate-y-24 -translate-x-24"></div>
        <div class="relative z-10">
          <div class="flex items-center space-x-2 mb-4">
            <svg class="w-8 h-8" fill="currentColor" viewBox="0 0 20 20">
              <path fill-rule="evenodd" d="M5 2a1 1 0 011 1v1h1a1 1 0 010 2H6v1a1 1 0 01-2 0V6H3a1 1 0 010-2h1V3a1 1 0 011-1zm0 10a1 1 0 011 1v1h1a1 1 0 110 2H6v1a1 1 0 11-2 0v-1H3a1 1 0 110-2h1v-1a1 1 0 011-1zM12 2a1 1 0 01.967.744L14.146 7.2 17.5 9.134a1 1 0 010 1.732l-3.354 1.935-1.18 4.455a1 1 0 01-1.933 0L9.854 12.8 6.5 10.866a1 1 0 010-1.732l3.354-1.935 1.18-4.455A1 1 0 0112 2z" clip-rule="evenodd" />
            </svg>
            <h2 class="text-2xl font-bold">Featured Competition</h2>
          </div>
          <h3 class="text-3xl font-extrabold mb-3">Global AI Image Classification Challenge 2025</h3>
          <p class="text-white/90 mb-6 max-w-2xl">
            Push the boundaries of computer vision! Build the most accurate image classifier across 10,000 categories.
            Top prizes include GPU credits, cash rewards, and industry recognition.
          </p>
          <div class="flex items-center space-x-8 mb-6">
            <div>
              <div class="text-3xl font-bold">$50,000</div>
              <div class="text-white/70 text-sm">Total Prize Pool</div>
            </div>
            <div>
              <div class="text-3xl font-bold">2,847</div>
              <div class="text-white/70 text-sm">Participants</div>
            </div>
            <div>
              <div class="text-3xl font-bold">14 days</div>
              <div class="text-white/70 text-sm">Remaining</div>
            </div>
          </div>
          <button class="px-8 py-3 bg-white text-violet-700 font-semibold rounded-xl hover:bg-gray-100 transition-colors shadow-lg">
            Join Competition
          </button>
        </div>
      </div>

      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div class="bg-white rounded-2xl border border-slate-200 hover:border-green-300 hover:shadow-xl transition-all duration-300 p-6">
          <div class="flex items-start justify-between mb-4">
            <div class="flex items-center space-x-3">
              <div class="w-12 h-12 bg-gradient-to-br from-green-500 to-emerald-500 rounded-xl flex items-center justify-center">
                <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
                </svg>
              </div>
              <div>
                <h3 class="text-lg font-bold text-slate-900">Tabular Data Prediction</h3>
                <div class="flex items-center space-x-2 mt-1">
                  <span class="px-2 py-0.5 bg-green-100 text-green-700 text-xs font-semibold rounded-md">Active</span>
                  <span class="px-2 py-0.5 bg-blue-100 text-blue-700 text-xs font-semibold rounded-md">Beginner</span>
                </div>
              </div>
            </div>
            <button class="p-2 text-slate-400 hover:text-violet-600 transition-colors">
              <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
              </svg>
            </button>
          </div>

          <p class="text-slate-600 text-sm mb-4">
            Predict housing prices using structured data. Great for learning feature engineering and ensemble methods.
          </p>

          <div class="grid grid-cols-3 gap-4 mb-4">
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">$5,000</div>
              <div class="text-xs text-slate-500">Prize</div>
            </div>
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">1,234</div>
              <div class="text-xs text-slate-500">Teams</div>
            </div>
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">23d</div>
              <div class="text-xs text-slate-500">Left</div>
            </div>
          </div>

          <div class="mb-4">
            <div class="flex items-center justify-between text-xs text-slate-500 mb-1">
              <span>Progress</span>
              <span>68% complete</span>
            </div>
            <div class="w-full bg-slate-200 rounded-full h-2">
              <div class="bg-gradient-to-r from-green-500 to-emerald-500 h-2 rounded-full" style="width: 68%"></div>
            </div>
          </div>

          <div class="flex items-center justify-between pt-4 border-t border-slate-100">
            <div class="flex items-center space-x-2">
              <div class="flex -space-x-2">
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full border-2 border-white" />
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full border-2 border-white" />
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full border-2 border-white" />
              </div>
              <span class="text-xs text-slate-500">+1,231 others</span>
            </div>
            <button class="px-4 py-2 bg-green-600 text-white text-sm font-medium rounded-lg hover:bg-green-700 transition-colors">
              Join Now
            </button>
          </div>
        </div>

        <div class="bg-white rounded-2xl border border-slate-200 hover:border-purple-300 hover:shadow-xl transition-all duration-300 p-6">
          <div class="flex items-start justify-between mb-4">
            <div class="flex items-center space-x-3">
              <div class="w-12 h-12 bg-gradient-to-br from-purple-500 to-pink-500 rounded-xl flex items-center justify-center">
                <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z" />
                </svg>
              </div>
              <div>
                <h3 class="text-lg font-bold text-slate-900">NLP Question Answering</h3>
                <div class="flex items-center space-x-2 mt-1">
                  <span class="px-2 py-0.5 bg-green-100 text-green-700 text-xs font-semibold rounded-md">Active</span>
                  <span class="px-2 py-0.5 bg-orange-100 text-orange-700 text-xs font-semibold rounded-md">Advanced</span>
                </div>
              </div>
            </div>
            <button class="p-2 text-slate-400 hover:text-violet-600 transition-colors">
              <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
              </svg>
            </button>
          </div>

          <p class="text-slate-600 text-sm mb-4">
            Build a state-of-the-art question answering system using transformer models. Requires GPU compute.
          </p>

          <div class="grid grid-cols-3 gap-4 mb-4">
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">$15,000</div>
              <div class="text-xs text-slate-500">Prize</div>
            </div>
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">892</div>
              <div class="text-xs text-slate-500">Teams</div>
            </div>
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">7d</div>
              <div class="text-xs text-slate-500">Left</div>
            </div>
          </div>

          <div class="mb-4">
            <div class="flex items-center justify-between text-xs text-slate-500 mb-1">
              <span>Progress</span>
              <span>89% complete</span>
            </div>
            <div class="w-full bg-slate-200 rounded-full h-2">
              <div class="bg-gradient-to-r from-purple-500 to-pink-500 h-2 rounded-full" style="width: 89%"></div>
            </div>
          </div>

          <div class="flex items-center justify-between pt-4 border-t border-slate-100">
            <div class="flex items-center space-x-2">
              <div class="flex -space-x-2">
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full border-2 border-white" />
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full border-2 border-white" />
              </div>
              <span class="text-xs text-slate-500">+890 others</span>
            </div>
            <button class="px-4 py-2 bg-purple-600 text-white text-sm font-medium rounded-lg hover:bg-purple-700 transition-colors">
              Join Now
            </button>
          </div>
        </div>

        <div class="bg-white rounded-2xl border border-slate-200 hover:border-amber-300 hover:shadow-xl transition-all duration-300 p-6">
          <div class="flex items-start justify-between mb-4">
            <div class="flex items-center space-x-3">
              <div class="w-12 h-12 bg-gradient-to-br from-amber-500 to-orange-500 rounded-xl flex items-center justify-center">
                <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 12l3-3 3 3 4-4M8 21l4-4 4 4M3 4h18M4 4h16v12a1 1 0 01-1 1H5a1 1 0 01-1-1V4z" />
                </svg>
              </div>
              <div>
                <h3 class="text-lg font-bold text-slate-900">Time Series Forecasting</h3>
                <div class="flex items-center space-x-2 mt-1">
                  <span class="px-2 py-0.5 bg-amber-100 text-amber-700 text-xs font-semibold rounded-md">Starting Soon</span>
                  <span class="px-2 py-0.5 bg-yellow-100 text-yellow-700 text-xs font-semibold rounded-md">Intermediate</span>
                </div>
              </div>
            </div>
            <button class="p-2 text-slate-400 hover:text-violet-600 transition-colors">
              <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
              </svg>
            </button>
          </div>

          <p class="text-slate-600 text-sm mb-4">
            Forecast energy consumption patterns using historical data. Explore LSTM, Prophet, and advanced techniques.
          </p>

          <div class="grid grid-cols-3 gap-4 mb-4">
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">$10,000</div>
              <div class="text-xs text-slate-500">Prize</div>
            </div>
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">456</div>
              <div class="text-xs text-slate-500">Registered</div>
            </div>
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">3d</div>
              <div class="text-xs text-slate-500">Until Start</div>
            </div>
          </div>

          <div class="flex items-center justify-between pt-4 border-t border-slate-100">
            <div class="flex items-center space-x-2">
              <svg class="w-4 h-4 text-amber-500" fill="currentColor" viewBox="0 0 20 20">
                <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm1-12a1 1 0 10-2 0v4a1 1 0 00.293.707l2.828 2.829a1 1 0 101.415-1.415L11 9.586V6z" clip-rule="evenodd" />
              </svg>
              <span class="text-xs text-slate-500">Starts in 3 days</span>
            </div>
            <button class="px-4 py-2 bg-amber-600 text-white text-sm font-medium rounded-lg hover:bg-amber-700 transition-colors">
              Register
            </button>
          </div>
        </div>

        <div class="bg-white rounded-2xl border border-slate-200 hover:border-red-300 hover:shadow-xl transition-all duration-300 p-6">
          <div class="flex items-start justify-between mb-4">
            <div class="flex items-center space-x-3">
              <div class="w-12 h-12 bg-gradient-to-br from-red-500 to-rose-500 rounded-xl flex items-center justify-center">
                <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4.318 6.318a4.5 4.5 0 000 6.364L12 20.364l7.682-7.682a4.5 4.5 0 00-6.364-6.364L12 7.636l-1.318-1.318a4.5 4.5 0 00-6.364 0z" />
                </svg>
              </div>
              <div>
                <h3 class="text-lg font-bold text-slate-900">Medical Image Analysis</h3>
                <div class="flex items-center space-x-2 mt-1">
                  <span class="px-2 py-0.5 bg-gray-100 text-gray-700 text-xs font-semibold rounded-md">Ended</span>
                  <span class="px-2 py-0.5 bg-red-100 text-red-700 text-xs font-semibold rounded-md">Expert</span>
                </div>
              </div>
            </div>
            <button class="p-2 text-slate-400 hover:text-violet-600 transition-colors">
              <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
              </svg>
            </button>
          </div>

          <p class="text-slate-600 text-sm mb-4">
            Detect abnormalities in chest X-rays using deep learning. Critical for healthcare AI advancement.
          </p>

          <div class="grid grid-cols-3 gap-4 mb-4">
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">$25,000</div>
              <div class="text-xs text-slate-500">Awarded</div>
            </div>
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">2,156</div>
              <div class="text-xs text-slate-500">Participated</div>
            </div>
            <div class="text-center p-3 bg-slate-50 rounded-lg">
              <div class="text-lg font-bold text-slate-900">98.7%</div>
              <div class="text-xs text-slate-500">Top Score</div>
            </div>
          </div>

          <div class="flex items-center justify-between pt-4 border-t border-slate-100">
            <div class="flex items-center space-x-2">
              <svg class="w-4 h-4 text-green-500" fill="currentColor" viewBox="0 0 20 20">
                <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clip-rule="evenodd" />
              </svg>
              <span class="text-xs text-slate-500">Completed 2 weeks ago</span>
            </div>
            <button class="px-4 py-2 bg-slate-600 text-white text-sm font-medium rounded-lg hover:bg-slate-700 transition-colors">
              View Results
            </button>
          </div>
        </div>
      </div>

      <div class="flex items-center justify-center">
        <button class="flex items-center space-x-2 px-6 py-3 text-violet-600 font-medium hover:bg-violet-50 rounded-xl transition-colors">
          <span>Load More Competitions</span>
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
          </svg>
        </button>
      </div>
    </div>
    """
  end
end
