defmodule MazarynWeb.AiLive.DatasetsComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="space-y-6">
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        <div class="group bg-white rounded-2xl border border-slate-200 hover:border-violet-300 hover:shadow-xl hover:shadow-violet-100 transition-all duration-300 overflow-hidden">
          <div class="relative h-48 bg-gradient-to-br from-blue-500 to-cyan-500 overflow-hidden">
            <div class="absolute inset-0 bg-black/20"></div>
            <div class="absolute bottom-4 left-4 right-4">
              <div class="flex items-center space-x-2 mb-2">
                <span class="px-2 py-1 bg-white/90 text-blue-700 text-xs font-semibold rounded-md">Public</span>
                <span class="px-2 py-1 bg-white/90 text-emerald-700 text-xs font-semibold rounded-md">Featured</span>
              </div>
              <h3 class="text-xl font-bold text-white">ImageNet 2024 Subset</h3>
            </div>
            <div class="absolute top-4 right-4">
              <button class="p-2 bg-white/90 rounded-lg hover:bg-white transition-colors">
                <svg class="w-5 h-5 text-slate-700" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
                </svg>
              </button>
            </div>
          </div>
          <div class="p-5">
            <p class="text-slate-600 text-sm mb-4 line-clamp-2">
              A curated subset of ImageNet with 100K high-quality labeled images across 1000 categories for efficient model training.
            </p>
            <div class="flex items-center space-x-4 mb-4">
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4" />
                </svg>
                <span class="text-xs font-medium">12.4K</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
                  <path d="M9.049 2.927c.3-.921 1.603-.921 1.902 0l1.07 3.292a1 1 0 00.95.69h3.462c.969 0 1.371 1.24.588 1.81l-2.8 2.034a1 1 0 00-.364 1.118l1.07 3.292c.3.921-.755 1.688-1.54 1.118l-2.8-2.034a1 1 0 00-1.175 0l-2.8 2.034c-.784.57-1.838-.197-1.539-1.118l1.07-3.292a1 1 0 00-.364-1.118L2.98 8.72c-.783-.57-.38-1.81.588-1.81h3.461a1 1 0 00.951-.69l1.07-3.292z" />
                </svg>
                <span class="text-xs font-medium">4.8</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                </svg>
                <span class="text-xs font-medium">2.3 GB</span>
              </div>
            </div>
            <div class="flex flex-wrap gap-2 mb-4">
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">computer-vision</span>
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">image-classification</span>
            </div>
            <div class="flex items-center justify-between pt-4 border-t border-slate-100">
              <div class="flex items-center space-x-2">
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full" alt="User avatar" />
                <span class="text-sm text-slate-600">@datamaster</span>
              </div>
              <span class="text-xs text-slate-400">Updated 2d ago</span>
            </div>
          </div>
        </div>

        <div class="group bg-white rounded-2xl border border-slate-200 hover:border-violet-300 hover:shadow-xl hover:shadow-violet-100 transition-all duration-300 overflow-hidden">
          <div class="relative h-48 bg-gradient-to-br from-emerald-500 to-teal-500 overflow-hidden">
            <div class="absolute inset-0 bg-black/20"></div>
            <div class="absolute bottom-4 left-4 right-4">
              <div class="flex items-center space-x-2 mb-2">
                <span class="px-2 py-1 bg-white/90 text-emerald-700 text-xs font-semibold rounded-md">Public</span>
              </div>
              <h3 class="text-xl font-bold text-white">NLP Sentiment Corpus</h3>
            </div>
            <div class="absolute top-4 right-4">
              <button class="p-2 bg-white/90 rounded-lg hover:bg-white transition-colors">
                <svg class="w-5 h-5 text-slate-700" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
                </svg>
              </button>
            </div>
          </div>
          <div class="p-5">
            <p class="text-slate-600 text-sm mb-4 line-clamp-2">
              Multi-language sentiment analysis dataset with 500K labeled reviews from various domains including products, movies, and services.
            </p>
            <div class="flex items-center space-x-4 mb-4">
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4" />
                </svg>
                <span class="text-xs font-medium">8.9K</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
                  <path d="M9.049 2.927c.3-.921 1.603-.921 1.902 0l1.07 3.292a1 1 0 00.95.69h3.462c.969 0 1.371 1.24.588 1.81l-2.8 2.034a1 1 0 00-.364 1.118l1.07 3.292c.3.921-.755 1.688-1.54 1.118l-2.8-2.034a1 1 0 00-1.175 0l-2.8 2.034c-.784.57-1.838-.197-1.539-1.118l1.07-3.292a1 1 0 00-.364-1.118L2.98 8.72c-.783-.57-.38-1.81.588-1.81h3.461a1 1 0 00.951-.69l1.07-3.292z" />
                </svg>
                <span class="text-xs font-medium">4.6</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                </svg>
                <span class="text-xs font-medium">890 MB</span>
              </div>
            </div>
            <div class="flex flex-wrap gap-2 mb-4">
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">nlp</span>
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">sentiment</span>
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">multilingual</span>
            </div>
            <div class="flex items-center justify-between pt-4 border-t border-slate-100">
              <div class="flex items-center space-x-2">
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full" alt="User avatar" />
                <span class="text-sm text-slate-600">@nlp_researcher</span>
              </div>
              <span class="text-xs text-slate-400">Updated 5d ago</span>
            </div>
          </div>
        </div>

        <div class="group bg-white rounded-2xl border border-slate-200 hover:border-violet-300 hover:shadow-xl hover:shadow-violet-100 transition-all duration-300 overflow-hidden">
          <div class="relative h-48 bg-gradient-to-br from-orange-500 to-red-500 overflow-hidden">
            <div class="absolute inset-0 bg-black/20"></div>
            <div class="absolute bottom-4 left-4 right-4">
              <div class="flex items-center space-x-2 mb-2">
                <span class="px-2 py-1 bg-white/90 text-orange-700 text-xs font-semibold rounded-md">Private</span>
              </div>
              <h3 class="text-xl font-bold text-white">Time Series Financial</h3>
            </div>
            <div class="absolute top-4 right-4">
              <button class="p-2 bg-white/90 rounded-lg hover:bg-white transition-colors">
                <svg class="w-5 h-5 text-slate-700" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
                </svg>
              </button>
            </div>
          </div>
          <div class="p-5">
            <p class="text-slate-600 text-sm mb-4 line-clamp-2">
              Historical stock market data with technical indicators for predictive modeling and algorithmic trading strategies.
            </p>
            <div class="flex items-center space-x-4 mb-4">
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4" />
                </svg>
                <span class="text-xs font-medium">3.2K</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
                  <path d="M9.049 2.927c.3-.921 1.603-.921 1.902 0l1.07 3.292a1 1 0 00.95.69h3.462c.969 0 1.371 1.24.588 1.81l-2.8 2.034a1 1 0 00-.364 1.118l1.07 3.292c.3.921-.755 1.688-1.54 1.118l-2.8-2.034a1 1 0 00-1.175 0l-2.8 2.034c-.784.57-1.838-.197-1.539-1.118l1.07-3.292a1 1 0 00-.364-1.118L2.98 8.72c-.783-.57-.38-1.81.588-1.81h3.461a1 1 0 00.951-.69l1.07-3.292z" />
                </svg>
                <span class="text-xs font-medium">4.9</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                </svg>
                <span class="text-xs font-medium">1.5 GB</span>
              </div>
            </div>
            <div class="flex flex-wrap gap-2 mb-4">
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">time-series</span>
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">finance</span>
            </div>
            <div class="flex items-center justify-between pt-4 border-t border-slate-100">
              <div class="flex items-center space-x-2">
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full" alt="User avatar" />
                <span class="text-sm text-slate-600">@quant_trader</span>
              </div>
              <span class="text-xs text-slate-400">Updated 1w ago</span>
            </div>
          </div>
        </div>
      </div>

      <div class="flex items-center justify-center">
        <button class="flex items-center space-x-2 px-6 py-3 text-violet-600 font-medium hover:bg-violet-50 rounded-xl transition-colors">
          <span>Load More Datasets</span>
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
          </svg>
        </button>
      </div>
    </div>
    """
  end
end
