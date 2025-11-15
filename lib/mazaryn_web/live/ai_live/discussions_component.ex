defmodule MazarynWeb.AiLive.DiscussionsComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="space-y-6">
      <div class="bg-white rounded-2xl border border-slate-200 p-6">
        <div class="flex items-center justify-between mb-6">
          <h3 class="text-lg font-semibold text-slate-900">Start a Discussion</h3>
        </div>
        <div class="space-y-4">
          <input
            type="text"
            placeholder="What's your question or topic?"
            class="w-full px-4 py-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-violet-500 focus:border-violet-500"
          />
          <textarea
            placeholder="Provide more details..."
            rows="3"
            class="w-full px-4 py-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-violet-500 focus:border-violet-500 resize-none"
          ></textarea>
          <div class="flex items-center justify-between">
            <div class="flex items-center space-x-2">
              <button class="px-3 py-1.5 text-slate-600 hover:bg-slate-100 rounded-lg text-sm font-medium transition-colors">
                Add Tags
              </button>
              <button class="px-3 py-1.5 text-slate-600 hover:bg-slate-100 rounded-lg text-sm font-medium transition-colors">
                Attach Code
              </button>
            </div>
            <button class="px-6 py-2.5 bg-gradient-to-r from-violet-600 to-indigo-600 text-white font-medium rounded-xl hover:from-violet-700 hover:to-indigo-700 transition-all">
              Post Discussion
            </button>
          </div>
        </div>
      </div>

      <div class="space-y-4">
        <div class="bg-white rounded-2xl border border-slate-200 hover:border-violet-300 transition-all p-6">
          <div class="flex items-start space-x-4">
            <img src="/images/default-user.svg" class="w-10 h-10 rounded-full" alt="User avatar" />
            <div class="flex-1">
              <div class="flex items-center justify-between mb-2">
                <div class="flex items-center space-x-2">
                  <span class="font-semibold text-slate-900">@ml_researcher</span>
                  <span class="text-slate-400">•</span>
                  <span class="text-sm text-slate-500">3 hours ago</span>
                </div>
                <div class="flex items-center space-x-2">
                  <span class="px-2 py-1 bg-green-100 text-green-700 text-xs font-semibold rounded">Solved</span>
                  <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-semibold rounded">Hot</span>
                </div>
              </div>
              <h3 class="text-lg font-bold text-slate-900 mb-2 hover:text-violet-600 cursor-pointer">
                Best practices for handling class imbalance in multi-label classification?
              </h3>
              <p class="text-slate-600 text-sm mb-4">
                I'm working on a multi-label classification problem where some labels appear in less than 1% of samples. I've tried SMOTE and class weights but neither seems to work well. Any suggestions for handling extreme class imbalance?
              </p>
              <div class="flex flex-wrap gap-2 mb-4">
                <span class="px-2 py-1 bg-slate-100 text-slate-600 text-xs font-medium rounded-md">machine-learning</span>
                <span class="px-2 py-1 bg-slate-100 text-slate-600 text-xs font-medium rounded-md">class-imbalance</span>
                <span class="px-2 py-1 bg-slate-100 text-slate-600 text-xs font-medium rounded-md">multi-label</span>
              </div>
              <div class="flex items-center space-x-6">
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 15l7-7 7 7" />
                  </svg>
                  <span class="text-sm font-medium">42</span>
                </button>
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z" />
                  </svg>
                  <span class="text-sm font-medium">18 answers</span>
                </button>
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z" />
                  </svg>
                  <span class="text-sm font-medium">1.2K views</span>
                </button>
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
                  </svg>
                  <span class="text-sm font-medium">Save</span>
                </button>
              </div>
            </div>
          </div>
        </div>

        <div class="bg-white rounded-2xl border border-slate-200 hover:border-violet-300 transition-all p-6">
          <div class="flex items-start space-x-4">
            <img src="/images/default-user.svg" class="w-10 h-10 rounded-full" alt="User avatar" />
            <div class="flex-1">
              <div class="flex items-center justify-between mb-2">
                <div class="flex items-center space-x-2">
                  <span class="font-semibold text-slate-900">@deep_learning_pro</span>
                  <span class="text-slate-400">•</span>
                  <span class="text-sm text-slate-500">8 hours ago</span>
                </div>
                <div class="flex items-center space-x-2">
                  <span class="px-2 py-1 bg-amber-100 text-amber-700 text-xs font-semibold rounded">Open</span>
                </div>
              </div>
              <h3 class="text-lg font-bold text-slate-900 mb-2 hover:text-violet-600 cursor-pointer">
                GPU memory optimization techniques for training large transformer models
              </h3>
              <p class="text-slate-600 text-sm mb-4">
                I'm trying to train a custom transformer model with 1B parameters but running into OOM errors even with gradient checkpointing. Looking for advanced memory optimization techniques beyond the basics.
              </p>
              <div class="flex flex-wrap gap-2 mb-4">
                <span class="px-2 py-1 bg-slate-100 text-slate-600 text-xs font-medium rounded-md">transformers</span>
                <span class="px-2 py-1 bg-slate-100 text-slate-600 text-xs font-medium rounded-md">gpu</span>
                <span class="px-2 py-1 bg-slate-100 text-slate-600 text-xs font-medium rounded-md">optimization</span>
              </div>
              <div class="flex items-center space-x-6">
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 15l7-7 7 7" />
                  </svg>
                  <span class="text-sm font-medium">28</span>
                </button>
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z" />
                  </svg>
                  <span class="text-sm font-medium">9 answers</span>
                </button>
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z" />
                  </svg>
                  <span class="text-sm font-medium">892 views</span>
                </button>
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
                  </svg>
                  <span class="text-sm font-medium">Save</span>
                </button>
              </div>
            </div>
          </div>
        </div>

        <div class="bg-white rounded-2xl border border-slate-200 hover:border-violet-300 transition-all p-6">
          <div class="flex items-start space-x-4">
            <img src="/images/default-user.svg" class="w-10 h-10 rounded-full" alt="User avatar" />
            <div class="flex-1">
              <div class="flex items-center justify-between mb-2">
                <div class="flex items-center space-x-2">
                  <span class="font-semibold text-slate-900">@data_engineer</span>
                  <span class="text-slate-400">•</span>
                  <span class="text-sm text-slate-500">1 day ago</span>
                </div>
                <div class="flex items-center space-x-2">
                  <span class="px-2 py-1 bg-blue-100 text-blue-700 text-xs font-semibold rounded">Discussion</span>
                </div>
              </div>
              <h3 class="text-lg font-bold text-slate-900 mb-2 hover:text-violet-600 cursor-pointer">
                What's your preferred data versioning tool? DVC vs LakeFS vs Delta Lake
              </h3>
              <p class="text-slate-600 text-sm mb-4">
                I'm setting up MLOps infrastructure and need to choose a data versioning solution. Would love to hear experiences with different tools, especially for large datasets (100GB+).
              </p>
              <div class="flex flex-wrap gap-2 mb-4">
                <span class="px-2 py-1 bg-slate-100 text-slate-600 text-xs font-medium rounded-md">mlops</span>
                <span class="px-2 py-1 bg-slate-100 text-slate-600 text-xs font-medium rounded-md">data-versioning</span>
                <span class="px-2 py-1 bg-slate-100 text-slate-600 text-xs font-medium rounded-md">infrastructure</span>
              </div>
              <div class="flex items-center space-x-6">
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 15l7-7 7 7" />
                  </svg>
                  <span class="text-sm font-medium">56</span>
                </button>
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z" />
                  </svg>
                  <span class="text-sm font-medium">34 comments</span>
                </button>
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z" />
                  </svg>
                  <span class="text-sm font-medium">2.1K views</span>
                </button>
                <button class="flex items-center space-x-1 text-slate-500 hover:text-violet-600 transition-colors">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
                  </svg>
                  <span class="text-sm font-medium">Save</span>
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>

      <div class="flex items-center justify-center">
        <button class="flex items-center space-x-2 px-6 py-3 text-violet-600 font-medium hover:bg-violet-50 rounded-xl transition-colors">
          <span>Load More Discussions</span>
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
          </svg>
        </button>
      </div>
    </div>
    """
  end
end
