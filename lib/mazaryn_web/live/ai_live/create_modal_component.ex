defmodule MazarynWeb.AiLive.CreateModalComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="fixed inset-0 z-50 overflow-y-auto" phx-click="close_create_modal">
      <div class="flex min-h-screen items-center justify-center p-4">
        <div class="fixed inset-0 bg-black/50 backdrop-blur-sm"></div>
        <div class="relative w-full max-w-2xl bg-white rounded-2xl shadow-2xl" phx-click="stop_propagation">
          <div class="flex items-center justify-between p-6 border-b border-slate-200">
            <div class="flex items-center space-x-3">
              <div class="w-10 h-10 bg-gradient-to-br from-violet-600 to-indigo-600 rounded-xl flex items-center justify-center">
                <svg class="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 4v16m8-8H4" />
                </svg>
              </div>
              <div>
                <h2 class="text-xl font-bold text-slate-900">Create New <%= String.capitalize(@create_type || "Item") %></h2>
                <p class="text-sm text-slate-500">Fill in the details below</p>
              </div>
            </div>
            <button phx-click="close_create_modal" class="p-2 text-slate-400 hover:text-slate-600 rounded-lg hover:bg-slate-100">
              <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />
              </svg>
            </button>
          </div>
          <div class="p-6 max-h-[60vh] overflow-y-auto">
            <%= case @create_type do %>
              <% "datasets" -> %>
                <div class="space-y-6">
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Dataset Title</label>
                    <input type="text" placeholder="e.g., Customer Churn Prediction Dataset" class="w-full px-4 py-3 border border-slate-200 rounded-xl focus:ring-2 focus:ring-violet-500" />
                  </div>
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Description</label>
                    <textarea rows="4" placeholder="Describe your dataset..." class="w-full px-4 py-3 border border-slate-200 rounded-xl resize-none"></textarea>
                  </div>
                  <div class="grid grid-cols-2 gap-4">
                    <div>
                      <label class="block text-sm font-medium text-slate-700 mb-2">License</label>
                      <select class="w-full px-4 py-3 border border-slate-200 rounded-xl">
                        <option>CC0: Public Domain</option>
                        <option>CC BY 4.0</option>
                        <option>MIT</option>
                      </select>
                    </div>
                    <div>
                      <label class="block text-sm font-medium text-slate-700 mb-2">Visibility</label>
                      <select class="w-full px-4 py-3 border border-slate-200 rounded-xl">
                        <option>Public</option>
                        <option>Private</option>
                      </select>
                    </div>
                  </div>
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Upload Files</label>
                    <div class="border-2 border-dashed border-slate-300 rounded-xl p-8 text-center">
                      <svg class="w-12 h-12 text-slate-400 mx-auto mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12" />
                      </svg>
                      <p class="text-slate-600 mb-2">Drag and drop files here</p>
                      <p class="text-sm text-slate-400">CSV, JSON, Parquet (max 50GB)</p>
                    </div>
                  </div>
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Tags</label>
                    <input type="text" placeholder="classification, tabular (comma separated)" class="w-full px-4 py-3 border border-slate-200 rounded-xl" />
                  </div>
                </div>
              <% "competitions" -> %>
                <div class="space-y-6">
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Competition Title</label>
                    <input type="text" placeholder="e.g., Image Classification Challenge" class="w-full px-4 py-3 border border-slate-200 rounded-xl" />
                  </div>
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Description</label>
                    <textarea rows="4" placeholder="Describe competition goals..." class="w-full px-4 py-3 border border-slate-200 rounded-xl resize-none"></textarea>
                  </div>
                  <div class="grid grid-cols-2 gap-4">
                    <div>
                      <label class="block text-sm font-medium text-slate-700 mb-2">Start Date</label>
                      <input type="datetime-local" class="w-full px-4 py-3 border border-slate-200 rounded-xl" />
                    </div>
                    <div>
                      <label class="block text-sm font-medium text-slate-700 mb-2">End Date</label>
                      <input type="datetime-local" class="w-full px-4 py-3 border border-slate-200 rounded-xl" />
                    </div>
                  </div>
                  <div class="grid grid-cols-2 gap-4">
                    <div>
                      <label class="block text-sm font-medium text-slate-700 mb-2">Prize Pool ($)</label>
                      <input type="number" placeholder="5000" class="w-full px-4 py-3 border border-slate-200 rounded-xl" />
                    </div>
                    <div>
                      <label class="block text-sm font-medium text-slate-700 mb-2">Max Team Size</label>
                      <input type="number" placeholder="5" class="w-full px-4 py-3 border border-slate-200 rounded-xl" />
                    </div>
                  </div>
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Evaluation Metric</label>
                    <select class="w-full px-4 py-3 border border-slate-200 rounded-xl">
                      <option>Accuracy</option>
                      <option>F1 Score</option>
                      <option>AUC-ROC</option>
                      <option>RMSE</option>
                    </select>
                  </div>
                </div>
              <% "notebooks" -> %>
                <div class="space-y-6">
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Notebook Title</label>
                    <input type="text" placeholder="e.g., Complete EDA Pipeline" class="w-full px-4 py-3 border border-slate-200 rounded-xl" />
                  </div>
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Description</label>
                    <textarea rows="4" placeholder="What will this notebook teach?" class="w-full px-4 py-3 border border-slate-200 rounded-xl resize-none"></textarea>
                  </div>
                  <div class="grid grid-cols-2 gap-4">
                    <div>
                      <label class="block text-sm font-medium text-slate-700 mb-2">Language</label>
                      <select class="w-full px-4 py-3 border border-slate-200 rounded-xl">
                        <option>Python</option>
                        <option>R</option>
                        <option>Julia</option>
                      </select>
                    </div>
                    <div>
                      <label class="block text-sm font-medium text-slate-700 mb-2">Kernel Type</label>
                      <select class="w-full px-4 py-3 border border-slate-200 rounded-xl">
                        <option>Python 3.10</option>
                        <option>Python 3.11 (GPU)</option>
                        <option>R 4.3</option>
                      </select>
                    </div>
                  </div>
                  <div>
                    <label class="block text-sm font-medium text-slate-700 mb-2">Tags</label>
                    <input type="text" placeholder="machine-learning, eda" class="w-full px-4 py-3 border border-slate-200 rounded-xl" />
                  </div>
                </div>
              <% _ -> %>
                <p class="text-slate-500">Select a type to create</p>
            <% end %>
          </div>
          <div class="flex items-center justify-end space-x-3 p-6 border-t border-slate-200 bg-slate-50 rounded-b-2xl">
            <button phx-click="close_create_modal" class="px-6 py-2.5 text-slate-700 font-medium rounded-xl hover:bg-slate-200">Cancel</button>
            <button phx-click={"create_#{@create_type}"} class="px-6 py-2.5 bg-gradient-to-r from-violet-600 to-indigo-600 text-white font-medium rounded-xl shadow-lg">
              Create <%= String.capitalize(String.replace(@create_type || "", "s", "")) %>
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
