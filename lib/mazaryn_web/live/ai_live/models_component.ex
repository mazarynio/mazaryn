defmodule MazarynWeb.AiLive.ModelsComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="space-y-6">
      <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        <div class="bg-white rounded-2xl border border-slate-200 hover:border-violet-300 hover:shadow-xl transition-all duration-300 p-6">
          <div class="flex items-center justify-between mb-4">
            <div class="w-12 h-12 bg-gradient-to-br from-orange-500 to-red-500 rounded-xl flex items-center justify-center">
              <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z" />
              </svg>
            </div>
            <div class="flex items-center space-x-2">
              <span class="px-2 py-1 bg-green-100 text-green-700 text-xs font-semibold rounded-md">Deployed</span>
            </div>
          </div>

          <h3 class="text-lg font-bold text-slate-900 mb-2">ResNet-50 Image Classifier</h3>
          <p class="text-slate-600 text-sm mb-4">
            Pre-trained ResNet-50 model fine-tuned on custom dataset for multi-class image classification.
          </p>

          <div class="space-y-3 mb-4">
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">Framework</span>
              <span class="font-medium text-slate-900">PyTorch</span>
            </div>
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">Accuracy</span>
              <span class="font-medium text-green-600">94.2%</span>
            </div>
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">Size</span>
              <span class="font-medium text-slate-900">98 MB</span>
            </div>
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">Downloads</span>
              <span class="font-medium text-slate-900">2,345</span>
            </div>
          </div>

          <div class="flex flex-wrap gap-2 mb-4">
            <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">pytorch</span>
            <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">image-classification</span>
          </div>

          <div class="flex items-center space-x-2">
            <button class="flex-1 px-4 py-2 bg-violet-600 text-white text-sm font-medium rounded-lg hover:bg-violet-700 transition-colors">
              Download
            </button>
            <button class="px-4 py-2 border border-slate-300 text-slate-700 text-sm font-medium rounded-lg hover:bg-slate-50 transition-colors">
              API
            </button>
          </div>
        </div>

        <div class="bg-white rounded-2xl border border-slate-200 hover:border-blue-300 hover:shadow-xl transition-all duration-300 p-6">
          <div class="flex items-center justify-between mb-4">
            <div class="w-12 h-12 bg-gradient-to-br from-blue-500 to-cyan-500 rounded-xl flex items-center justify-center">
              <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 10h.01M12 10h.01M16 10h.01M9 16H5a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v8a2 2 0 01-2 2h-5l-5 5v-5z" />
              </svg>
            </div>
            <div class="flex items-center space-x-2">
              <span class="px-2 py-1 bg-blue-100 text-blue-700 text-xs font-semibold rounded-md">Active</span>
            </div>
          </div>

          <h3 class="text-lg font-bold text-slate-900 mb-2">BERT Sentiment Analyzer</h3>
          <p class="text-slate-600 text-sm mb-4">
            Fine-tuned BERT model for sentiment analysis supporting multiple languages and domains.
          </p>

          <div class="space-y-3 mb-4">
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">Framework</span>
              <span class="font-medium text-slate-900">Transformers</span>
            </div>
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">F1 Score</span>
              <span class="font-medium text-green-600">0.91</span>
            </div>
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">Size</span>
              <span class="font-medium text-slate-900">440 MB</span>
            </div>
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">API Calls</span>
              <span class="font-medium text-slate-900">12.5K/day</span>
            </div>
          </div>

          <div class="flex flex-wrap gap-2 mb-4">
            <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">nlp</span>
            <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">sentiment</span>
            <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">bert</span>
          </div>

          <div class="flex items-center space-x-2">
            <button class="flex-1 px-4 py-2 bg-blue-600 text-white text-sm font-medium rounded-lg hover:bg-blue-700 transition-colors">
              Download
            </button>
            <button class="px-4 py-2 border border-slate-300 text-slate-700 text-sm font-medium rounded-lg hover:bg-slate-50 transition-colors">
              API
            </button>
          </div>
        </div>

        <div class="bg-white rounded-2xl border border-slate-200 hover:border-green-300 hover:shadow-xl transition-all duration-300 p-6">
          <div class="flex items-center justify-between mb-4">
            <div class="w-12 h-12 bg-gradient-to-br from-green-500 to-emerald-500 rounded-xl flex items-center justify-center">
              <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 12l3-3 3 3 4-4M8 21l4-4 4 4M3 4h18M4 4h16v12a1 1 0 01-1 1H5a1 1 0 01-1-1V4z" />
              </svg>
            </div>
            <div class="flex items-center space-x-2">
              <span class="px-2 py-1 bg-amber-100 text-amber-700 text-xs font-semibold rounded-md">Training</span>
            </div>
          </div>

          <h3 class="text-lg font-bold text-slate-900 mb-2">LSTM Time Series Predictor</h3>
          <p class="text-slate-600 text-sm mb-4">
            Advanced LSTM network for time series forecasting with attention mechanism.
          </p>

          <div class="space-y-3 mb-4">
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">Framework</span>
              <span class="font-medium text-slate-900">TensorFlow</span>
            </div>
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">RMSE</span>
              <span class="font-medium text-green-600">0.023</span>
            </div>
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">Epochs</span>
              <span class="font-medium text-slate-900">145/200</span>
            </div>
            <div class="flex items-center justify-between text-sm">
              <span class="text-slate-500">ETA</span>
              <span class="font-medium text-slate-900">2h 15m</span>
            </div>
          </div>

          <div class="mb-4">
            <div class="w-full bg-slate-200 rounded-full h-2">
              <div class="bg-gradient-to-r from-green-500 to-emerald-500 h-2 rounded-full" style="width: 72%"></div>
            </div>
          </div>

          <div class="flex items-center space-x-2">
            <button class="flex-1 px-4 py-2 bg-amber-600 text-white text-sm font-medium rounded-lg hover:bg-amber-700 transition-colors">
              View Progress
            </button>
            <button class="px-4 py-2 border border-red-300 text-red-600 text-sm font-medium rounded-lg hover:bg-red-50 transition-colors">
              Stop
            </button>
          </div>
        </div>
      </div>

      <div class="bg-white rounded-2xl border border-slate-200 p-6">
        <h3 class="text-lg font-semibold text-slate-900 mb-4">Model Registry</h3>
        <div class="overflow-x-auto">
          <table class="w-full">
            <thead>
              <tr class="text-left text-sm text-slate-500 border-b border-slate-200">
                <th class="pb-3 font-medium">Model Name</th>
                <th class="pb-3 font-medium">Version</th>
                <th class="pb-3 font-medium">Status</th>
                <th class="pb-3 font-medium">Performance</th>
                <th class="pb-3 font-medium">Last Updated</th>
                <th class="pb-3 font-medium">Actions</th>
              </tr>
            </thead>
            <tbody class="text-sm">
              <tr class="border-b border-slate-100">
                <td class="py-4 font-medium text-slate-900">YOLOv5 Object Detector</td>
                <td class="py-4 text-slate-600">v2.3.1</td>
                <td class="py-4"><span class="px-2 py-1 bg-green-100 text-green-700 text-xs font-semibold rounded">Production</span></td>
                <td class="py-4 text-green-600 font-medium">mAP: 0.89</td>
                <td class="py-4 text-slate-500">2 days ago</td>
                <td class="py-4">
                  <button class="text-violet-600 hover:text-violet-800 font-medium">Manage</button>
                </td>
              </tr>
              <tr class="border-b border-slate-100">
                <td class="py-4 font-medium text-slate-900">GPT-2 Text Generator</td>
                <td class="py-4 text-slate-600">v1.0.0</td>
                <td class="py-4"><span class="px-2 py-1 bg-blue-100 text-blue-700 text-xs font-semibold rounded">Staging</span></td>
                <td class="py-4 text-green-600 font-medium">Perplexity: 23.4</td>
                <td class="py-4 text-slate-500">1 week ago</td>
                <td class="py-4">
                  <button class="text-violet-600 hover:text-violet-800 font-medium">Manage</button>
                </td>
              </tr>
              <tr>
                <td class="py-4 font-medium text-slate-900">XGBoost Classifier</td>
                <td class="py-4 text-slate-600">v3.1.0</td>
                <td class="py-4"><span class="px-2 py-1 bg-gray-100 text-gray-700 text-xs font-semibold rounded">Archived</span></td>
                <td class="py-4 text-green-600 font-medium">AUC: 0.95</td>
                <td class="py-4 text-slate-500">1 month ago</td>
                <td class="py-4">
                  <button class="text-violet-600 hover:text-violet-800 font-medium">Manage</button>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    """
  end
end
