defmodule MazarynWeb.AiLive.NotebooksComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    code1 = """
    import pandas as pd
    import matplotlib.pyplot as plt
    from sklearn.preprocessing import StandardScaler
    # Load and preprocess data
    df = pd.read_csv('data.csv')
    print(f"Dataset shape: {df.shape}")
    # Output: Dataset shape: (10000, 25)
    """

    code2 = """
    library(tidyverse)
    library(ggplot2)
    # Perform t-test
    result <- t.test(group_a, group_b)
    print(paste("p-value:", result$p.value))
    # p-value: 0.0023
    """

    code3 = """
    import torch
    from efficientnet_pytorch import EfficientNet
    model = EfficientNet.from_pretrained('efficientnet-b7')
    # Fine-tuned with custom head
    # Accuracy: 0.987 on validation set
    """

    code4 = """
    using LinearAlgebra, BenchmarkTools
    A = rand(1000, 1000)
    @btime A * A
    # 12.5 ms (10x faster than NumPy)
    """

    ~H"""
    <div class="space-y-6">
      <div class="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div class="bg-white rounded-2xl border border-slate-200 hover:border-violet-300 hover:shadow-xl transition-all duration-300 overflow-hidden">
          <div class="p-6">
            <div class="flex items-start justify-between mb-4">
              <div class="flex items-center space-x-3">
                <div class="w-10 h-10 bg-gradient-to-br from-yellow-400 to-orange-500 rounded-lg flex items-center justify-center">
                  <svg class="w-5 h-5 text-white" fill="currentColor" viewBox="0 0 20 20">
                    <path d="M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z" />
                  </svg>
                </div>
                <div>
                  <div class="flex items-center space-x-2">
                    <span class="px-2 py-0.5 bg-yellow-100 text-yellow-700 text-xs font-bold rounded">Python</span>
                    <span class="text-xs text-slate-400">v3.10</span>
                  </div>
                </div>
              </div>
              <div class="flex items-center space-x-2">
                <button class="p-1.5 text-slate-400 hover:text-violet-600 transition-colors">
                  <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8.684 13.342C8.886 12.938 9 12.482 9 12c0-.482-.114-.938-.316-1.342m0 2.684a3 3 0 110-2.684m0 2.684l6.632 3.316m-6.632-6l6.632-3.316m0 0a3 3 0 105.367-2.684 3 3 0 00-5.367 2.684zm0 9.316a3 3 0 105.368 2.684 3 3 0 00-5.368-2.684z" />
                  </svg>
                </button>
                <button class="p-1.5 text-slate-400 hover:text-violet-600 transition-colors">
                  <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
                  </svg>
                </button>
                <button class="p-1.5 text-slate-400 hover:text-slate-600 transition-colors">
                  <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 5v.01M12 12v.01M12 19v.01M12 6a1 1 0 110-2 1 1 0 010 2zm0 7a1 1 0 110-2 1 1 0 010 2zm0 7a1 1 0 110-2 1 1 0 010 2z" />
                  </svg>
                </button>
              </div>
            </div>
            <h3 class="text-xl font-bold text-slate-900 mb-2">Complete EDA Pipeline for Tabular Data</h3>
            <p class="text-slate-600 text-sm mb-4">
              Comprehensive exploratory data analysis notebook with automated visualizations, statistical tests, and feature engineering techniques.
            </p>
            <div class="bg-slate-900 rounded-lg p-4 mb-4 overflow-hidden">
              <div class="flex items-center space-x-2 mb-3">
                <div class="w-3 h-3 bg-red-500 rounded-full"></div>
                <div class="w-3 h-3 bg-yellow-500 rounded-full"></div>
                <div class="w-3 h-3 bg-green-500 rounded-full"></div>
                <span class="text-xs text-slate-400 ml-2">notebook.py</span>
              </div>
              <pre class="text-xs text-green-400 font-mono overflow-x-auto"><code><%= raw(code1) %></code></pre>
            </div>
            <div class="flex items-center space-x-6 mb-4">
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
                  <path d="M3.172 5.172a4 4 0 015.656 0L10 6.343l1.172-1.171a4 4 0 115.656 5.656L10 17.657l-6.828-6.829a4 4 0 010-5.656z" />
                </svg>
                <span class="text-xs font-medium">342</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" />
                </svg>
                <span class="text-xs font-medium">89 forks</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z" />
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z" />
                </svg>
                <span class="text-xs font-medium">1.2K views</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
                </svg>
                <span class="text-xs font-medium">~5 min</span>
              </div>
            </div>
            <div class="flex flex-wrap gap-2 mb-4">
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">eda</span>
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">data-analysis</span>
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">visualization</span>
            </div>
            <div class="flex items-center justify-between pt-4 border-t border-slate-100">
              <div class="flex items-center space-x-2">
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full" alt="User avatar" />
                <span class="text-sm text-slate-600">@data_scientist_pro</span>
              </div>
              <button class="px-4 py-2 bg-violet-600 text-white text-sm font-medium rounded-lg hover:bg-violet-700 transition-colors">
                Open Notebook
              </button>
            </div>
          </div>
        </div>
        <div class="bg-white rounded-2xl border border-slate-200 hover:border-blue-300 hover:shadow-xl transition-all duration-300 overflow-hidden">
          <div class="p-6">
            <div class="flex items-start justify-between mb-4">
              <div class="flex items-center space-x-3">
                <div class="w-10 h-10 bg-gradient-to-br from-blue-500 to-indigo-600 rounded-lg flex items-center justify-center">
                  <svg class="w-5 h-5 text-white" fill="currentColor" viewBox="0 0 20 20">
                    <path d="M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z" />
                  </svg>
                </div>
                <div>
                  <div class="flex items-center space-x-2">
                    <span class="px-2 py-0.5 bg-blue-100 text-blue-700 text-xs font-bold rounded">R</span>
                    <span class="text-xs text-slate-400">v4.3</span>
                  </div>
                </div>
              </div>
              <div class="flex items-center space-x-2">
                <button class="p-1.5 text-slate-400 hover:text-violet-600 transition-colors">
                  <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8.684 13.342C8.886 12.938 9 12.482 9 12c0-.482-.114-.938-.316-1.342m0 2.684a3 3 0 110-2.684m0 2.684l6.632 3.316m-6.632-6l6.632-3.316m0 0a3 3 0 105.367-2.684 3 3 0 00-5.367 2.684zm0 9.316a3 3 0 105.368 2.684 3 3 0 00-5.368-2.684z" />
                  </svg>
                </button>
                <button class="p-1.5 text-slate-400 hover:text-violet-600 transition-colors">
                  <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 5a2 2 0 012-2h10a2 2 0 012 2v16l-7-3.5L5 21V5z" />
                  </svg>
                </button>
              </div>
            </div>
            <h3 class="text-xl font-bold text-slate-900 mb-2">Statistical Hypothesis Testing Guide</h3>
            <p class="text-slate-600 text-sm mb-4">
              Learn to perform t-tests, ANOVA, chi-square tests, and more with real-world examples and interpretations.
            </p>
            <div class="bg-slate-900 rounded-lg p-4 mb-4 overflow-hidden">
              <div class="flex items-center space-x-2 mb-3">
                <div class="w-3 h-3 bg-red-500 rounded-full"></div>
                <div class="w-3 h-3 bg-yellow-500 rounded-full"></div>
                <div class="w-3 h-3 bg-green-500 rounded-full"></div>
                <span class="text-xs text-slate-400 ml-2">analysis.R</span>
              </div>
              <pre class="text-xs text-green-400 font-mono overflow-x-auto"><code><%= raw(code2) %></code></pre>
            </div>
            <div class="flex items-center space-x-6 mb-4">
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
                  <path d="M3.172 5.172a4 4 0 015.656 0L10 6.343l1.172-1.171a4 4 0 115.656 5.656L10 17.657l-6.828-6.829a4 4 0 010-5.656z" />
                </svg>
                <span class="text-xs font-medium">218</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" />
                </svg>
                <span class="text-xs font-medium">45 forks</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
                </svg>
                <span class="text-xs font-medium">~8 min</span>
              </div>
            </div>
            <div class="flex flex-wrap gap-2 mb-4">
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">statistics</span>
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">tutorial</span>
            </div>
            <div class="flex items-center justify-between pt-4 border-t border-slate-100">
              <div class="flex items-center space-x-2">
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full" alt="User avatar" />
                <span class="text-sm text-slate-600">@stats_guru</span>
              </div>
              <button class="px-4 py-2 bg-blue-600 text-white text-sm font-medium rounded-lg hover:bg-blue-700 transition-colors">
                Open Notebook
              </button>
            </div>
          </div>
        </div>
        <div class="bg-white rounded-2xl border border-slate-200 hover:border-green-300 hover:shadow-xl transition-all duration-300 overflow-hidden">
          <div class="relative">
            <div class="absolute top-4 right-4 z-10">
              <span class="px-3 py-1 bg-green-500 text-white text-xs font-bold rounded-full shadow-lg">
                Competition Winner
              </span>
            </div>
            <div class="p-6">
              <div class="flex items-start justify-between mb-4">
                <div class="flex items-center space-x-3">
                  <div class="w-10 h-10 bg-gradient-to-br from-green-500 to-emerald-600 rounded-lg flex items-center justify-center">
                    <svg class="w-5 h-5 text-white" fill="currentColor" viewBox="0 0 20 20">
                      <path d="M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z" />
                    </svg>
                  </div>
                  <div>
                    <div class="flex items-center space-x-2">
                      <span class="px-2 py-0.5 bg-yellow-100 text-yellow-700 text-xs font-bold rounded">Python</span>
                      <span class="text-xs text-slate-400">PyTorch</span>
                    </div>
                  </div>
                </div>
              </div>
              <h3 class="text-xl font-bold text-slate-900 mb-2">1st Place: Image Classification Solution</h3>
              <p class="text-slate-600 text-sm mb-4">
                Gold medal solution using EfficientNet with advanced augmentation and ensemble techniques. Achieved 0.987 accuracy.
              </p>
              <div class="bg-slate-900 rounded-lg p-4 mb-4 overflow-hidden">
                <div class="flex items-center space-x-2 mb-3">
                  <div class="w-3 h-3 bg-red-500 rounded-full"></div>
                  <div class="w-3 h-3 bg-yellow-500 rounded-full"></div>
                  <div class="w-3 h-3 bg-green-500 rounded-full"></div>
                  <span class="text-xs text-slate-400 ml-2">model.py</span>
                </div>
                <pre class="text-xs text-green-400 font-mono overflow-x-auto"><code><%= raw(code3) %></code></pre>
              </div>
              <div class="flex items-center space-x-6 mb-4">
                <div class="flex items-center space-x-1 text-slate-500">
                  <svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
                    <path d="M3.172 5.172a4 4 0 015.656 0L10 6.343l1.172-1.171a4 4 0 115.656 5.656L10 17.657l-6.828-6.829a4 4 0 010-5.656z" />
                  </svg>
                  <span class="text-xs font-medium">892</span>
                </div>
                <div class="flex items-center space-x-1 text-slate-500">
                  <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" />
                  </svg>
                  <span class="text-xs font-medium">234 forks</span>
                </div>
                <div class="flex items-center space-x-1 text-slate-500">
                  <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
                  </svg>
                  <span class="text-xs font-medium">~45 min</span>
                </div>
              </div>
              <div class="flex flex-wrap gap-2 mb-4">
                <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">deep-learning</span>
                <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">computer-vision</span>
                <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">gold-medal</span>
              </div>
              <div class="flex items-center justify-between pt-4 border-t border-slate-100">
                <div class="flex items-center space-x-2">
                  <img src="/images/default-user.svg" class="w-6 h-6 rounded-full" alt="User avatar" />
                  <span class="text-sm text-slate-600">@ml_champion</span>
                </div>
                <button class="px-4 py-2 bg-green-600 text-white text-sm font-medium rounded-lg hover:bg-green-700 transition-colors">
                  Open Notebook
                </button>
              </div>
            </div>
          </div>
        </div>
        <div class="bg-white rounded-2xl border border-slate-200 hover:border-purple-300 hover:shadow-xl transition-all duration-300 overflow-hidden">
          <div class="p-6">
            <div class="flex items-start justify-between mb-4">
              <div class="flex items-center space-x-3">
                <div class="w-10 h-10 bg-gradient-to-br from-purple-500 to-pink-600 rounded-lg flex items-center justify-center">
                  <svg class="w-5 h-5 text-white" fill="currentColor" viewBox="0 0 20 20">
                    <path d="M2 6a2 2 0 012-2h5l2 2h5a2 2 0 012 2v6a2 2 0 01-2 2H4a2 2 0 01-2-2V6z" />
                  </svg>
                </div>
                <div>
                  <div class="flex items-center space-x-2">
                    <span class="px-2 py-0.5 bg-purple-100 text-purple-700 text-xs font-bold rounded">Julia</span>
                    <span class="text-xs text-slate-400">v1.9</span>
                  </div>
                </div>
              </div>
            </div>
            <h3 class="text-xl font-bold text-slate-900 mb-2">High-Performance Matrix Operations</h3>
            <p class="text-slate-600 text-sm mb-4">
              Leverage Julia's speed for linear algebra operations. Benchmark comparisons with NumPy and optimized implementations.
            </p>
            <div class="bg-slate-900 rounded-lg p-4 mb-4 overflow-hidden">
              <div class="flex items-center space-x-2 mb-3">
                <div class="w-3 h-3 bg-red-500 rounded-full"></div>
                <div class="w-3 h-3 bg-yellow-500 rounded-full"></div>
                <div class="w-3 h-3 bg-green-500 rounded-full"></div>
                <span class="text-xs text-slate-400 ml-2">benchmark.jl</span>
              </div>
              <pre class="text-xs text-green-400 font-mono overflow-x-auto"><code><%= raw(code4) %></code></pre>
            </div>
            <div class="flex items-center space-x-6 mb-4">
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
                  <path d="M3.172 5.172a4 4 0 015.656 0L10 6.343l1.172-1.171a4 4 0 115.656 5.656L10 17.657l-6.828-6.829a4 4 0 010-5.656z" />
                </svg>
                <span class="text-xs font-medium">156</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" />
                </svg>
                <span class="text-xs font-medium">28 forks</span>
              </div>
              <div class="flex items-center space-x-1 text-slate-500">
                <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
                </svg>
                <span class="text-xs font-medium">~3 min</span>
              </div>
            </div>
            <div class="flex flex-wrap gap-2 mb-4">
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">julia</span>
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">performance</span>
              <span class="px-2 py-1 bg-violet-100 text-violet-700 text-xs font-medium rounded-md">linear-algebra</span>
            </div>
            <div class="flex items-center justify-between pt-4 border-t border-slate-100">
              <div class="flex items-center space-x-2">
                <img src="/images/default-user.svg" class="w-6 h-6 rounded-full" alt="User avatar" />
                <span class="text-sm text-slate-600">@julia_enthusiast</span>
              </div>
              <button class="px-4 py-2 bg-purple-600 text-white text-sm font-medium rounded-lg hover:bg-purple-700 transition-colors">
                Open Notebook
              </button>
            </div>
          </div>
        </div>
      </div>
      <div class="flex items-center justify-center">
        <button class="flex items-center space-x-2 px-6 py-3 text-violet-600 font-medium hover:bg-violet-50 rounded-xl transition-colors">
          <span>Load More Notebooks</span>
          <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
          </svg>
        </button>
      </div>
    </div>
    """
  end
end
