defmodule MazarynWeb.ChannelLive.New do
  use MazarynWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="max-w-2xl mx-auto p-6">
      <h1 class="text-3xl font-bold text-gray-900 mb-8">Create New Channel</h1>
      <p class="text-gray-600">Use the create buttons in the Groups page to create channels.</p>
    </div>
    """
  end
end
