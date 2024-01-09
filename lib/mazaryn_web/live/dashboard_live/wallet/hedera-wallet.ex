defmodule MazarynWeb.DashboardLive.Wallet.HederaWallet do
  use MazarynWeb, :live_view

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div></div>
    """
  end
end
