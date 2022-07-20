defmodule MazarynWeb.WalletLive.Index do
  use MazarynWeb, :live_view

  alias Mazaryn.Wallet
  alias Wallet.Client

  def mount(_session, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_event("create_wallet", %Wallet{} = wallet, socket) do
    add_wallet(socket, wallet)
  end

  defp add_wallet(socket, wallet) do
    case Wallet.Client.create_wallet(
           wallet.name,
           wallet.password
         ) do
      {:ok, _wallet} ->
        {:noreply, assign(socket, info: "wallet created!")}

      {:error, message} ->
        {:noreply, assign(socket, error: message)}
    end
  end
end
