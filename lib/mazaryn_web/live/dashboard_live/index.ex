defmodule DashboardLive.Index do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper
  import Logger

  alias Account.Users
  alias Wallet.Client
  alias Dashboard.Wallet
  alias Mazaryn.Wallet

  # case reload dashboard page
  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info(user_id: user_id)
    {:ok, do_mount(user_id, socket)}
  end

  defp do_mount(user_id, socket) do
    wallet_changeset = Wallet.changeset(%Wallet{})

    socket
    |> assign(wallet_changeset: wallet_changeset)
    |> assign(posts: get_wallet())
  end

  def get_wallet, do: Wallet.Client.get_wallets()
end
