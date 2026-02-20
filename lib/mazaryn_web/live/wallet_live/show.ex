defmodule MazarynWeb.WalletLive.Show do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Mazaryn.SolanaWallets

  @impl true
  def mount(%{"wallet_id" => wallet_id} = _params, %{"session_uuid" => session_uuid} = _session, socket) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        case SolanaWallets.get_wallet(wallet_id) do
          {:ok, wallet} ->
            transactions =
              case SolanaWallets.get_wallet_transactions(wallet_id) do
                {:ok, t} -> t
                _ -> []
              end

            tokens =
              case SolanaWallets.get_wallet_token_accounts(wallet_id) do
                {:ok, t} -> t
                _ -> []
              end

            nfts =
              case SolanaWallets.get_wallet_nfts(wallet_id) do
                {:ok, n} -> n
                _ -> []
              end

            stakes =
              case SolanaWallets.get_wallet_stakes(wallet_id) do
                {:ok, s} -> s
                _ -> []
              end

            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:transactions, transactions)
             |> assign(:tokens, tokens)
             |> assign(:nfts, nfts)
             |> assign(:stakes, stakes)
             |> assign(:active_tab, "overview")
             |> assign(:show_update_label_modal, false)
             |> assign(:new_label, "")}

          {:error, _} ->
            {:ok,
             socket
             |> put_flash(:error, "Wallet not found")
             |> redirect(to: "/en/wallet")}
        end

      {:error, _} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, :active_tab, tab)}
  end

  @impl true
  def handle_event("open_update_label_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_update_label_modal, true)
     |> assign(:new_label, to_string(socket.assigns.wallet.label || ""))}
  end

  @impl true
  def handle_event("close_update_label_modal", _params, socket) do
    {:noreply, assign(socket, :show_update_label_modal, false)}
  end

  @impl true
  def handle_event("validate_label", %{"label" => label}, socket) do
    {:noreply, assign(socket, :new_label, label)}
  end

  @impl true
  def handle_event("save_label", %{"label" => label}, socket) do
    case SolanaWallets.update_wallet_label(socket.assigns.wallet_id, label) do
      :ok ->
        {:ok, wallet} = SolanaWallets.get_wallet(socket.assigns.wallet_id)

        {:noreply,
         socket
         |> assign(:wallet, wallet)
         |> assign(:show_update_label_modal, false)
         |> put_flash(:info, "Wallet label updated")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to update label")}
    end
  end

  @impl true
  def handle_event("set_primary", _params, socket) do
    case SolanaWallets.set_primary_wallet(socket.assigns.user.id, socket.assigns.wallet_id) do
      :ok ->
        {:ok, wallet} = SolanaWallets.get_wallet(socket.assigns.wallet_id)

        {:noreply,
         socket
         |> assign(:wallet, wallet)
         |> put_flash(:info, "Set as primary wallet")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to set primary wallet")}
    end
  end

  @impl true
  def handle_event("refresh_transactions", _params, socket) do
    transactions =
      case SolanaWallets.get_wallet_transactions(socket.assigns.wallet_id) do
        {:ok, t} -> t
        _ -> []
      end

    {:noreply, assign(socket, :transactions, transactions)}
  end

  @impl true
  def handle_event("refresh_tokens", _params, socket) do
    tokens =
      case SolanaWallets.get_wallet_token_accounts(socket.assigns.wallet_id) do
        {:ok, t} -> t
        _ -> []
      end

    {:noreply, assign(socket, :tokens, tokens)}
  end

  @impl true
  def handle_event("refresh_nfts", _params, socket) do
    nfts =
      case SolanaWallets.get_wallet_nfts(socket.assigns.wallet_id) do
        {:ok, n} -> n
        _ -> []
      end

    {:noreply, assign(socket, :nfts, nfts)}
  end

  @impl true
  def handle_event("refresh_stakes", _params, socket) do
    stakes =
      case SolanaWallets.get_wallet_stakes(socket.assigns.wallet_id) do
        {:ok, s} -> s
        _ -> []
      end

    {:noreply, assign(socket, :stakes, stakes)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  def truncate_key(nil), do: "—"
  def truncate_key(:undefined), do: "—"
  def truncate_key(key) do
    s = to_string(key)
    if String.length(s) > 16,
      do: String.slice(s, 0, 8) <> "...." <> String.slice(s, -8, 8),
      else: s
  end

  def format_datetime(nil), do: "—"
  def format_datetime(:undefined), do: "—"
  def format_datetime({{y, m, d}, {h, mn, s}}) do
    :io_lib.format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [y, m, d, h, mn, s])
    |> IO.iodata_to_binary()
  end
  def format_datetime(%NaiveDateTime{} = dt), do: NaiveDateTime.to_string(dt) |> String.slice(0, 16)
  def format_datetime(%DateTime{} = dt), do: DateTime.to_string(dt) |> String.slice(0, 16)
  def format_datetime(_), do: "—"

  def tx_status_class("confirmed"), do: "bg-green-500/10 border border-green-500/20 text-green-400"
  def tx_status_class("pending"),   do: "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
  def tx_status_class("failed"),    do: "bg-red-500/10 border border-red-500/20 text-red-400"
  def tx_status_class(_),           do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"

  def tx_type_icon("transfer"), do: "→"
  def tx_type_icon("stake"),    do: "⚡"
  def tx_type_icon("unstake"),  do: "↩"
  def tx_type_icon("airdrop"),  do: "🎁"
  def tx_type_icon("nft"),      do: "🖼"
  def tx_type_icon(_),          do: "·"
end
