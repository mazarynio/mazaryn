defmodule MazarynWeb.AptosWalletLive.Show do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Mazaryn.AptosWallets

  @impl true
  def mount(%{"wallet_id" => wallet_id} = _params, %{"session_uuid" => session_uuid} = _session, socket) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        case AptosWallets.get_wallet(wallet_id) do
          {:ok, wallet} ->
            transactions =
              case AptosWallets.get_wallet_transactions(wallet_id) do
                {:ok, t} -> t
                _ -> []
              end

            nft_collections =
              case AptosWallets.get_wallet_nft_collections(wallet_id) do
                {:ok, c} -> c
                _ -> []
              end

            nft_tokens =
              case AptosWallets.get_wallet_nft_tokens(wallet_id) do
                {:ok, t} -> t
                _ -> []
              end

            fungible_assets =
              case AptosWallets.get_wallet_fungible_assets(wallet_id) do
                {:ok, fa} -> fa
                _ -> []
              end

            coin_balances =
              case AptosWallets.get_wallet_coin_balances(wallet_id) do
                {:ok, b} -> b
                _ -> []
              end

            stakes =
              case AptosWallets.get_wallet_stakes(wallet_id) do
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
             |> assign(:nft_collections, nft_collections)
             |> assign(:nft_tokens, nft_tokens)
             |> assign(:fungible_assets, fungible_assets)
             |> assign(:coin_balances, coin_balances)
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
    case AptosWallets.update_wallet_label(socket.assigns.wallet_id, label) do
      {:ok, _} ->
        {:ok, wallet} = AptosWallets.get_wallet(socket.assigns.wallet_id)

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
    case AptosWallets.set_primary_wallet(socket.assigns.user.id, socket.assigns.wallet_id) do
      {:ok, _} ->
        {:ok, wallet} = AptosWallets.get_wallet(socket.assigns.wallet_id)

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
      case AptosWallets.get_wallet_transactions(socket.assigns.wallet_id) do
        {:ok, t} -> t
        _ -> []
      end

    {:noreply, assign(socket, :transactions, transactions)}
  end

  @impl true
  def handle_event("refresh_tokens", _params, socket) do
    fungible_assets =
      case AptosWallets.get_wallet_fungible_assets(socket.assigns.wallet_id) do
        {:ok, fa} -> fa
        _ -> []
      end

    coin_balances =
      case AptosWallets.get_wallet_coin_balances(socket.assigns.wallet_id) do
        {:ok, b} -> b
        _ -> []
      end

    {:noreply,
     socket
     |> assign(:fungible_assets, fungible_assets)
     |> assign(:coin_balances, coin_balances)}
  end

  @impl true
  def handle_event("refresh_nfts", _params, socket) do
    nft_collections =
      case AptosWallets.get_wallet_nft_collections(socket.assigns.wallet_id) do
        {:ok, c} -> c
        _ -> []
      end

    nft_tokens =
      case AptosWallets.get_wallet_nft_tokens(socket.assigns.wallet_id) do
        {:ok, t} -> t
        _ -> []
      end

    {:noreply,
     socket
     |> assign(:nft_collections, nft_collections)
     |> assign(:nft_tokens, nft_tokens)}
  end

  @impl true
  def handle_event("refresh_stakes", _params, socket) do
    stakes =
      case AptosWallets.get_wallet_stakes(socket.assigns.wallet_id) do
        {:ok, s} -> s
        _ -> []
      end

    {:noreply, assign(socket, :stakes, stakes)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  def truncate_addr(nil), do: "—"
  def truncate_addr(:undefined), do: "—"
  def truncate_addr(addr) do
    s = to_string(addr)
    if String.length(s) > 16,
      do: String.slice(s, 0, 10) <> "...." <> String.slice(s, -8, 8),
      else: s
  end

  def format_octas(nil), do: "0"
  def format_octas(:undefined), do: "0"
  def format_octas(octas) do
    case Integer.parse(to_string(octas)) do
      {n, _} -> :erlang.float_to_binary(n / 100_000_000, decimals: 8)
      :error -> "0"
    end
  end

  def format_datetime(nil), do: "—"
  def format_datetime(:undefined), do: "—"
  def format_datetime(ts) when is_integer(ts) do
    case DateTime.from_unix(div(ts, 1000)) do
      {:ok, dt} -> Calendar.strftime(dt, "%Y-%m-%d %H:%M UTC")
      _ -> to_string(ts)
    end
  end
  def format_datetime(_), do: "—"

  def tx_status_class("success"), do: "bg-green-500/10 border border-green-500/20 text-green-400"
  def tx_status_class("pending"), do: "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
  def tx_status_class("failed"),  do: "bg-red-500/10 border border-red-500/20 text-red-400"
  def tx_status_class(_),         do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"

  def stake_status_class("active"),  do: "bg-green-500/10 border border-green-500/20 text-green-400"
  def stake_status_class("pending"), do: "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
  def stake_status_class(_),         do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"

  def tx_type_icon("apt_transfer"),   do: "↗"
  def tx_type_icon("coin_transfer"),  do: "🪙"
  def tx_type_icon("nft_mint"),       do: "🎨"
  def tx_type_icon("nft_transfer"),   do: "🖼"
  def tx_type_icon("stake_add"),      do: "🔒"
  def tx_type_icon("stake_unlock"),   do: "🔓"
  def tx_type_icon("stake_withdraw"), do: "↩"
  def tx_type_icon(_),                do: "·"
end
