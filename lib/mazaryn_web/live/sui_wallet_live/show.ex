defmodule MazarynWeb.SuiWalletLive.Show do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Mazaryn.SuiWallets

  @impl true
  def mount(%{"wallet_id" => wallet_id} = _params, %{"session_uuid" => session_uuid} = _session, socket) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        case SuiWallets.get_wallet(wallet_id) do
          {:ok, wallet} ->
            transactions =
              case SuiWallets.get_wallet_transactions(wallet_id) do
                {:ok, t} -> t
                _ -> []
              end

            objects =
              case SuiWallets.get_wallet_objects(wallet_id) do
                {:ok, o} -> o
                _ -> []
              end

            nfts =
              case SuiWallets.get_wallet_nfts(wallet_id) do
                {:ok, n} -> n
                _ -> []
              end

            coin_balances =
              case SuiWallets.get_wallet_coin_balances(wallet_id) do
                {:ok, b} -> b
                _ -> []
              end

            stakes =
              case SuiWallets.get_wallet_stakes(wallet_id) do
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
             |> assign(:objects, objects)
             |> assign(:nfts, nfts)
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
    case SuiWallets.update_wallet_label(socket.assigns.wallet_id, label) do
      {:ok, _} ->
        {:ok, wallet} = SuiWallets.get_wallet(socket.assigns.wallet_id)

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
    case SuiWallets.set_primary_wallet(socket.assigns.user.id, socket.assigns.wallet_id) do
      {:ok, _} ->
        {:ok, wallet} = SuiWallets.get_wallet(socket.assigns.wallet_id)

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
      case SuiWallets.get_wallet_transactions(socket.assigns.wallet_id) do
        {:ok, t} -> t
        _ -> []
      end

    {:noreply, assign(socket, :transactions, transactions)}
  end

  @impl true
  def handle_event("refresh_balances", _params, socket) do
    coin_balances =
      case SuiWallets.get_wallet_coin_balances(socket.assigns.wallet_id) do
        {:ok, b} -> b
        _ -> []
      end

    {:noreply, assign(socket, :coin_balances, coin_balances)}
  end

  @impl true
  def handle_event("refresh_objects", _params, socket) do
    objects =
      case SuiWallets.get_wallet_objects(socket.assigns.wallet_id) do
        {:ok, o} -> o
        _ -> []
      end

    nfts =
      case SuiWallets.get_wallet_nfts(socket.assigns.wallet_id) do
        {:ok, n} -> n
        _ -> []
      end

    {:noreply,
     socket
     |> assign(:objects, objects)
     |> assign(:nfts, nfts)}
  end

  @impl true
  def handle_event("refresh_stakes", _params, socket) do
    stakes =
      case SuiWallets.get_wallet_stakes(socket.assigns.wallet_id) do
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

  def format_mist(nil), do: "0"
  def format_mist(:undefined), do: "0"
  def format_mist(mist) do
    case Integer.parse(to_string(mist)) do
      {n, _} -> :erlang.float_to_binary(n / 1_000_000_000, decimals: 9)
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

  def tx_status_class(s) do
    case to_string(s) do
      "success"   -> "bg-green-500/10 border border-green-500/20 text-green-400"
      "confirmed" -> "bg-green-500/10 border border-green-500/20 text-green-400"
      "pending"   -> "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
      "failure"   -> "bg-red-500/10 border border-red-500/20 text-red-400"
      _           -> "bg-slate-700/50 border border-slate-600/30 text-slate-400"
    end
  end

  def stake_status_class(s) do
    case to_string(s) do
      "active"   -> "bg-green-500/10 border border-green-500/20 text-green-400"
      "pending"  -> "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
      "unstaked" -> "bg-slate-700/50 border border-slate-600/30 text-slate-400"
      _          -> "bg-slate-700/50 border border-slate-600/30 text-slate-400"
    end
  end

  def tx_type_icon(t) do
    case to_string(t) do
      "sui_transfer"   -> "↗"
      "batch_transfer" -> "↗↗"
      "move_call"      -> "⚙"
      "publish"        -> "📦"
      "upgrade"        -> "⬆"
      "split_coin"     -> "✂"
      "merge_coins"    -> "⊕"
      "stake"          -> "🔒"
      "unstake"        -> "🔓"
      "nft_mint"       -> "🎨"
      "nft_transfer"   -> "🖼"
      _                -> "·"
    end
  end

  def coin_type_short(nil), do: "?"
  def coin_type_short(ct) do
    ct
    |> to_string()
    |> String.split("::")
    |> List.last()
    |> then(&(&1 || "?"))
  end
end
