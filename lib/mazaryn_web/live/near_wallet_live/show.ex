defmodule MazarynWeb.NearWalletLive.Show do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Mazaryn.NearWallets

  @impl true
  def mount(
        %{"wallet_id" => wallet_id} = _params,
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        case NearWallets.get_wallet(wallet_id) do
          {:ok, wallet} ->
            transactions =
              case NearWallets.get_wallet_transactions(wallet_id) do
                {:ok, t} -> t
                _ -> []
              end

            access_keys =
              case NearWallets.get_wallet_access_keys(wallet_id) do
                {:ok, k} -> k
                _ -> []
              end

            stakes =
              case NearWallets.get_wallet_stakes(wallet_id) do
                {:ok, s} -> s
                _ -> []
              end

            social_posts =
              case NearWallets.get_wallet_social_posts(wallet_id) do
                {:ok, p} -> p
                _ -> []
              end

            implicit_accounts =
              case NearWallets.get_wallet_implicit_accounts(wallet_id) do
                {:ok, a} -> a
                _ -> []
              end

            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:transactions, transactions)
             |> assign(:access_keys, access_keys)
             |> assign(:stakes, stakes)
             |> assign(:social_posts, social_posts)
             |> assign(:implicit_accounts, implicit_accounts)
             |> assign(:active_tab, "overview")
             |> assign(:show_update_label_modal, false)
             |> assign(:new_label, "")}

          {:error, _} ->
            {:ok,
             socket
             |> put_flash(:error, "NEAR wallet not found")
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
    case NearWallets.update_wallet_label(socket.assigns.wallet_id, label) do
      :ok ->
        {:ok, wallet} = NearWallets.get_wallet(socket.assigns.wallet_id)

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
    case NearWallets.set_primary_wallet(socket.assigns.user.id, socket.assigns.wallet_id) do
      :ok ->
        {:ok, wallet} = NearWallets.get_wallet(socket.assigns.wallet_id)

        {:noreply,
         socket
         |> assign(:wallet, wallet)
         |> put_flash(:info, "Set as primary NEAR wallet")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to set primary wallet")}
    end
  end

  @impl true
  def handle_event("refresh_transactions", _params, socket) do
    transactions =
      case NearWallets.get_wallet_transactions(socket.assigns.wallet_id) do
        {:ok, t} -> t
        _ -> []
      end

    {:noreply, assign(socket, :transactions, transactions)}
  end

  @impl true
  def handle_event("refresh_access_keys", _params, socket) do
    access_keys =
      case NearWallets.get_wallet_access_keys(socket.assigns.wallet_id) do
        {:ok, k} -> k
        _ -> []
      end

    {:noreply, assign(socket, :access_keys, access_keys)}
  end

  @impl true
  def handle_event("refresh_stakes", _params, socket) do
    stakes =
      case NearWallets.get_wallet_stakes(socket.assigns.wallet_id) do
        {:ok, s} -> s
        _ -> []
      end

    {:noreply, assign(socket, :stakes, stakes)}
  end

  @impl true
  def handle_event("refresh_social_posts", _params, socket) do
    social_posts =
      case NearWallets.get_wallet_social_posts(socket.assigns.wallet_id) do
        {:ok, p} -> p
        _ -> []
      end

    {:noreply, assign(socket, :social_posts, social_posts)}
  end

  @impl true
  def handle_event("refresh_implicit_accounts", _params, socket) do
    implicit_accounts =
      case NearWallets.get_wallet_implicit_accounts(socket.assigns.wallet_id) do
        {:ok, a} -> a
        _ -> []
      end

    {:noreply, assign(socket, :implicit_accounts, implicit_accounts)}
  end

  @impl true
  def handle_event("delete_access_key", %{"key_id" => key_id}, socket) do
    case NearWallets.delete_access_key(key_id) do
      :ok ->
        access_keys =
          case NearWallets.get_wallet_access_keys(socket.assigns.wallet_id) do
            {:ok, k} -> k
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:access_keys, access_keys)
         |> put_flash(:info, "Access key deleted")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to delete access key")}
    end
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

  def tx_status_class("success"), do: "bg-green-500/10 border border-green-500/20 text-green-400"
  def tx_status_class("pending"), do: "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
  def tx_status_class("failure"), do: "bg-red-500/10 border border-red-500/20 text-red-400"
  def tx_status_class(_),         do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"

  def tx_type_icon("transfer"),     do: "→"
  def tx_type_icon("stake"),        do: "⚡"
  def tx_type_icon("unstake"),      do: "↩"
  def tx_type_icon("function_call"), do: "⚙"
  def tx_type_icon("social_post"),  do: "💬"
  def tx_type_icon("deploy"),       do: "📦"
  def tx_type_icon(_),              do: "·"

  def stake_status_class("active"),     do: "bg-green-500/10 border border-green-500/20 text-green-400"
  def stake_status_class("unstaked"),   do: "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
  def stake_status_class("withdrawn"),  do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"
  def stake_status_class(_),            do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"

  def key_type_class("FullAccess"),    do: "bg-red-500/10 border border-red-500/20 text-red-400"
  def key_type_class("FunctionCall"),  do: "bg-blue-500/10 border border-blue-500/20 text-blue-400"
  def key_type_class(_),               do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"
end
