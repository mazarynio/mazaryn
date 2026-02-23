defmodule MazarynWeb.NearWalletLive.Transactions do
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

            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:transactions, transactions)
             |> assign(:filtered_transactions, transactions)
             |> assign(:search, "")
             |> assign(:filter_type, "all")
             |> assign(:filter_status, "all")
             |> assign(:selected_tx, nil)}

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
  def handle_event("search", %{"value" => search}, socket) do
    filtered = apply_filters(socket.assigns.transactions, search, socket.assigns.filter_type, socket.assigns.filter_status)
    {:noreply, socket |> assign(:search, search) |> assign(:filtered_transactions, filtered)}
  end

  @impl true
  def handle_event("filter_type", %{"value" => type}, socket) do
    filtered = apply_filters(socket.assigns.transactions, socket.assigns.search, type, socket.assigns.filter_status)
    {:noreply, socket |> assign(:filter_type, type) |> assign(:filtered_transactions, filtered)}
  end

  @impl true
  def handle_event("filter_status", %{"value" => status}, socket) do
    filtered = apply_filters(socket.assigns.transactions, socket.assigns.search, socket.assigns.filter_type, status)
    {:noreply, socket |> assign(:filter_status, status) |> assign(:filtered_transactions, filtered)}
  end

  @impl true
  def handle_event("select_tx", %{"tx_id" => tx_id}, socket) do
    tx = Enum.find(socket.assigns.transactions, &(to_string(&1.tx_id) == tx_id))
    {:noreply, assign(socket, :selected_tx, tx)}
  end

  @impl true
  def handle_event("close_tx_modal", _params, socket) do
    {:noreply, assign(socket, :selected_tx, nil)}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    transactions =
      case NearWallets.get_wallet_transactions(socket.assigns.wallet_id) do
        {:ok, t} -> t
        _ -> []
      end

    filtered = apply_filters(transactions, socket.assigns.search, socket.assigns.filter_type, socket.assigns.filter_status)

    {:noreply,
     socket
     |> assign(:transactions, transactions)
     |> assign(:filtered_transactions, filtered)
     |> put_flash(:info, "Transactions refreshed")}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  defp apply_filters(transactions, search, type, status) do
    transactions
    |> filter_by_search(search)
    |> filter_by_type(type)
    |> filter_by_status(status)
  end

  defp filter_by_search(transactions, ""), do: transactions
  defp filter_by_search(transactions, search) do
    s = String.downcase(search)
    Enum.filter(transactions, fn tx ->
      String.contains?(String.downcase(to_string(tx.transaction_hash || "")), s) or
        String.contains?(String.downcase(to_string(tx.receiver_id || "")), s) or
        String.contains?(String.downcase(to_string(tx.from_account_id || "")), s) or
        String.contains?(String.downcase(to_string(tx.tx_type || "")), s)
    end)
  end

  defp filter_by_type(transactions, "all"), do: transactions
  defp filter_by_type(transactions, type) do
    Enum.filter(transactions, fn tx -> to_string(tx.tx_type || "") == type end)
  end

  defp filter_by_status(transactions, "all"), do: transactions
  defp filter_by_status(transactions, status) do
    Enum.filter(transactions, fn tx -> to_string(tx.status || "") == status end)
  end

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

  def tx_type_icon("transfer"),      do: "→"
  def tx_type_icon("stake"),         do: "⚡"
  def tx_type_icon("unstake"),       do: "↩"
  def tx_type_icon("function_call"), do: "⚙"
  def tx_type_icon("social_post"),   do: "💬"
  def tx_type_icon("deploy"),        do: "📦"
  def tx_type_icon(_),               do: "·"

  def tx_type_color("transfer"),      do: "bg-blue-500/20 text-blue-400"
  def tx_type_color("stake"),         do: "bg-green-500/20 text-green-400"
  def tx_type_color("unstake"),       do: "bg-yellow-500/20 text-yellow-400"
  def tx_type_color("function_call"), do: "bg-purple-500/20 text-purple-400"
  def tx_type_color("social_post"),   do: "bg-pink-500/20 text-pink-400"
  def tx_type_color("deploy"),        do: "bg-orange-500/20 text-orange-400"
  def tx_type_color(_),               do: "bg-slate-700/50 text-slate-400"
end
