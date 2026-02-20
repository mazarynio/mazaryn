defmodule MazarynWeb.WalletLive.Transactions do
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
                {:ok, txs} -> txs
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
             |> assign(:filter_type, "all")
             |> assign(:filter_status, "all")
             |> assign(:search, "")}

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
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("filter_type", %{"value" => type}, socket) do
    filtered = apply_filters(socket.assigns.transactions, type, socket.assigns.filter_status, socket.assigns.search)
    {:noreply, socket |> assign(:filter_type, type) |> assign(:filtered_transactions, filtered)}
  end

  @impl true
  def handle_event("filter_status", %{"value" => status}, socket) do
    filtered = apply_filters(socket.assigns.transactions, socket.assigns.filter_type, status, socket.assigns.search)
    {:noreply, socket |> assign(:filter_status, status) |> assign(:filtered_transactions, filtered)}
  end

  @impl true
  def handle_event("search", %{"value" => search}, socket) do
    filtered = apply_filters(socket.assigns.transactions, socket.assigns.filter_type, socket.assigns.filter_status, search)
    {:noreply, socket |> assign(:search, search) |> assign(:filtered_transactions, filtered)}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    transactions =
      case SolanaWallets.get_wallet_transactions(socket.assigns.wallet_id) do
        {:ok, txs} -> txs
        _ -> []
      end

    filtered = apply_filters(transactions, socket.assigns.filter_type, socket.assigns.filter_status, socket.assigns.search)

    {:noreply,
     socket
     |> assign(:transactions, transactions)
     |> assign(:filtered_transactions, filtered)
     |> put_flash(:info, "Transactions refreshed")}
  end

  defp apply_filters(transactions, type, status, search) do
    transactions
    |> filter_by_type(type)
    |> filter_by_status(status)
    |> filter_by_search(search)
  end

  defp filter_by_type(txs, "all"), do: txs
  defp filter_by_type(txs, type), do: Enum.filter(txs, &(to_string(&1.tx_type) == type))

  defp filter_by_status(txs, "all"), do: txs
  defp filter_by_status(txs, status), do: Enum.filter(txs, &(to_string(&1.status) == status))

  defp filter_by_search(txs, ""), do: txs
  defp filter_by_search(txs, search) do
    s = String.downcase(search)
    Enum.filter(txs, fn tx ->
      String.contains?(String.downcase(to_string(tx.signature)), s) or
      String.contains?(String.downcase(to_string(tx.tx_type)), s)
    end)
  end

  def format_datetime(nil), do: "—"
  def format_datetime(:undefined), do: "—"
  def format_datetime(dt) do
    case dt do
      %NaiveDateTime{} -> NaiveDateTime.to_string(dt) |> String.slice(0, 16)
      %DateTime{} -> DateTime.to_string(dt) |> String.slice(0, 16)
      _ -> "—"
    end
  end

  def truncate_key(nil), do: "—"
  def truncate_key(:undefined), do: "—"
  def truncate_key(key) when is_binary(key) and byte_size(key) > 16 do
    String.slice(key, 0, 8) <> "...." <> String.slice(key, -8, 8)
  end
  def truncate_key(key), do: to_string(key)

  def status_class("confirmed"), do: "bg-green-500/10 border border-green-500/20 text-green-400"
  def status_class("pending"), do: "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
  def status_class("failed"), do: "bg-red-500/10 border border-red-500/20 text-red-400"
  def status_class(_), do: "bg-slate-700/50 border border-slate-600/20 text-slate-400"

  def type_color("transfer"), do: "text-purple-400 bg-purple-500/10"
  def type_color("swap"), do: "text-blue-400 bg-blue-500/10"
  def type_color("stake"), do: "text-green-400 bg-green-500/10"
  def type_color(_), do: "text-slate-400 bg-slate-700/50"
end
