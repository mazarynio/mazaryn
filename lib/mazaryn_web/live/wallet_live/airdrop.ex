defmodule MazarynWeb.WalletLive.Airdrop do
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
            airdrops =
              case SolanaWallets.get_wallet_airdrops(wallet_id) do
                {:ok, a} -> a
                _ -> []
              end

            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:airdrops, airdrops)
             |> assign(:show_new_airdrop_modal, false)
             |> assign(:airdrop_amount, "")
             |> assign(:airdrop_memo, "")
             |> assign(:form_errors, %{})
             |> assign(:submitting, false)
             |> assign(:selected_airdrop, nil)}

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
  def handle_event("open_new_airdrop_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_new_airdrop_modal, true)
     |> assign(:airdrop_amount, "")
     |> assign(:airdrop_memo, "")
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_new_airdrop_modal", _params, socket) do
    {:noreply, assign(socket, :show_new_airdrop_modal, false)}
  end

  @impl true
  def handle_event("update_airdrop_amount", %{"value" => value}, socket) do
    {:noreply, assign(socket, :airdrop_amount, value)}
  end

  @impl true
  def handle_event("update_airdrop_memo", %{"value" => value}, socket) do
    {:noreply, assign(socket, :airdrop_memo, value)}
  end

  @impl true
  def handle_event("validate_airdrop", params, socket) do
    amount = Map.get(params, "amount", "")
    memo   = Map.get(params, "memo", "")
    errors = validate_airdrop_form(%{amount: amount, memo: memo})
    {:noreply,
     socket
     |> assign(:airdrop_amount, amount)
     |> assign(:airdrop_memo, memo)
     |> assign(:form_errors, errors)}
  end

  @impl true
  def handle_event("request_airdrop", params, socket) do
    amount = Map.get(params, "amount", "")
    memo   = Map.get(params, "memo", "")
    errors = validate_airdrop_form(%{amount: amount, memo: memo})

    if map_size(errors) > 0 do
      {:noreply, assign(socket, :form_errors, errors)}
    else
      socket = assign(socket, :submitting, true)
      wallet_id = socket.assigns.wallet_id

      result =
        try do
          SolanaWallets.request_airdrop(wallet_id, amount, memo)
        rescue
          e -> {:error, Exception.message(e)}
        end

      case result do
        {:ok, _} ->
          airdrops =
            case SolanaWallets.get_wallet_airdrops(wallet_id) do
              {:ok, a} -> a
              _ -> []
            end

          {:noreply,
           socket
           |> assign(:submitting, false)
           |> assign(:show_new_airdrop_modal, false)
           |> assign(:airdrops, airdrops)
           |> put_flash(:info, "Airdrop requested successfully")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(:submitting, false)
           |> assign(:form_errors, %{general: "Airdrop failed: #{inspect(reason)}"})}
      end
    end
  end

  @impl true
  def handle_event("select_airdrop", %{"id" => id}, socket) do
    airdrop = Enum.find(socket.assigns.airdrops, fn a -> to_string(a.airdrop_id) == id end)
    {:noreply, assign(socket, :selected_airdrop, airdrop)}
  end

  @impl true
  def handle_event("close_airdrop_detail", _params, socket) do
    {:noreply, assign(socket, :selected_airdrop, nil)}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    airdrops =
      case SolanaWallets.get_wallet_airdrops(socket.assigns.wallet_id) do
        {:ok, a} -> a
        _ -> []
      end

    {:noreply, assign(socket, :airdrops, airdrops)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  defp validate_airdrop_form(%{amount: amount}) do
    errors = %{}

    errors =
      case Float.parse(to_string(amount)) do
        {v, _} when v > 0 -> errors
        _ -> Map.put(errors, :amount, "Enter a valid positive amount")
      end

    errors
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

  def status_class("completed"), do: "bg-green-500/10 border border-green-500/20 text-green-400"
  def status_class("pending"),   do: "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
  def status_class("failed"),    do: "bg-red-500/10 border border-red-500/20 text-red-400"
  def status_class(_),           do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"
end
