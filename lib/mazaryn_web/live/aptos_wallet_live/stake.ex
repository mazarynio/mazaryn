defmodule MazarynWeb.AptosWalletLive.Stake do
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
             |> assign(:stakes, stakes)
             |> assign(:selected_stake, nil)
             |> assign(:show_new_stake_modal, false)
             |> assign(:pool_address, "")
             |> assign(:stake_amount_apt, "")
             |> assign(:form_errors, %{})
             |> assign(:submitting, false)}

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
  def handle_event("open_new_stake_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_new_stake_modal, true)
     |> assign(:pool_address, "")
     |> assign(:stake_amount_apt, "")
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_new_stake_modal", _params, socket) do
    {:noreply, assign(socket, :show_new_stake_modal, false)}
  end

  @impl true
  def handle_event("validate_stake", params, socket) do
    {:noreply,
     socket
     |> assign(:pool_address, Map.get(params, "pool_address", ""))
     |> assign(:stake_amount_apt, Map.get(params, "stake_amount_apt", ""))
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("create_stake", params, socket) do
    pool_address    = Map.get(params, "pool_address", "")    |> String.trim()
    stake_amount_apt = Map.get(params, "stake_amount_apt", "") |> String.trim()

    errors = validate_stake_form(pool_address, stake_amount_apt)

    if map_size(errors) > 0 do
      {:noreply, assign(socket, :form_errors, errors)}
    else
      socket = assign(socket, :submitting, true)

      {amount_float, _} = Float.parse(stake_amount_apt)
      amount_octas = trunc(amount_float * 100_000_000)

      stake_data = %{
        delegator_address:     socket.assigns.wallet.account_address,
        pool_address:          pool_address,
        amount_octas:          amount_octas,
        active_stake:          "0",
        inactive_stake:        "0",
        pending_inactive_stake: "0",
        tx_type:               :stake_add,
        transaction_hash:      "",
        gas_used:              0,
        gas_unit_price:        0,
        vm_status:             "",
        version:               "",
        network:               to_string(socket.assigns.wallet.network || "mainnet"),
        status:                "pending"
      }

      case AptosWallets.create_stake(socket.assigns.wallet_id, stake_data) do
        {:ok, _stake} ->
          stakes =
            case AptosWallets.get_wallet_stakes(socket.assigns.wallet_id) do
              {:ok, s} -> s
              _ -> []
            end

          {:noreply,
           socket
           |> assign(:submitting, false)
           |> assign(:show_new_stake_modal, false)
           |> assign(:pool_address, "")
           |> assign(:stake_amount_apt, "")
           |> assign(:stakes, stakes)
           |> put_flash(:info, "Stake recorded (#{stake_amount_apt} APT → #{String.slice(pool_address, 0, 10)}...)")}

        {:error, reason} ->
          Logger.error("Aptos stake creation failed: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(:submitting, false)
           |> assign(:form_errors, %{general: "Failed to record stake. Please try again."})}
      end
    end
  end

  @impl true
  def handle_event("select_stake", %{"stake_id" => stake_id}, socket) do
    stake = Enum.find(socket.assigns.stakes, fn s -> to_string(s.stake_id) == stake_id end)
    {:noreply, assign(socket, :selected_stake, stake)}
  end

  @impl true
  def handle_event("close_stake_detail", _params, socket) do
    {:noreply, assign(socket, :selected_stake, nil)}
  end

  @impl true
  def handle_event("unlock_stake", %{"stake_id" => stake_id}, socket) do
    case AptosWallets.update_stake(stake_id, %{status: "pending_inactive"}) do
      {:ok, _} ->
        stakes =
          case AptosWallets.get_wallet_stakes(socket.assigns.wallet_id) do
            {:ok, s} -> s
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:stakes, stakes)
         |> assign(:selected_stake, nil)
         |> put_flash(:info, "Unlock initiated — stake will be withdrawable next epoch")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to initiate unlock")}
    end
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    stakes =
      case AptosWallets.get_wallet_stakes(socket.assigns.wallet_id) do
        {:ok, s} -> s
        _ -> []
      end

    {:noreply, assign(socket, :stakes, stakes)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  defp validate_stake_form(pool_address, stake_amount_apt) do
    errors = %{}

    errors =
      if String.length(pool_address) < 10,
        do: Map.put(errors, :pool_address, "Enter a valid delegation pool address (0x...)"),
        else: errors

    errors =
      case Float.parse(stake_amount_apt) do
        {v, _} when v >= 11.0 -> errors
        _ -> Map.put(errors, :stake_amount_apt, "Minimum stake is 11 APT")
      end

    errors
  end

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
      {n, _} -> :erlang.float_to_binary(n / 100_000_000, decimals: 4)
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

  def status_class(s) do
    case to_string(s) do
      "active"           -> "bg-green-500/10 border border-green-500/20 text-green-400"
      "pending"          -> "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
      "pending_inactive" -> "bg-orange-500/10 border border-orange-500/20 text-orange-400"
      "inactive"         -> "bg-slate-700/50 border border-slate-600/30 text-slate-400"
      _                  -> "bg-slate-700/50 border border-slate-600/30 text-slate-400"
    end
  end
end
