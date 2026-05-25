defmodule MazarynWeb.SuiWalletLive.Stake do
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
             |> assign(:stakes, stakes)
             |> assign(:selected_stake, nil)
             |> assign(:show_new_stake_modal, false)
             |> assign(:validator_address, "")
             |> assign(:stake_amount_sui, "")
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
     |> assign(:validator_address, "")
     |> assign(:stake_amount_sui, "")
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
     |> assign(:validator_address, Map.get(params, "validator_address", ""))
     |> assign(:stake_amount_sui, Map.get(params, "stake_amount_sui", ""))
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("create_stake", params, socket) do
    validator_address = Map.get(params, "validator_address", "") |> String.trim()
    stake_amount_sui  = Map.get(params, "stake_amount_sui", "")  |> String.trim()

    errors = validate_stake_form(validator_address, stake_amount_sui)

    if map_size(errors) > 0 do
      {:noreply, assign(socket, :form_errors, errors)}
    else
      socket = assign(socket, :submitting, true)

      {amount_float, _} = Float.parse(stake_amount_sui)
      amount_mist = trunc(amount_float * 1_000_000_000)

      stake_data = %{
        owner_address:          socket.assigns.wallet.sui_address,
        validator_address:      validator_address,
        staked_sui_id:          "",
        principal_mist:         to_string(amount_mist),
        estimated_reward_mist:  "0",
        stake_activation_epoch: "",
        status:                 :pending,
        tx_type:                :stake,
        digest:                 "",
        gas_computation:        "0",
        gas_storage:            "0",
        gas_storage_rebate:     "0",
        network:                String.to_atom(to_string(socket.assigns.wallet.network || "mainnet"))
      }

      case SuiWallets.create_stake(socket.assigns.wallet_id, stake_data) do
        {:ok, _stake} ->
          stakes =
            case SuiWallets.get_wallet_stakes(socket.assigns.wallet_id) do
              {:ok, s} -> s
              _ -> []
            end

          {:noreply,
           socket
           |> assign(:submitting, false)
           |> assign(:show_new_stake_modal, false)
           |> assign(:validator_address, "")
           |> assign(:stake_amount_sui, "")
           |> assign(:stakes, stakes)
           |> put_flash(:info, "Stake recorded (#{stake_amount_sui} SUI → #{String.slice(validator_address, 0, 10)}...)")}

        {:error, reason} ->
          Logger.error("Sui stake creation failed: #{inspect(reason)}")

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
  def handle_event("unstake", %{"stake_id" => stake_id}, socket) do
    case SuiWallets.update_stake(stake_id, %{status: :unstaked}) do
      {:ok, _} ->
        stakes =
          case SuiWallets.get_wallet_stakes(socket.assigns.wallet_id) do
            {:ok, s} -> s
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:stakes, stakes)
         |> assign(:selected_stake, nil)
         |> put_flash(:info, "Unstake initiated")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to initiate unstake")}
    end
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    stakes =
      case SuiWallets.get_wallet_stakes(socket.assigns.wallet_id) do
        {:ok, s} -> s
        _ -> []
      end

    {:noreply, assign(socket, :stakes, stakes)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  defp validate_stake_form(validator_address, stake_amount_sui) do
    errors = %{}

    errors =
      if String.length(validator_address) < 10,
        do: Map.put(errors, :validator_address, "Enter a valid validator address (0x...)"),
        else: errors

    errors =
      case Float.parse(stake_amount_sui) do
        {v, _} when v >= 1.0 -> errors
        _ -> Map.put(errors, :stake_amount_sui, "Minimum stake is 1 SUI")
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

  def format_mist(nil), do: "0"
  def format_mist(:undefined), do: "0"
  def format_mist(mist) do
    case Integer.parse(to_string(mist)) do
      {n, _} -> :erlang.float_to_binary(n / 1_000_000_000, decimals: 4)
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
      "active"   -> "bg-green-500/10 border border-green-500/20 text-green-400"
      "pending"  -> "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
      "unstaked" -> "bg-slate-700/50 border border-slate-600/30 text-slate-400"
      _          -> "bg-slate-700/50 border border-slate-600/30 text-slate-400"
    end
  end
end
