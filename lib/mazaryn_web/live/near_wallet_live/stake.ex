defmodule MazarynWeb.NearWalletLive.Stake do
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
            stakes =
              case NearWallets.get_wallet_stakes(wallet_id) do
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
             |> assign(:validator_account_id, "")
             |> assign(:stake_amount, "")
             |> assign(:staking, false)
             |> assign(:form_errors, %{})}

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
  def handle_params(%{"live_action" => :new}, _url, socket) do
    {:noreply,
     socket
     |> assign(:show_new_stake_modal, true)
     |> assign(:validator_account_id, "")
     |> assign(:stake_amount, "")
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("open_new_stake_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_new_stake_modal, true)
     |> assign(:validator_account_id, "")
     |> assign(:stake_amount, "")
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_new_stake_modal", _params, socket) do
    {:noreply, assign(socket, :show_new_stake_modal, false)}
  end

  @impl true
  def handle_event(
        "validate_stake",
        %{"validator_account_id" => validator, "stake_amount" => amount},
        socket
      ) do
    {:noreply,
     socket
     |> assign(:validator_account_id, validator)
     |> assign(:stake_amount, amount)
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event(
        "create_stake",
        %{"validator_account_id" => validator, "stake_amount" => amount},
        socket
      ) do
    errors = validate_stake_form(validator, amount)

    if map_size(errors) > 0 do
      {:noreply, assign(socket, :form_errors, errors)}
    else
      socket = assign(socket, :staking, true)

      stake_data = %{
        validator_account_id: validator,
        amount_near: amount,
        status: "active"
      }

      case NearWallets.create_stake(socket.assigns.wallet_id, stake_data) do
        {:ok, _stake_id} ->
          stakes =
            case NearWallets.get_wallet_stakes(socket.assigns.wallet_id) do
              {:ok, s} -> s
              _ -> []
            end

          {:noreply,
           socket
           |> assign(:staking, false)
           |> assign(:show_new_stake_modal, false)
           |> assign(:validator_account_id, "")
           |> assign(:stake_amount, "")
           |> assign(:stakes, stakes)
           |> put_flash(:info, "Stake created successfully")}

        {:error, reason} ->
          Logger.error("NEAR stake creation failed: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(:staking, false)
           |> assign(:form_errors, %{general: "Failed to create stake. Please try again."})}
      end
    end
  end

  @impl true
  def handle_event("select_stake", %{"stake_id" => stake_id}, socket) do
    stake = Enum.find(socket.assigns.stakes, &(to_string(&1.stake_id) == stake_id))
    {:noreply, assign(socket, :selected_stake, stake)}
  end

  @impl true
  def handle_event("close_stake_modal", _params, socket) do
    {:noreply, assign(socket, :selected_stake, nil)}
  end

  @impl true
  def handle_event("unstake", %{"stake_id" => stake_id}, socket) do
    case NearWallets.update_stake(stake_id, %{status: "unstaked"}) do
      :ok ->
        stakes =
          case NearWallets.get_wallet_stakes(socket.assigns.wallet_id) do
            {:ok, s} -> s
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:stakes, stakes)
         |> assign(:selected_stake, nil)
         |> put_flash(:info, "Unstake initiated successfully")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to unstake")}
    end
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    stakes =
      case NearWallets.get_wallet_stakes(socket.assigns.wallet_id) do
        {:ok, s} -> s
        _ -> []
      end

    {:noreply,
     socket
     |> assign(:stakes, stakes)
     |> put_flash(:info, "Stakes refreshed")}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  defp validate_stake_form(validator, amount) do
    errors = %{}

    errors =
      if String.trim(validator) == "" do
        Map.put(errors, :validator_account_id, "Validator account ID is required")
      else
        trimmed = String.trim(validator)
        valid = String.match?(trimmed, ~r/^[a-z0-9_\-\.]+\.(near|testnet|pool\.near|pool\.testnet)$/)

        if valid do
          errors
        else
          Map.put(errors, :validator_account_id, "Enter a valid validator account ID (e.g. validator.pool.near)")
        end
      end

    errors =
      if String.trim(amount) == "" do
        Map.put(errors, :stake_amount, "Amount is required")
      else
        case Float.parse(amount) do
          {val, ""} when val >= 1.0 ->
            errors

          {val, ""} when val < 1.0 ->
            Map.put(errors, :stake_amount, "Minimum stake is 1 NEAR")

          _ ->
            Map.put(errors, :stake_amount, "Invalid amount")
        end
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

  def stake_status_class("active"),    do: "bg-green-500/10 border border-green-500/20 text-green-400"
  def stake_status_class("unstaked"),  do: "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
  def stake_status_class("withdrawn"), do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"
  def stake_status_class(_),           do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"

  def total_staked(stakes) do
    stakes
    |> Enum.filter(fn s -> to_string(s.status || "") == "active" end)
    |> Enum.map(fn s ->
      case s.amount_near do
        nil -> 0.0
        v when is_float(v) -> v
        v when is_integer(v) -> v * 1.0
        v ->
          case Float.parse(to_string(v)) do
            {f, _} -> f
            :error -> 0.0
          end
      end
    end)
    |> Enum.sum()
  end
end
