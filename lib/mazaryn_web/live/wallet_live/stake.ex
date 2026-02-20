defmodule MazarynWeb.WalletLive.Stake do
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
             |> assign(:stakes, stakes)
             |> assign(:show_new_stake_modal, false)
             |> assign(:validator_address, "")
             |> assign(:stake_amount, "")
             |> assign(:stake_label, "")
             |> assign(:form_errors, %{})
             |> assign(:submitting, false)
             |> assign(:selected_stake, nil)}

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
     |> assign(:stake_amount, "")
     |> assign(:stake_label, "")
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_new_stake_modal", _params, socket) do
    {:noreply, assign(socket, :show_new_stake_modal, false)}
  end

  @impl true
  def handle_event("validate_stake", params, socket) do
    validator_address = Map.get(params, "validator_address", "")
    stake_amount      = Map.get(params, "stake_amount", "")
    stake_label       = Map.get(params, "stake_label", "")
    errors = validate_stake_form(%{validator_address: validator_address, stake_amount: stake_amount})

    {:noreply,
     socket
     |> assign(:validator_address, validator_address)
     |> assign(:stake_amount, stake_amount)
     |> assign(:stake_label, stake_label)
     |> assign(:form_errors, errors)}
  end

  @impl true
  def handle_event("create_stake", params, socket) do
    validator_address = Map.get(params, "validator_address", "")
    stake_amount      = Map.get(params, "stake_amount", "")
    stake_label       = Map.get(params, "stake_label", "")
    errors = validate_stake_form(%{validator_address: validator_address, stake_amount: stake_amount})

    if map_size(errors) > 0 do
      {:noreply, assign(socket, :form_errors, errors)}
    else
      socket = assign(socket, :submitting, true)
      wallet_id = socket.assigns.wallet_id

      result =
        try do
          SolanaWallets.create_stake_account(wallet_id, validator_address, stake_amount, stake_label)
        rescue
          e -> {:error, Exception.message(e)}
        end

      case result do
        {:ok, _} ->
          stakes =
            case SolanaWallets.get_wallet_stakes(wallet_id) do
              {:ok, s} -> s
              _ -> []
            end

          {:noreply,
           socket
           |> assign(:submitting, false)
           |> assign(:show_new_stake_modal, false)
           |> assign(:stakes, stakes)
           |> put_flash(:info, "Stake account created successfully")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(:submitting, false)
           |> assign(:form_errors, %{general: "Failed to create stake: #{inspect(reason)}"})}
      end
    end
  end

  @impl true
  def handle_event("select_stake", %{"id" => id}, socket) do
    stake = Enum.find(socket.assigns.stakes, fn s -> to_string(s.stake_account_id) == id end)
    {:noreply, assign(socket, :selected_stake, stake)}
  end

  @impl true
  def handle_event("close_stake_detail", _params, socket) do
    {:noreply, assign(socket, :selected_stake, nil)}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    stakes =
      case SolanaWallets.get_wallet_stakes(socket.assigns.wallet_id) do
        {:ok, s} -> s
        _ -> []
      end

    {:noreply, assign(socket, :stakes, stakes)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  defp validate_stake_form(%{validator_address: addr, stake_amount: amount}) do
    errors = %{}

    errors =
      if String.length(to_string(addr)) < 32,
        do: Map.put(errors, :validator_address, "Enter a valid validator address"),
        else: errors

    errors =
      case Float.parse(to_string(amount)) do
        {v, _} when v >= 0.01 -> errors
        _ -> Map.put(errors, :stake_amount, "Minimum stake is 0.01 SOL")
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

  def status_class("active"),        do: "bg-green-500/10 border border-green-500/20 text-green-400"
  def status_class("inactive"),      do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"
  def status_class("deactivating"),  do: "bg-yellow-500/10 border border-yellow-500/20 text-yellow-400"
  def status_class(_),               do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"
end
