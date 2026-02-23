defmodule MazarynWeb.NearWalletLive.AccessKeys do
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
            access_keys =
              case NearWallets.get_wallet_access_keys(wallet_id) do
                {:ok, k} -> k
                _ -> []
              end

            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:access_keys, access_keys)
             |> assign(:selected_key, nil)
             |> assign(:show_add_key_modal, false)
             |> assign(:new_key_public_key, "")
             |> assign(:new_key_label, "")
             |> assign(:new_key_type, "FunctionCall")
             |> assign(:new_key_contract_id, "")
             |> assign(:new_key_allowance, "")
             |> assign(:adding_key, false)
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
  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("open_add_key_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_add_key_modal, true)
     |> assign(:new_key_public_key, "")
     |> assign(:new_key_label, "")
     |> assign(:new_key_type, "FunctionCall")
     |> assign(:new_key_contract_id, "")
     |> assign(:new_key_allowance, "")
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_add_key_modal", _params, socket) do
    {:noreply, assign(socket, :show_add_key_modal, false)}
  end

  @impl true
  def handle_event(
        "validate_key",
        %{
          "public_key" => pk,
          "label" => label,
          "key_type" => key_type,
          "contract_id" => contract_id,
          "allowance" => allowance
        },
        socket
      ) do
    {:noreply,
     socket
     |> assign(:new_key_public_key, pk)
     |> assign(:new_key_label, label)
     |> assign(:new_key_type, key_type)
     |> assign(:new_key_contract_id, contract_id)
     |> assign(:new_key_allowance, allowance)
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event(
        "add_key",
        %{
          "public_key" => pk,
          "label" => label,
          "key_type" => key_type,
          "contract_id" => contract_id,
          "allowance" => allowance
        },
        socket
      ) do
    errors = validate_key_form(pk, key_type, contract_id)

    if map_size(errors) > 0 do
      {:noreply, assign(socket, :form_errors, errors)}
    else
      socket = assign(socket, :adding_key, true)

      key_data = %{
        public_key: pk,
        key_type: key_type,
        label: label,
        contract_id: if(String.trim(contract_id) == "", do: nil, else: contract_id),
        allowance_near: parse_float(allowance),
        method_names: []
      }

      case NearWallets.create_access_key(socket.assigns.wallet_id, key_data) do
        {:ok, _key_id} ->
          access_keys =
            case NearWallets.get_wallet_access_keys(socket.assigns.wallet_id) do
              {:ok, k} -> k
              _ -> []
            end

          {:noreply,
           socket
           |> assign(:adding_key, false)
           |> assign(:show_add_key_modal, false)
           |> assign(:access_keys, access_keys)
           |> put_flash(:info, "Access key added successfully")}

        {:error, reason} ->
          Logger.error("NEAR access key creation failed: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(:adding_key, false)
           |> assign(:form_errors, %{general: "Failed to add key. Please try again."})}
      end
    end
  end

  @impl true
  def handle_event("select_key", %{"key_id" => key_id}, socket) do
    key = Enum.find(socket.assigns.access_keys, &(to_string(&1.key_id) == key_id))
    {:noreply, assign(socket, :selected_key, key)}
  end

  @impl true
  def handle_event("close_key_modal", _params, socket) do
    {:noreply, assign(socket, :selected_key, nil)}
  end

  @impl true
  def handle_event("delete_key", %{"key_id" => key_id}, socket) do
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
         |> assign(:selected_key, nil)
         |> put_flash(:info, "Access key deleted")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to delete access key")}
    end
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    access_keys =
      case NearWallets.get_wallet_access_keys(socket.assigns.wallet_id) do
        {:ok, k} -> k
        _ -> []
      end

    {:noreply,
     socket
     |> assign(:access_keys, access_keys)
     |> put_flash(:info, "Access keys refreshed")}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  defp validate_key_form(pk, key_type, contract_id) do
    errors = %{}

    errors =
      if String.trim(pk) == "" do
        Map.put(errors, :public_key, "Public key is required")
      else
        trimmed = String.trim(pk)
        valid = String.match?(trimmed, ~r/^(ed25519:)?[A-Za-z0-9+\/]{43,44}={0,2}$/)
        if valid, do: errors, else: Map.put(errors, :public_key, "Enter a valid ed25519 public key")
      end

    errors =
      if key_type == "FunctionCall" and String.trim(contract_id) == "" do
        Map.put(errors, :contract_id, "Contract ID is required for Function Call keys")
      else
        errors
      end

    errors
  end

  defp parse_float(""), do: nil
  defp parse_float(s) do
    case Float.parse(s) do
      {v, _} -> v
      :error -> nil
    end
  end

  def truncate_key(nil), do: "—"
  def truncate_key(:undefined), do: "—"
  def truncate_key(key) do
    s = to_string(key)
    if String.length(s) > 20,
      do: String.slice(s, 0, 10) <> "...." <> String.slice(s, -10, 10),
      else: s
  end

  def format_datetime(nil), do: "—"
  def format_datetime(:undefined), do: "—"
  def format_datetime({{y, m, d}, {h, mn, s}}) do
    :io_lib.format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [y, m, d, h, mn, s])
    |> IO.iodata_to_binary()
  end
  def format_datetime(%NaiveDateTime{} = dt), do: NaiveDateTime.to_string(dt) |> String.slice(0, 16)
  def format_datetime(%DateTime{} = dt),      do: DateTime.to_string(dt) |> String.slice(0, 16)
  def format_datetime(_), do: "—"

  def key_type_class("FullAccess"),   do: "bg-red-500/10 border border-red-500/20 text-red-400"
  def key_type_class("FunctionCall"), do: "bg-blue-500/10 border border-blue-500/20 text-blue-400"
  def key_type_class(_),              do: "bg-slate-700/50 border border-slate-600/30 text-slate-400"
end
