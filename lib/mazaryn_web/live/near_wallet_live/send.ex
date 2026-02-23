defmodule MazarynWeb.NearWalletLive.Send do
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
            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:session_uuid, session_uuid)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:step, "form")
             |> assign(:receiver_id, "")
             |> assign(:amount, "")
             |> assign(:memo, "")
             |> assign(:tx_type, "transfer")
             |> assign(:form_errors, %{})
             |> assign(:submitting, false)
             |> assign(:tx_result, nil)}

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
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("update_receiver_id", %{"value" => value}, socket) do
    {:noreply, assign(socket, :receiver_id, value)}
  end

  @impl true
  def handle_event("update_amount", %{"value" => value}, socket) do
    {:noreply, assign(socket, :amount, value)}
  end

  @impl true
  def handle_event("update_memo", %{"value" => value}, socket) do
    {:noreply, assign(socket, :memo, value)}
  end

  @impl true
  def handle_event("update_tx_type", %{"value" => value}, socket) do
    {:noreply, assign(socket, :tx_type, value)}
  end

  @impl true
  def handle_event(
        "validate",
        %{"receiver_id" => receiver_id, "amount" => amount, "memo" => memo, "tx_type" => tx_type},
        socket
      ) do
    errors = validate_form(receiver_id, amount)

    {:noreply,
     socket
     |> assign(:receiver_id, receiver_id)
     |> assign(:amount, amount)
     |> assign(:memo, memo)
     |> assign(:tx_type, tx_type)
     |> assign(:form_errors, errors)}
  end

  @impl true
  def handle_event(
        "preview",
        %{"receiver_id" => receiver_id, "amount" => amount, "memo" => memo, "tx_type" => tx_type},
        socket
      ) do
    errors = validate_form(receiver_id, amount)

    if map_size(errors) > 0 do
      {:noreply,
       socket
       |> assign(:receiver_id, receiver_id)
       |> assign(:amount, amount)
       |> assign(:memo, memo)
       |> assign(:tx_type, tx_type)
       |> assign(:form_errors, errors)}
    else
      {:noreply,
       socket
       |> assign(:receiver_id, receiver_id)
       |> assign(:amount, amount)
       |> assign(:memo, memo)
       |> assign(:tx_type, tx_type)
       |> assign(:form_errors, %{})
       |> assign(:step, "confirm")}
    end
  end

  @impl true
  def handle_event("back_to_form", _params, socket) do
    {:noreply, assign(socket, :step, "form")}
  end

  @impl true
  def handle_event("confirm_send", _params, socket) do
    socket = assign(socket, :submitting, true)

    tx_data = %{
      receiver_id: socket.assigns.receiver_id,
      amount_near: socket.assigns.amount,
      memo: socket.assigns.memo,
      tx_type: socket.assigns.tx_type,
      from_account_id: socket.assigns.wallet.account_id
    }

    case NearWallets.create_transaction(socket.assigns.wallet_id, tx_data) do
      {:ok, tx_id} ->
        {:noreply,
         socket
         |> assign(:submitting, false)
         |> assign(:step, "success")
         |> assign(:tx_result, tx_id)}

      {:error, reason} ->
        Logger.error("NEAR transaction failed: #{inspect(reason)}")

        {:noreply,
         socket
         |> assign(:submitting, false)
         |> assign(:step, "error")
         |> assign(:tx_result, inspect(reason))}
    end
  end

  @impl true
  def handle_event("send_another", _params, socket) do
    {:noreply,
     socket
     |> assign(:step, "form")
     |> assign(:receiver_id, "")
     |> assign(:amount, "")
     |> assign(:memo, "")
     |> assign(:tx_type, "transfer")
     |> assign(:form_errors, %{})
     |> assign(:tx_result, nil)}
  end

  defp validate_form(receiver_id, amount) do
    errors = %{}

    errors =
      if String.trim(receiver_id) == "" do
        Map.put(errors, :receiver_id, "Receiver account ID is required")
      else
        trimmed = String.trim(receiver_id)
        valid_named = String.match?(trimmed, ~r/^[a-z0-9_\-\.]+\.(near|testnet)$/)
        valid_implicit = String.match?(trimmed, ~r/^[0-9a-f]{64}$/)

        if valid_named or valid_implicit do
          errors
        else
          Map.put(errors, :receiver_id, "Enter a valid NEAR account ID (e.g. alice.near) or 64-char implicit account")
        end
      end

    errors =
      if String.trim(amount) == "" do
        Map.put(errors, :amount, "Amount is required")
      else
        case Float.parse(amount) do
          {val, ""} when val > 0 ->
            errors

          {val, ""} when val <= 0 ->
            Map.put(errors, :amount, "Amount must be greater than 0")

          _ ->
            Map.put(errors, :amount, "Invalid amount")
        end
      end

    errors
  end
end
