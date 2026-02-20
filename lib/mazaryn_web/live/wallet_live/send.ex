defmodule MazarynWeb.WalletLive.Send do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Mazaryn.SolanaWallets

  @impl true
  def mount(
        %{"wallet_id" => wallet_id} = _params,
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        case SolanaWallets.get_wallet(wallet_id) do
          {:ok, wallet} ->
            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:session_uuid, session_uuid)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:step, "form")
             |> assign(:recipient, "")
             |> assign(:amount, "")
             |> assign(:memo, "")
             |> assign(:tx_type, "transfer")
             |> assign(:form_errors, %{})
             |> assign(:submitting, false)
             |> assign(:tx_result, nil)}

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
  def handle_event("update_recipient", %{"value" => value}, socket) do
    {:noreply, assign(socket, :recipient, value)}
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
        %{"recipient" => recipient, "amount" => amount, "memo" => memo, "tx_type" => tx_type},
        socket
      ) do
    errors = validate_form(recipient, amount)

    {:noreply,
     socket
     |> assign(:recipient, recipient)
     |> assign(:amount, amount)
     |> assign(:memo, memo)
     |> assign(:tx_type, tx_type)
     |> assign(:form_errors, errors)}
  end

  @impl true
  def handle_event(
        "preview",
        %{"recipient" => recipient, "amount" => amount, "memo" => memo, "tx_type" => tx_type},
        socket
      ) do
    errors = validate_form(recipient, amount)

    if map_size(errors) > 0 do
      {:noreply,
       socket
       |> assign(:recipient, recipient)
       |> assign(:amount, amount)
       |> assign(:memo, memo)
       |> assign(:tx_type, tx_type)
       |> assign(:form_errors, errors)}
    else
      {:noreply,
       socket
       |> assign(:recipient, recipient)
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
      recipient_address: socket.assigns.recipient,
      amount: socket.assigns.amount,
      memo: socket.assigns.memo,
      tx_type: socket.assigns.tx_type,
      sender_address: socket.assigns.wallet.public_key
    }

    case SolanaWallets.create_transaction(socket.assigns.wallet_id, tx_data) do
      {:ok, tx_id} ->
        {:noreply,
         socket
         |> assign(:submitting, false)
         |> assign(:step, "success")
         |> assign(:tx_result, tx_id)}

      {:error, reason} ->
        Logger.error("Transaction failed: #{inspect(reason)}")

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
     |> assign(:recipient, "")
     |> assign(:amount, "")
     |> assign(:memo, "")
     |> assign(:tx_type, "transfer")
     |> assign(:form_errors, %{})
     |> assign(:tx_result, nil)}
  end

  defp validate_form(recipient, amount) do
    errors = %{}

    errors =
      if String.trim(recipient) == "" do
        Map.put(errors, :recipient, "Recipient address is required")
      else
        if String.length(String.trim(recipient)) < 32 do
          Map.put(errors, :recipient, "Invalid Solana address")
        else
          errors
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
