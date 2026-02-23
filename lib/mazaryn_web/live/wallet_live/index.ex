defmodule MazarynWeb.WalletLive.Index do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Mazaryn.SolanaWallets
  alias Mazaryn.NearWallets

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        solana_wallets =
          case SolanaWallets.get_user_wallets(user.id) do
            {:ok, wallets} -> wallets
            _ -> []
          end

        near_wallets =
          case NearWallets.get_user_wallets(user.id) do
            {:ok, wallets} -> wallets
            _ -> []
          end

        {:ok,
         socket
         |> assign(:user, user)
         |> assign(:current_user, user)
         |> assign(:session_uuid, session_uuid)
         |> assign(:solana_wallets, solana_wallets)
         |> assign(:near_wallets, near_wallets)
         |> assign(:active_wallet_tab, "solana")
         |> assign(:show_create_modal, false)
         |> assign(:show_import_modal, false)
         |> assign(:show_export_modal, false)
         |> assign(:show_near_create_modal, false)
         |> assign(:create_label, "")
         |> assign(:create_password, "")
         |> assign(:import_public_key, "")
         |> assign(:import_label, "")
         |> assign(:export_password, "")
         |> assign(:export_wallet_id, nil)
         |> assign(:exported_key, nil)
         |> assign(:creating, false)
         |> assign(:importing, false)
         |> assign(:near_create_account_id, "")
         |> assign(:near_create_label, "")
         |> assign(:near_create_network, "testnet")
         |> assign(:near_creating, false)
         |> assign(:form_errors, %{})}

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
  def handle_event("switch_wallet_tab", %{"tab" => tab}, socket) do
    {:noreply, assign(socket, :active_wallet_tab, tab)}
  end

  @impl true
  def handle_event("open_create_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_create_modal, true)
     |> assign(:create_label, "")
     |> assign(:create_password, "")
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_create_modal", _params, socket) do
    {:noreply, assign(socket, :show_create_modal, false)}
  end

  @impl true
  def handle_event("open_import_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_import_modal, true)
     |> assign(:import_public_key, "")
     |> assign(:import_label, "")
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_import_modal", _params, socket) do
    {:noreply, assign(socket, :show_import_modal, false)}
  end

  @impl true
  def handle_event("open_export_modal", %{"wallet_id" => wallet_id}, socket) do
    {:noreply,
     socket
     |> assign(:show_export_modal, true)
     |> assign(:export_wallet_id, wallet_id)
     |> assign(:export_password, "")
     |> assign(:exported_key, nil)
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_export_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_export_modal, false)
     |> assign(:export_wallet_id, nil)
     |> assign(:exported_key, nil)
     |> assign(:export_password, "")}
  end

  @impl true
  def handle_event("open_near_create_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_near_create_modal, true)
     |> assign(:near_create_account_id, "")
     |> assign(:near_create_label, "")
     |> assign(:near_create_network, "testnet")
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_near_create_modal", _params, socket) do
    {:noreply, assign(socket, :show_near_create_modal, false)}
  end

  @impl true
  def handle_event("validate_create", %{"label" => label, "password" => password}, socket) do
    {:noreply,
     socket
     |> assign(:create_label, label)
     |> assign(:create_password, password)
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("validate_import", %{"public_key" => pk, "label" => label}, socket) do
    {:noreply,
     socket
     |> assign(:import_public_key, pk)
     |> assign(:import_label, label)
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("validate_export", %{"password" => password}, socket) do
    {:noreply,
     socket
     |> assign(:export_password, password)
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event(
        "validate_near_create",
        %{"account_id" => account_id, "label" => label, "network" => network},
        socket
      ) do
    {:noreply,
     socket
     |> assign(:near_create_account_id, account_id)
     |> assign(:near_create_label, label)
     |> assign(:near_create_network, network)
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("create_wallet", %{"label" => label, "password" => password}, socket) do
    errors = %{}

    errors =
      if String.trim(label) == "",
        do: Map.put(errors, :label, "Label is required"),
        else: errors

    errors =
      if String.length(password) < 8,
        do: Map.put(errors, :password, "Password must be at least 8 characters"),
        else: errors

    if map_size(errors) > 0 do
      {:noreply, assign(socket, :form_errors, errors)}
    else
      socket = assign(socket, :creating, true)

      case SolanaWallets.create_wallet(socket.assigns.user.id, label, password) do
        {:ok, _wallet_id} ->
          wallets =
            case SolanaWallets.get_user_wallets(socket.assigns.user.id) do
              {:ok, w} -> w
              _ -> []
            end

          {:noreply,
           socket
           |> assign(:creating, false)
           |> assign(:show_create_modal, false)
           |> assign(:create_label, "")
           |> assign(:create_password, "")
           |> assign(:solana_wallets, wallets)
           |> put_flash(:info, "Wallet created successfully")}

        {:error, reason} ->
          Logger.error("Wallet creation failed: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(:creating, false)
           |> assign(:form_errors, %{general: "Failed to create wallet. Please try again."})}
      end
    end
  end

  @impl true
  def handle_event(
        "create_near_wallet",
        %{"account_id" => account_id, "label" => label, "network" => network},
        socket
      ) do
    errors = %{}

    errors =
      if String.trim(account_id) == "",
        do: Map.put(errors, :account_id, "Account ID is required"),
        else: errors

    errors =
      if String.trim(label) == "",
        do: Map.put(errors, :label, "Label is required"),
        else: errors

    if map_size(errors) > 0 do
      {:noreply, assign(socket, :form_errors, errors)}
    else
      socket = assign(socket, :near_creating, true)

      case NearWallets.create_wallet(socket.assigns.user.id, account_id, network, label) do
        {:ok, _wallet_id} ->
          wallets =
            case NearWallets.get_user_wallets(socket.assigns.user.id) do
              {:ok, w} -> w
              _ -> []
            end

          {:noreply,
           socket
           |> assign(:near_creating, false)
           |> assign(:show_near_create_modal, false)
           |> assign(:near_create_account_id, "")
           |> assign(:near_create_label, "")
           |> assign(:near_create_network, "testnet")
           |> assign(:near_wallets, wallets)
           |> put_flash(:info, "NEAR wallet created successfully")}

        {:error, "Account ID already exists"} ->
          {:noreply,
           socket
           |> assign(:near_creating, false)
           |> assign(:form_errors, %{account_id: "This account ID is already registered"})}

        {:error, reason} ->
          Logger.error("NEAR wallet creation failed: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(:near_creating, false)
           |> assign(:form_errors, %{general: "Failed to create NEAR wallet. Please try again."})}
      end
    end
  end

  @impl true
  def handle_event("export_key", %{"password" => password}, socket) do
    if String.trim(password) == "" do
      {:noreply, assign(socket, :form_errors, %{password: "Password is required"})}
    else
      case SolanaWallets.export_private_key(socket.assigns.export_wallet_id, password) do
        {:ok, private_key} ->
          {:noreply,
           socket
           |> assign(:exported_key, private_key)
           |> assign(:form_errors, %{})}

        {:error, :invalid_password} ->
          {:noreply, assign(socket, :form_errors, %{password: "Incorrect password"})}

        {:error, _} ->
          {:noreply,
           assign(socket, :form_errors, %{password: "Failed to export key. Try again."})}
      end
    end
  end

  @impl true
  def handle_event("set_primary_wallet", %{"wallet_id" => wallet_id}, socket) do
    case SolanaWallets.set_primary_wallet(socket.assigns.user.id, wallet_id) do
      :ok ->
        wallets =
          case SolanaWallets.get_user_wallets(socket.assigns.user.id) do
            {:ok, w} -> w
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:solana_wallets, wallets)
         |> put_flash(:info, "Primary wallet updated")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to update primary wallet")}
    end
  end

  @impl true
  def handle_event("set_near_primary_wallet", %{"wallet_id" => wallet_id}, socket) do
    case NearWallets.set_primary_wallet(socket.assigns.user.id, wallet_id) do
      :ok ->
        wallets =
          case NearWallets.get_user_wallets(socket.assigns.user.id) do
            {:ok, w} -> w
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:near_wallets, wallets)
         |> put_flash(:info, "Primary NEAR wallet updated")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to update primary NEAR wallet")}
    end
  end

  @impl true
  def handle_event("delete_wallet", %{"wallet_id" => wallet_id}, socket) do
    case SolanaWallets.delete_wallet(wallet_id) do
      :ok ->
        wallets =
          case SolanaWallets.get_user_wallets(socket.assigns.user.id) do
            {:ok, w} -> w
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:solana_wallets, wallets)
         |> put_flash(:info, "Wallet deleted")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to delete wallet")}
    end
  end

  @impl true
  def handle_event("delete_near_wallet", %{"wallet_id" => wallet_id}, socket) do
    case NearWallets.delete_wallet(wallet_id) do
      :ok ->
        wallets =
          case NearWallets.get_user_wallets(socket.assigns.user.id) do
            {:ok, w} -> w
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:near_wallets, wallets)
         |> put_flash(:info, "NEAR wallet deleted")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to delete NEAR wallet")}
    end
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end
end
