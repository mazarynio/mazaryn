defmodule MazarynWeb.WalletLive.Tokens do
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
            tokens =
              case SolanaWallets.get_wallet_token_accounts(wallet_id) do
                {:ok, t} -> t
                _ -> []
              end

            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:tokens, tokens)
             |> assign(:filtered_tokens, tokens)
             |> assign(:search, "")
             |> assign(:selected_token, nil)}

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
  def handle_event("search", %{"value" => search}, socket) do
    filtered =
      if search == "" do
        socket.assigns.tokens
      else
        s = String.downcase(search)
        Enum.filter(socket.assigns.tokens, fn t ->
          String.contains?(String.downcase(to_string(t.token_symbol)), s) or
          String.contains?(String.downcase(to_string(t.token_name)), s) or
          String.contains?(String.downcase(to_string(t.mint_address)), s)
        end)
      end

    {:noreply, socket |> assign(:search, search) |> assign(:filtered_tokens, filtered)}
  end

  @impl true
  def handle_event("select_token", %{"token_id" => token_id}, socket) do
    token = Enum.find(socket.assigns.tokens, &(to_string(&1.token_account_id) == token_id))
    {:noreply, assign(socket, :selected_token, token)}
  end

  @impl true
  def handle_event("close_token_modal", _params, socket) do
    {:noreply, assign(socket, :selected_token, nil)}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    tokens =
      case SolanaWallets.get_wallet_token_accounts(socket.assigns.wallet_id) do
        {:ok, t} -> t
        _ -> []
      end

    {:noreply,
     socket
     |> assign(:tokens, tokens)
     |> assign(:filtered_tokens, tokens)
     |> assign(:search, "")
     |> put_flash(:info, "Tokens refreshed")}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  def truncate_key(nil), do: "—"
  def truncate_key(:undefined), do: "—"
  def truncate_key(key) when is_binary(key) and byte_size(key) > 16 do
    String.slice(key, 0, 8) <> "...." <> String.slice(key, -8, 8)
  end
  def truncate_key(key), do: to_string(key)

  def format_datetime(nil), do: "—"
  def format_datetime(:undefined), do: "—"
  def format_datetime(dt) do
    case dt do
      %NaiveDateTime{} -> NaiveDateTime.to_string(dt) |> String.slice(0, 16)
      %DateTime{} -> DateTime.to_string(dt) |> String.slice(0, 16)
      _ -> "—"
    end
  end

  def token_color(symbol) do
    colors = %{
      "USDC" => "from-blue-500 to-cyan-600",
      "USDT" => "from-green-500 to-emerald-600",
      "RAY" => "from-purple-500 to-indigo-600",
      "SRM" => "from-blue-400 to-blue-600",
      "ORCA" => "from-pink-500 to-rose-600",
      "MNGO" => "from-orange-400 to-amber-600"
    }
    Map.get(colors, to_string(symbol), "from-slate-500 to-slate-700")
  end
end
