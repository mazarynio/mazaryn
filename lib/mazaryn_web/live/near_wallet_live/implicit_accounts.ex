defmodule MazarynWeb.NearWalletLive.ImplicitAccounts do
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
            implicit_accounts =
              case NearWallets.get_wallet_implicit_accounts(wallet_id) do
                {:ok, a} -> a
                _ -> []
              end

            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:implicit_accounts, implicit_accounts)
             |> assign(:selected_account, nil)
             |> assign(:show_generate_modal, false)
             |> assign(:generating, false)
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
  def handle_event("open_generate_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_generate_modal, true)
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event("close_generate_modal", _params, socket) do
    {:noreply, assign(socket, :show_generate_modal, false)}
  end

  @impl true
  def handle_event("generate_implicit_account", _params, socket) do
    socket = assign(socket, :generating, true)

    account_data = %{
      account_id: generate_implicit_account_id(),
      funded: false
    }

    case NearWallets.create_implicit_account(socket.assigns.wallet_id, account_data) do
      {:ok, _implicit_id} ->
        implicit_accounts =
          case NearWallets.get_wallet_implicit_accounts(socket.assigns.wallet_id) do
            {:ok, a} -> a
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:generating, false)
         |> assign(:show_generate_modal, false)
         |> assign(:implicit_accounts, implicit_accounts)
         |> put_flash(:info, "Implicit account generated successfully")}

      {:error, reason} ->
        Logger.error("Implicit account generation failed: #{inspect(reason)}")

        {:noreply,
         socket
         |> assign(:generating, false)
         |> assign(:form_errors, %{general: "Failed to generate account. Please try again."})}
    end
  end

  @impl true
  def handle_event("select_account", %{"implicit_id" => implicit_id}, socket) do
    account =
      Enum.find(socket.assigns.implicit_accounts, &(to_string(&1.implicit_id) == implicit_id))

    {:noreply, assign(socket, :selected_account, account)}
  end

  @impl true
  def handle_event("close_account_modal", _params, socket) do
    {:noreply, assign(socket, :selected_account, nil)}
  end

  @impl true
  def handle_event("mark_funded", %{"implicit_id" => implicit_id}, socket) do
    now = :calendar.universal_time()

    case NearWallets.mark_implicit_funded(implicit_id, now) do
      :ok ->
        implicit_accounts =
          case NearWallets.get_wallet_implicit_accounts(socket.assigns.wallet_id) do
            {:ok, a} -> a
            _ -> []
          end

        {:noreply,
         socket
         |> assign(:implicit_accounts, implicit_accounts)
         |> assign(:selected_account, nil)
         |> put_flash(:info, "Account marked as funded")}

      {:error, _} ->
        {:noreply, put_flash(socket, :error, "Failed to update account")}
    end
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    implicit_accounts =
      case NearWallets.get_wallet_implicit_accounts(socket.assigns.wallet_id) do
        {:ok, a} -> a
        _ -> []
      end

    {:noreply,
     socket
     |> assign(:implicit_accounts, implicit_accounts)
     |> put_flash(:info, "Accounts refreshed")}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  defp generate_implicit_account_id do
    :crypto.strong_rand_bytes(32)
    |> Base.encode16(case: :lower)
  end

  def truncate_account_id(nil), do: "—"
  def truncate_account_id(:undefined), do: "—"

  def truncate_account_id(id) do
    s = to_string(id)

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

  def format_datetime(%NaiveDateTime{} = dt),
    do: NaiveDateTime.to_string(dt) |> String.slice(0, 16)

  def format_datetime(%DateTime{} = dt), do: DateTime.to_string(dt) |> String.slice(0, 16)
  def format_datetime(_), do: "—"
end
