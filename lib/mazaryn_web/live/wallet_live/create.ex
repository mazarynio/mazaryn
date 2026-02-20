defmodule MazarynWeb.WalletLive.Create do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Mazaryn.SolanaWallets

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        {:ok,
         socket
         |> assign(:user, user)
         |> assign(:current_user, user)
         |> assign(:label, "")
         |> assign(:password, "")
         |> assign(:form_errors, %{})
         |> assign(:creating, false)}

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
  def handle_event("validate", params, socket) do
    label    = Map.get(params, "label", "")
    password = Map.get(params, "password", "")
    errors   = validate_form(%{label: label, password: password})

    {:noreply,
     socket
     |> assign(:label, label)
     |> assign(:password, password)
     |> assign(:form_errors, errors)}
  end

  @impl true
  def handle_event("create_wallet", params, socket) do
    label    = Map.get(params, "label", "")
    password = Map.get(params, "password", "")
    errors   = validate_form(%{label: label, password: password})

    if map_size(errors) > 0 do
      {:noreply,
       socket
       |> assign(:label, label)
       |> assign(:password, password)
       |> assign(:form_errors, errors)}
    else
      socket = assign(socket, :creating, true)
      user_id = to_string(socket.assigns.user.id)

      result =
        try do
          SolanaWallets.create_wallet(user_id, label, password)
        rescue
          e -> {:error, Exception.message(e)}
        end

      case result do
        {:ok, wallet} ->
          wallet_id = to_string(wallet.wallet_id || "")

          {:noreply,
           socket
           |> assign(:creating, false)
           |> put_flash(:info, "Wallet created successfully")
           |> push_navigate(to: "/#{socket.assigns.locale}/wallet/#{wallet_id}")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(:creating, false)
           |> assign(:form_errors, %{general: "Failed to create wallet: #{inspect(reason)}"})}
      end
    end
  end

  defp validate_form(%{label: label, password: password}) do
    errors = %{}

    errors =
      if String.trim(to_string(label)) == "",
        do: Map.put(errors, :label, "Label is required"),
        else: errors

    errors =
      if String.length(to_string(password)) < 8,
        do: Map.put(errors, :password, "Password must be at least 8 characters"),
        else: errors

    errors
  end
end
