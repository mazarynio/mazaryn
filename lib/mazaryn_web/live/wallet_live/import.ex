defmodule MazarynWeb.WalletLive.Import do
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
         |> assign(:private_key, "")
         |> assign(:password, "")
         |> assign(:form_errors, %{})
         |> assign(:importing, false)}

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
    label       = Map.get(params, "label", "")
    private_key = Map.get(params, "private_key", "")
    password    = Map.get(params, "password", "")
    errors      = validate_form(%{label: label, private_key: private_key, password: password})

    {:noreply,
     socket
     |> assign(:label, label)
     |> assign(:private_key, private_key)
     |> assign(:password, password)
     |> assign(:form_errors, errors)}
  end

  @impl true
  def handle_event("import_wallet", params, socket) do
    label       = Map.get(params, "label", "")
    private_key = Map.get(params, "private_key", "")
    password    = Map.get(params, "password", "")
    errors      = validate_form(%{label: label, private_key: private_key, password: password})

    if map_size(errors) > 0 do
      {:noreply,
       socket
       |> assign(:label, label)
       |> assign(:private_key, private_key)
       |> assign(:password, password)
       |> assign(:form_errors, errors)}
    else
      socket = assign(socket, :importing, true)
      user_id = to_string(socket.assigns.user.id)

      result =
        try do
          SolanaWallets.import_wallet(user_id, private_key, label, password)
        rescue
          e -> {:error, Exception.message(e)}
        end

      case result do
        {:ok, wallet} ->
          wallet_id = to_string(wallet.wallet_id || "")

          {:noreply,
           socket
           |> assign(:importing, false)
           |> put_flash(:info, "Wallet imported successfully")
           |> push_navigate(to: "/#{socket.assigns.locale}/wallet/#{wallet_id}")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(:importing, false)
           |> assign(:form_errors, %{general: "Import failed: #{inspect(reason)}"})}
      end
    end
  end

  defp validate_form(%{label: label, private_key: private_key, password: password}) do
    errors = %{}

    errors =
      if String.trim(to_string(label)) == "",
        do: Map.put(errors, :label, "Label is required"),
        else: errors

    errors =
      if String.length(String.trim(to_string(private_key))) < 32,
        do: Map.put(errors, :private_key, "Enter a valid base58 private key"),
        else: errors

    errors =
      if String.length(to_string(password)) < 8,
        do: Map.put(errors, :password, "Password must be at least 8 characters"),
        else: errors

    errors
  end

  @impl true
  def render(assigns) do
    ~H"""
    <.live_component
      module={MazarynWeb.HomeLive.NavComponent}
      id="navigation"
      user={@current_user}
      search=""
      locale={@locale}
    />

    <div class="min-h-screen bg-gradient-to-br from-slate-950 via-slate-900 to-indigo-950 flex items-center justify-center px-6 py-10">
      <div class="w-full max-w-md">

        <div class="flex items-center gap-3 mb-8">
          <.link
            navigate={~p"/#{@locale}/wallet"}
            class="flex items-center gap-2 px-4 py-2 rounded-xl bg-slate-800 text-slate-300 hover:bg-slate-700 hover:text-white transition-all text-sm font-medium"
          >
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 19l-7-7m0 0l7-7m-7 7h18" />
            </svg>
            Back
          </.link>
        </div>

        <div class="bg-slate-900/60 border border-slate-800 rounded-3xl p-8 shadow-2xl">
          <div class="flex items-center gap-3 mb-6">
            <div class="w-12 h-12 bg-gradient-to-br from-blue-500 to-cyan-600 rounded-2xl flex items-center justify-center shadow-lg shadow-blue-900/50">
              <svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4-4m0 0L8 8m4-4v12" />
              </svg>
            </div>
            <div>
              <h1 class="text-2xl font-bold text-white">Import Wallet</h1>
              <p class="text-slate-400 text-sm">Import an existing Solana wallet via private key</p>
            </div>
          </div>

          <div class="mb-5 px-4 py-3 bg-amber-500/10 border border-amber-500/20 rounded-xl">
            <div class="flex items-start gap-2">
              <svg class="w-4 h-4 text-amber-400 mt-0.5 flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
              </svg>
              <p class="text-amber-300 text-xs leading-relaxed">
                Never share your private key with anyone. Only import keys on trusted devices.
              </p>
            </div>
          </div>

          <%= if Map.get(@form_errors, :general) do %>
            <div class="mb-5 px-4 py-3 bg-red-500/10 border border-red-500/20 rounded-xl">
              <p class="text-red-400 text-sm"><%= @form_errors.general %></p>
            </div>
          <% end %>

          <form phx-submit="import_wallet" phx-change="validate" class="space-y-5">
            <div>
              <label class="block text-sm font-medium text-slate-300 mb-2">Wallet Label</label>
              <input
                type="text"
                name="label"
                value={@label}
                placeholder="e.g. Imported Wallet"
                autofocus
                class={"w-full px-4 py-3 bg-slate-800 border rounded-xl text-white placeholder-slate-500 focus:outline-none focus:ring-2 focus:ring-blue-500 transition-all #{if Map.get(@form_errors, :label), do: "border-red-500", else: "border-slate-700"}"}
              />
              <%= if Map.get(@form_errors, :label) do %>
                <p class="mt-1.5 text-red-400 text-xs"><%= @form_errors.label %></p>
              <% end %>
            </div>

            <div>
              <label class="block text-sm font-medium text-slate-300 mb-2">Private Key (Base58)</label>
              <textarea
                name="private_key"
                rows="3"
                placeholder="Enter your base58-encoded private key..."
                class={"w-full px-4 py-3 bg-slate-800 border rounded-xl text-white placeholder-slate-500 font-mono text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 transition-all resize-none #{if Map.get(@form_errors, :private_key), do: "border-red-500", else: "border-slate-700"}"}
              ><%= @private_key %></textarea>
              <%= if Map.get(@form_errors, :private_key) do %>
                <p class="mt-1.5 text-red-400 text-xs"><%= @form_errors.private_key %></p>
              <% end %>
            </div>

            <div>
              <label class="block text-sm font-medium text-slate-300 mb-2">Encryption Password</label>
              <input
                type="password"
                name="password"
                value={@password}
                placeholder="Min. 8 characters"
                class={"w-full px-4 py-3 bg-slate-800 border rounded-xl text-white placeholder-slate-500 focus:outline-none focus:ring-2 focus:ring-blue-500 transition-all #{if Map.get(@form_errors, :password), do: "border-red-500", else: "border-slate-700"}"}
              />
              <%= if Map.get(@form_errors, :password) do %>
                <p class="mt-1.5 text-red-400 text-xs"><%= @form_errors.password %></p>
              <% end %>
              <p class="mt-2 text-slate-500 text-xs leading-relaxed">
                This password re-encrypts your private key locally. It cannot be recovered if lost.
              </p>
            </div>

            <div class="pt-1">
              <button
                type="submit"
                disabled={@importing}
                class="w-full px-4 py-3.5 bg-gradient-to-r from-blue-600 to-cyan-600 text-white font-bold rounded-xl hover:from-blue-700 hover:to-cyan-700 transition-all shadow-lg shadow-blue-900/30 disabled:opacity-60 disabled:cursor-not-allowed text-sm"
              >
                <%= if @importing do %>
                  <span class="flex items-center justify-center gap-2">
                    <svg class="animate-spin w-4 h-4" fill="none" viewBox="0 0 24 24">
                      <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
                      <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4z"></path>
                    </svg>
                    Importing...
                  </span>
                <% else %>
                  Import Wallet
                <% end %>
              </button>
            </div>
          </form>

          <div class="mt-6 pt-5 border-t border-slate-800 text-center">
            <p class="text-slate-500 text-sm">
              Don't have a wallet yet?
              <.link navigate={~p"/#{@locale}/wallet/create"} class="text-purple-400 hover:text-purple-300 font-medium">Create one</.link>
            </p>
          </div>
        </div>

      </div>
    </div>
    """
  end
end
