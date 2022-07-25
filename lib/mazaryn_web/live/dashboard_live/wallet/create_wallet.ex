defmodule DashboardLive.Wallet.CreateWallet do
  use MazarynWeb, :live_view

  alias Dashboard.Wallet

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.form let={f} for={@changeset} phx-change="validate-wallet" phx-submit="save-wallet" phx-target={@myself} multipart class="flex flex-col justify-between align-center py-5">
        <div class="flex items-center px-5">
          <img class="h-11 w-11 rounded-full" src="https://placeimg.com/192/192/people" />
          <div class="ml-4 text-sm leading-tight w-full">
            <%= textarea f, :name, class: "w-full border-none resize-none text-gray-300 font-normal block", placeholder: "Write a comment" %>
              <span class="text-sm"><%= push_error_tag f, :name %></span>
          </div>
        </div>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate-wallet", %{"wallet" => wallet_params} = _params, socket) do
    wallet_params = Map.put(wallet_params, "user_id", socket.assigns.user_id)

    changeset =
      %Wallet{}
      |> Wallet.changeset(wallet_params)
      |> Ecto.Changeset.put_change(:user_id, socket.assigns.user_id)
      |> Map.put(:action, :validate)

    socket =
      socket
      |> assign(:changeset, changeset)

    {:noreply, socket}
  end

  def handle_event("save-wallet", %{"wallet" => wallet_params} = _params, socket) do
    wallet_params = Map.put(wallet_params, "user_id", socket.assigns.user_id)
    changeset = Wallet.changeset(%Wallet{}, wallet_params)

    case Wallet.create_wallet(changeset) do
      %Wallet{} ->
        {:noreply, socket}

      _other ->
        {:noreply, socket}
    end
  end
end
