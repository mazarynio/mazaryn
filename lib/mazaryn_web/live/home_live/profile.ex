defmodule MazarynWeb.HomeLive.Profile do
  use MazarynWeb, :live_view

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do

    {:ok, assign(socket, user_id: user_id)}
  end

  @impl true
  def handle_params(%{"user" => user}, _, socket) do
    {:noreply,
     socket
     }
  end

  @impl true
  def handle_params(_, _, socket) do
    {:noreply, socket}
  end
end
