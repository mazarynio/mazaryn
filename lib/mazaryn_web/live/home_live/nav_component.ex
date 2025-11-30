defmodule MazarynWeb.HomeLive.NavComponent do
  use MazarynWeb, :live_component
  import Phoenix.Component

  def mount(socket) do
    {:ok, socket}
  end

  def update(%{user: user} = assigns, socket) do
    notifs_count = Core.NotifEvent.count_unread(user.id)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(notifs_count: notifs_count)
     |> assign(current_path: "")
     |> assign_new(:search, fn -> "" end)}
  end

  def handle_path(socket) do
    {:noreply, socket}
  end
end
