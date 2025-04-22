defmodule MazarynWeb.HomeLive.NavComponent do
  use MazarynWeb, :live_component
  import Phoenix.Component
  require Logger
  
  def mount(socket) do
    {:ok, socket}
  end
  
  def update(%{user: user} = assigns, socket) do
    unread_count =
      user.id
      |> Core.NotifEvent.get_all_notifs()
      |> Enum.filter(fn {:notif, _, _, _, _, _, viewed, _} -> not viewed end)
      |> Enum.count()
      
    {:ok,
     socket
     |> assign(assigns)
     |> assign(notifs_count: unread_count)
     |> assign(current_path: "")}
  end
  
  def handle_path(socket) do
    {:noreply, socket}
  end
  
  @impl true
  def handle_event("toggle_notification", _params, socket) do
    user_id = socket.assigns.user.id
    Logger.info("Marking notifications as read for user: #{inspect(user_id)}")
    
    # Mark all notifications as read
    Core.NotifEvent.mark_all_as_read(user_id)
    
    # Broadcast notification update
    Phoenix.PubSub.broadcast(
      Mazaryn.PubSub,
      "user:#{user_id}:notifications",
      {:notification_update}
    )
    
    # Set notification count to 0 to make badge disappear
    {:noreply, assign(socket, :notifs_count, 0)}
  end
  
  @impl true
  def handle_info({:notification_update}, socket) do
    user_id = socket.assigns.user.id
    # Recalculate unread count
    unread_count =
      user_id
      |> Core.NotifEvent.get_all_notifs()
      |> Enum.filter(fn {:notif, _, _, _, _, _, viewed, _} -> not viewed end)
      |> Enum.count()
      
    {:noreply, assign(socket, :notifs_count, unread_count)}
  end
end
