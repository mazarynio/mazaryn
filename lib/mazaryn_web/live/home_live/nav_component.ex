defmodule MazarynWeb.HomeLive.NavComponent do
  use MazarynWeb, :live_component
  import Phoenix.Component
  
  def mount(socket) do
    {:ok, socket}
  end
  
  def update(%{user: user} = assigns, socket) do
    all_notifs = Core.NotifEvent.get_all_unread(user.id)
    
    general_notifs_count = if Map.has_key?(assigns, :general_notifs_count) do
      assigns.general_notifs_count
    else
      
      {chat_notifs, general_notifs} = split_notifications(all_notifs)
      length(general_notifs)
    end
    
    {:ok,
     socket 
     |> assign(assigns) 
     |> assign(general_notifs_count: general_notifs_count) 
     |> assign(current_path: "")}
  end
  
  def handle_path(socket) do
    {:noreply, socket}
  end
  
  defp split_notifications(all_notifs) do
    Enum.split_with(all_notifs, fn notification_tuple -> 
      {:notif, _notif_id, _actor_id, _target_id, message, _time_stamp, _read, metadata, _extra} = notification_tuple
      
      is_chat_notification?(message, metadata)
    end)
  end
  
  defp is_chat_notification?(message, metadata) do

    chat_type = metadata[:type] == "chat_message"
    message_contains_chat = String.contains?(message, ["sent you a message", "messaged you", "chat"])
    
    chat_type || message_contains_chat
  end
end
