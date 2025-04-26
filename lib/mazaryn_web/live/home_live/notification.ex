defmodule MazarynWeb.HomeLive.Notification do
  use MazarynWeb, :live_view
  alias Account.Users

  @impl true
  def mount(_params, %{"user_id" => user_email} = _session, socket) do
    Process.send_after(self(), :time_diff, 1000)
    {:ok, user} = Users.one_by_email(user_email)
    
    all_notifs = Core.NotifEvent.get_all_notifs(user.id)
    {chat_notifs, general_notifs} = split_notifications(all_notifs)
    
    formatted_general_notifs = format_notifications(general_notifs)
    mark_notifications_as_read(general_notifs)
   
    chat_notifs_count = length(chat_notifs)
    general_notifs_count = length(general_notifs)
    
    send_update(MazarynWeb.HomeLive.NavComponent, id: "navigation", 
      user: user, 
      general_notifs_count: general_notifs_count)
      
    send_update(MazarynWeb.HomeLive.LeftSidebarComponent, id: "leftsidebar", 
      user: user, 
      chat_notifs_count: chat_notifs_count)

    {:ok,
     socket
     |> assign(target_user: user)
     |> assign(search: "")
     |> assign(notifs: formatted_general_notifs)
     |> assign(chat_notifs_count: chat_notifs_count)
     |> assign(general_notifs_count: general_notifs_count)}
  end

  @impl true
  def handle_params(_params, url, socket) do
    socket = assign(socket, current_path: URI.parse(url).path)
    MazarynWeb.HomeLive.NavComponent.handle_path(socket)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <!-- navigation -->
    <.live_component
      module={MazarynWeb.HomeLive.NavComponent}
      id="navigation"
      user={@target_user}
      search={@search}
      locale={@locale}
      general_notifs_count={@general_notifs_count}
    />
    <!-- Three columns -->
    <div class="bg-[#FAFAFA]">
      <div class="flex flex-wrap w-[90%] max-w-[1440px] mx-auto">
        <div class="w-full lg:w-[18%] py-6">
          <.live_component
            module={MazarynWeb.HomeLive.LeftSidebarComponent}
            id="leftsidebar"
            user={@target_user}
            locale={@locale}
            chat_notifs_count={@chat_notifs_count}
          />
        </div>

        <div class="w-full lg:w-[54%] py-6 pl-11 pr-8">
          <div class="flex flex-wrap justify-center align-center mb-6">
            <div class="w-full bg-white white:bg-gray-800 custom-box-shadow pr-[1.35rem] pl-[1.6rem] pb-2 pt-5 mt-8 rounded-[20px]">
              <%= for {user, message, time_passed, _time_stamp} <- @notifs do %>
                <div class="flex justify-between align-center items-center mb-5">
                  <div class="flex justify-center items-center">
                    <img
                      class="h-11 w-11 rounded-full"
                      src={user.avatar_url || "/images/default-user.svg"}
                    />
                    <div class="ml-3.5 text-sm leading-tight mt-5">
                      <span class="block text-[#60616D] text-sm">
                        <a
                          class="text-blue-500"
                          href={
                            Routes.live_path(
                              @socket,
                              MazarynWeb.UserLive.Profile,
                              user.username,
                              @locale
                            )
                          }
                        >
                          <%= user.username %>
                        </a>
                      </span>
                      <span class="block text-[#60616D] text-sm"><%= message %></span>
                      <span class="block text-[#60616D] text-sm"><%= time_passed %></span>
                    </div>
                  </div>
                </div>
              <% end %>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_info(:time_diff, socket) do
    notifs =
      socket.assigns.notifs
      |> Enum.map(fn {user, message, _time_passed, time_stamp} ->
        time_passed = time_passed(time_stamp)
        {user, message, time_passed, time_stamp}
      end)

    Process.send_after(self(), :time_diff, 1000)

    {:noreply, assign(socket, :notifs, notifs)}
  end
  
  defp split_notifications(all_notifs) do
    Enum.split_with(all_notifs, fn notification_tuple -> 
      {:notif, _notif_id, _actor_id, _target_id, message, _time_stamp, _read, metadata, _extra} = notification_tuple
      
      is_chat_notification?(message, metadata)
    end)
  end
  
  defp is_chat_notification?(message, metadata) do
    chat_type = if is_map(metadata) || is_list(metadata) do
      metadata[:type] == "chat_message"
    else
     false
   end
   message_contains_chat = String.contains?(message, ["sent you a message", "messaged you", "chat"])
   chat_type || message_contains_chat
  end
  
  defp format_notifications(notifs) do
    Enum.map(notifs, fn {:notif, _notif_id, actor_id, target_id, message, time_stamp, _read, _metadata, _extra} ->
      {:ok, user} = get_user(actor_id, target_id)
      time_passed = time_passed(time_stamp)
      {user, message, time_passed, time_stamp}
    end)
  end
  
  # Mark notifications as read
  defp mark_notifications_as_read(notifs) do
    Enum.each(notifs, fn {:notif, notif_id, _actor_id, _target_id, _message, _time_stamp, _read, _metadata, _extra} ->
      Core.NotifEvent.mark_notif_as_read(notif_id)
    end)
  end

  defp time_passed(time_stamp) do
    date_time = Timex.to_datetime(time_stamp)
    time_difference([:years, :months, :weeks, :days, :hours, :minutes, :seconds], date_time)
  end

  defp time_difference([:seconds], date_time) do
    case Timex.diff(Timex.now(), date_time, :seconds) do
      0 -> "Now"
      diff -> "#{diff} seconds ago"
    end
  end

  defp time_difference([h | t] = _granulaties, date_time) do
    case Timex.diff(Timex.now(), date_time, h) do
      0 -> time_difference(t, date_time)
      diff -> "#{diff} #{h} ago"
    end
  end

  defp get_user(actor_id, target_id) do
    id =
      case actor_id do
        :undefined -> target_id
        actor_id -> actor_id
      end

    Users.one_by_id(id)
  end
end
