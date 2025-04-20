defmodule MazarynWeb.HomeLive.Notification do
  use MazarynWeb, :live_view
  alias Account.Users
  alias Core.NotifEvent

  @impl true
  def mount(_params, %{"user_id" => user_email} = _session, socket) do
    Process.send_after(self(), :time_diff, 1000)
    {:ok, user} = Users.one_by_email(user_email)

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "user:#{user.id}:notifications")
    end

    {:ok,
     socket
     |> assign(target_user: user)
     |> assign(search: "")
     |> assign(notifs: get_all_user_notifs(user))
     |> assign(notification_visible: true)
     |> assign(notification_count: NotifEvent.unread_count(user.id))}
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
      notification_count={@notification_count}
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
          />
        </div>
        <div class="w-full lg:w-[54%] py-6 pl-11 pr-8">
          <div class="flex flex-wrap justify-center align-center mb-6">
            <div class="w-full bg-white custom-box-shadow pr-[1.35rem] pl-[1.6rem] pb-2 pt-5 mt-8 rounded-[20px]">
              <button phx-click="toggle_notification" class="text-right w-full text-xs text-gray-500">
                <%= if @notification_visible, do: "Hide Notifications", else: "Show Notifications" %>
              </button>
              <%= if @notification_visible do %>
                <%= if Enum.empty?(@notifs) do %>
                  <div class="flex justify-center align-center items-center py-8">
                    <p class="text-gray-500">No notifications yet</p>
                  </div>
                <% else %>
                  <%= for {user, message, time_passed, timestamp} <- @notifs do %>
                    <div class="flex justify-between align-center items-center mb-5">
                      <div class="flex justify-center items-center">
                        <img
                          class="h-11 w-11 rounded-full"
                          src={user.avatar_url || "/images/default-user.svg"}
                        />
                        <div class="ml-3.5 text-sm leading-tight mt-5">
                          <span class="block text-[#60616D] text-sm">
                            <a class="text-blue-500"
                               href={
                                 Routes.live_path(
                                   @socket,
                                   MazarynWeb.UserLive.Profile,
                                   user.username,
                                   @locale
                                 )
                               }>
                              <%= user.username %>
                            </a>
                          </span>
                          <span class="block text-[#60616D] text-sm"><%= message %></span>
                          <span class="block text-[#60616D] text-sm"><%= time_passed %></span>
                        </div>
                      </div>
                    </div>
                  <% end %>
                <% end %>
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
      |> Enum.map(fn {user, message, _old_time_passed, time_stamp} ->
        time_passed = time_passed(time_stamp)
        {user, message, time_passed, time_stamp}
      end)

    Process.send_after(self(), :time_diff, 1000)
    {:noreply, assign(socket, :notifs, notifs)}
  end

  @impl true
  def handle_info({:notification_update}, socket) do
    notification_count = NotifEvent.unread_count(socket.assigns.target_user.id)

    notifs =
      if socket.assigns.notification_visible do
        get_all_user_notifs(socket.assigns.target_user)
      else
        socket.assigns.notifs
      end

    {:noreply,
     socket
     |> assign(:notification_count, notification_count)
     |> assign(:notifs, notifs)}
  end

  @impl true
  def handle_event("toggle_notification", _params, socket) do
    new_visibility = !socket.assigns.notification_visible

    if new_visibility do
      user_id = socket.assigns.target_user.id

      # ✅ FIXED: Properly mark notifications as read
      NotifEvent.mark_all_as_read(user_id)

      notification_count = 0

      Phoenix.PubSub.broadcast(
        Mazaryn.PubSub,
        "user:#{user_id}:notifications",
        {:notification_update}
      )

      {:noreply,
       socket
       |> assign(:notification_visible, new_visibility)
       |> assign(:notification_count, notification_count)
       |> assign(:notifs, [])}  # Clear notifications if needed
    else
      {:noreply, assign(socket, :notification_visible, new_visibility)}
    end
  end

  defp get_all_user_notifs(user) do
    user.id
    |> NotifEvent.get_all_notifs()
    |> Enum.map(fn {:notif, notifid, actor_id, target_id, message, time_stamp, _metadata} ->
      {:ok, user} = get_user(actor_id, target_id)
      time_passed = time_passed(time_stamp)
      {user, message, time_passed, time_stamp}
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

  defp time_difference([h | t], date_time) do
    case Timex.diff(Timex.now(), date_time, h) do
      0 -> time_difference(t, date_time)
      diff -> "#{diff} #{h} ago"
    end
  end

  defp get_user(actor_id, target_id) do
    id =
      case actor_id do
        :undefined -> target_id
        _ -> actor_id
      end

    Users.one_by_id(id)
  end
end

