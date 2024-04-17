defmodule MazarynWeb.HomeLive.Notification do
  use MazarynWeb, :live_view
  alias Account.Users

  # case reload home page
  @impl true
  def mount(_params, %{"user_id" => user_email} = _session, socket) do
    {:ok, user} = Users.one_by_email(user_email)

    {:ok,
     socket
     |> assign(user: user)
     |> assign(search: "")
     |> assign(notifs: get_all_user_notifs(user))}
  end

  def render(assigns) do
    ~H"""
    <!-- navigation -->
    <.live_component
      module={MazarynWeb.HomeLive.NavComponent}
      id="navigation"
      user={@user}
      search={@search}
    />
    <!-- Three columns -->
    <div class="bg-[#FAFAFA]">
      <div class="flex flex-wrap w-[90%] max-w-[1440px] mx-auto">
        <div class="w-full lg:w-[18%] py-6">
          <.live_component
            module={MazarynWeb.HomeLive.LeftSidebarComponent}
            id="leftsidebar"
            user={@user}
          />
        </div>
        <div class="w-full lg:w-[54%] py-6 pl-11 pr-8">
          <div class="flex flex-wrap justify-center align-center mb-6">
            <ol>
              <%= for notif <- @notifs do %>
                <li><%= notif %></li>
              <% end %>
            </ol>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp get_all_user_notifs(user) do
    user.id
    |> Core.NotifEvent.get_all_notifs()
    |> Enum.map(fn {:notif, _notif_id, _user_id, message, _time_stamp, _metadata} ->
      message
    end)
  end
end
