defmodule MazarynWeb.ChannelLive.Show do
  use MazarynWeb, :live_view

  on_mount {MazarynWeb.UserLiveAuth, :user_resource}

  @impl true
  def mount(%{"unique_name" => _unique_name}, _session, socket) do
    {:ok,
     socket
     |> put_flash(:info, "Channels coming soon! For now, use Groups.")
     |> push_navigate(to: ~p"/#{socket.assigns.locale}/groups")}
  end

  @impl true
  def render(assigns) do
    ~H""
  end
end
