defmodule MazarynWeb.HomeLive.NavComponent do
  use MazarynWeb, :live_component
  import Phoenix.Component

  def mount(socket) do
    {:ok, socket}
  end

  def update(%{user: user} = assigns, socket) do
    notifs_count =
      user.id
      |> Core.NotifEvent.get_all_notifs()
      |> Enum.count()

    {:ok, socket |> assign(assigns) |> assign(notifs_count: notifs_count)}
  end
end
