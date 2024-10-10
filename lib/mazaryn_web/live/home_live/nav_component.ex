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

    {:ok,
     socket |> assign(assigns) |> assign(notifs_count: notifs_count) |> assign(current_path: "")}
  end

  def handle_params(_params, url, socket) do
    IO.inspect("thisflkajflaj")
    socket = assign(socket, current_path: URI.parse(url).path)
    handle_path(socket)
    #  IO.inspect(URI.parse(url).path, label: "path in the nav component")
    # {:noreply, socket}
  end

  def handle_path(socket) do
    {:noreply, socket}
  end
end
