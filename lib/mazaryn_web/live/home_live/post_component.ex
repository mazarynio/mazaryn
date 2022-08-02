defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Component.SelectLive
  alias Home.Post

  @impl true
  def mount(socket) do
    socket =
      socket
      |> assign(:uploaded_files, [])
      |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)

    {:ok, socket}
  end
end
