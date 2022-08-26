defmodule MazarynWeb.HomeLive.PostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Live.Helper
  alias MazarynWeb.Component.SelectLive
  alias Home.Post
  alias Account.Users

  @impl true
  def mount(socket) do
    socket =
      socket
      |> assign(:uploaded_files, [])
      |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)

    {:ok, socket}
  end

  def get_user_avatar(author) do
    case Users.one_by_username(author) do
      {:ok, user} -> Helper.handle_avatar(user)
      _ -> "/images/default-user.png"
    end
  end
end
