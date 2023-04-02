defmodule MazarynWeb.HomeLive.Approve do
  use MazarynWeb, :live_view
  alias Core.PostClient, as: PostClient

  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info(user_id: user_id)
    {:ok, do_mount(user_id, socket)}
  end

  # case redirect form login, signup
  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    {:ok, do_mount(get_user_id(session_uuid), socket)}
  end

  @impl true
  def handle_event("messages", _param, socket) do
    random_id = "/messages/" <> "1"
    {:noreply, push_redirect(socket, to: random_id)}
  end

  defp do_mount(email, socket) do
    {:ok, user} = Users.one_by_email(email)

    socket
    |> assign(search: "")
    |> assign(user: user)
  end
end
