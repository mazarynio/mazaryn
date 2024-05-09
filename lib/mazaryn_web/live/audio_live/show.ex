defmodule MazarynWeb.AudioLive.Show do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper

  alias Account.Users

  @type socket :: Phoenix.LiveView.Socket.t()

  @impl Phoenix.LiveView
  @spec mount(map(), map(), socket()) :: {:ok, socket()}
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    {:ok, do_mount(user_id, socket)}
  end

  # In case of redirect from login, signup
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    {:ok, do_mount(get_user_id(session_uuid), socket)}
  end

  defp do_mount(email, socket) do
    {:ok, user} = Users.one_by_email(email)

    socket
    |> assign(search: "")
    |> assign(user: user)
  end
end
