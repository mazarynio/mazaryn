defmodule MazarynWeb.MediaLive.MusicComponent do
  use Phoenix.LiveView

  require Logger

  alias Mazaryn.Schema.Media
  alias Account.Users

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info(user_id: user_id)
    {:ok, do_mount(user_id, socket)}
  end

  defp do_mount(email, socket) do
    media_changeset = Media.changeset(%Media{})

    {:ok, user} = Users.one_by_email(email)

    socket
    |> assign(media_changeset: media_changeset)
    |> assign(search: "")
    |> assign(user: user)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div></div>
    """
  end
end
