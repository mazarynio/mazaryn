defmodule DashboardLive.Index do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper
  import Logger

  alias Account.Users

  # case reload dashboard page
  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info(user_id: user_id)
    {:ok, do_mount(user_id, socket)}
  end

end
