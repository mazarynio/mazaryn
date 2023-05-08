defmodule MazarynWeb.UserLive.DeleteProfileComponent do
  use MazarynWeb, :live_component

  import MazarynWeb.Live.Helper
  alias Phoenix.LiveView.JS

  @impl true
  def mount(socket) do
    {:ok,socket}
  end


end
