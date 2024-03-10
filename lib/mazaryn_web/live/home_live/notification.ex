defmodule MazarynWeb.HomeLive.Notification do
  use MazarynWeb, :live_view
  alias Account.Users

  # case reload home page
  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    {:ok, user} = Users.one_by_email(user_id)

    {:ok,
     socket
     |> assign(user: user)
     |> assign(search: "")}
  end
end
