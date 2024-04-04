defmodule MazarynWeb.HomeLive.Notification do
  use MazarynWeb, :live_view
  alias Account.Users

  # case reload home page
  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    {:ok, user} = Users.one_by_email(user_id)

    IO.inspect(user, label: "=============================")

    Core.NotifEvent.get_all_notifs(user_id)
    |> IO.inspect(label: "-------------------")

    {:ok,
     socket
     |> assign(user: user)
     |> assign(search: "")}
  end
end
