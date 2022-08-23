defmodule MazarynWeb.SearchLive.Index do
  use MazarynWeb, :live_view

  alias Account.Users

  @impl true
  def mount(_params, %{"user_id" => email} = _session, socket) do
    {:ok, user} = user_info(email)
    found_users = users_by_search()

    socket =
      socket
      |> assign(:user, user)
      |> assign(:found_users, found_users)

    {:ok, socket}
  end

  defp user_info(email) do
    Users.one_by_email(email)
  end

  defp users_by_search do
    # temp
    Users.get_all_users()
  end
end
