defmodule MazarynWeb.SearchLive.Index do
  use MazarynWeb, :live_view

  alias Account.Users

  @impl true
  def mount(_params, %{"user_id" => email} = _session, socket) do
    {:ok, user} = user_info(email)
    {:ok, assign(socket, :user, user)}
  end

  defp user_info(email) do
    Users.one_by_email(email)
  end

  defp users_by_search do

  end
end
