defmodule MazarynWeb.UserLive.Index do
  use MazarynWeb, :live_view

  alias Core.UserClient, as: UserClient

  def mounth(_params, _session, socket) do
    # Get the Users from the database
    {:ok, assign(socket, :users, get_user())}
  end

  defp get_user, do: UserClient.getting_users()
end
