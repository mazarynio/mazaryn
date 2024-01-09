defmodule MazarynWeb.UserBlog.Index do
  use MazarynWeb, :live_view

  alias Account.Users

  alias Account.User
  alias Mazaryn.Search

  @impl true
  def mount(_params, %{"user_id" => email} = _session, socket) do
    {:ok, user} = user_info(email)

    {:atomic, found_users} = Search.user_search(user.username)

    found_users =
      found_users
      |> Enum.map(fn user ->
        {:ok, user_struct} =
          user
          |> User.erl_changeset()
          |> User.build()

        user_struct
      end)

    socket =
      socket
      |> assign(:user, user)
      |> assign(:search, [])
      |> assign(:found_users, found_users)

    {:ok, socket}
  end

  defp user_info(email) do
    Users.one_by_email(email)
  end
end
