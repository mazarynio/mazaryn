defmodule MazarynWeb.UserLive.Manage do
  use MazarynWeb, :live_view

  require Logger

  alias Account.User
  alias Account.Users
  alias ManageUser

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    {:ok, current_user} = Users.get_by_session_uuid(session_uuid)

    # users = Users.get_all_users() |> IO.inspect(label: "mbogi")
    users_info_list = ManageUser.get_users_info();
    # users_info_list |> IO.inspect(label: "mbogi")

    users = fetch_data(users_info_list)
    users |> IO.inspect(label: "genje mbogi")
    socket =
      socket
      |> assign(title: "Manage Users")
      |> assign(session_uuid: session_uuid)
      |> assign(current_user: current_user)
      |> assign(search: nil)
      |> assign(users: users)

    {:ok, assign(socket, %{})}
  end

  def fetch_data(data) do
    data |> IO.inspect(label: "kekekeke")

    Enum.map(data, fn
      {:user, _,username,password_hash,email,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_} -> %{email: email, username: username, password_hash: password_hash}
      # {:user,
      #  _, _, _, email, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} -> email
    end)
  end

end
