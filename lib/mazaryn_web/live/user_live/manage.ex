defmodule MazarynWeb.UserLive.Manage do
  use MazarynWeb, :live_view

  require Logger

  # alias Account.User
  alias Account.Users
  alias ManageUser

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    {:ok, current_user} = Users.get_by_session_uuid(session_uuid)

    users_info_list = ManageUser.get_users_info()

    users = fetch_data(users_info_list)

    socket =
      socket
      |> assign(title: "Manage Users")
      |> assign(session_uuid: session_uuid)
      |> assign(current_user: current_user)
      |> assign(search: nil)
      |> assign(users: users |> sort_by_date())

    {:ok, assign(socket, %{})}
  end

  @impl true
  def handle_event("activate-user", %{"user-id" => id}, socket) do
    case check_if_is_admin(socket.assigns.current_user.username) do
      true -> ManageUser.verify_user(id, socket.assigns.current_user.username)
      false -> IO.inspect("not an admin")
    end

    users_info_list =
      ManageUser.get_users_info()

    users = fetch_data(users_info_list)

    socket =
      socket
      |> put_flash(:info, "successfully activated #{id}")
      |> assign(users: users |> sort_by_date())

    {:noreply, socket}
  end

  @impl true
  def handle_event("deactivate-user", %{"user-id" => id}, socket) do
    # ManageUser.unverify_user(id, "mazaryn")
    case check_if_is_admin(socket.assigns.current_user.username) do
      true -> ManageUser.unverify_user(id, socket.assigns.current_user.username)
      false -> IO.inspect("not an admin")
    end

    users_info_list = ManageUser.get_users_info()

    users = fetch_data(users_info_list)

    socket =
      socket
      |> put_flash(:info, "successfully activated #{id}")
      |> assign(users: users |> sort_by_date())

    {:noreply, socket}
  end

  @impl true
  def handle_event("unban-user", %{"user-id" => id}, socket) do
    ManageUser.unban_user(id)

    users_info_list = ManageUser.get_users_info()

    users = fetch_data(users_info_list)

    socket =
      socket
      |> put_flash(:info, "successfully activated #{id}")
      |> assign(users: users |> sort_by_date())

    {:noreply, socket}
  end

  @impl true
  def handle_event("ban-user", %{"user-id" => id}, socket) do
    ManageUser.ban_user(id)

    users_info_list = ManageUser.get_users_info()

    users = fetch_data(users_info_list)

    socket =
      socket
      |> put_flash(:info, "successfully activated #{id}")
      |> assign(users: users |> sort_by_date())

    {:noreply, socket}
  end

  @impl true
  def handle_event("unsuspend-user", %{"user-id" => id}, socket) do
    ManageUser.unsuspend_user(id)

    users_info_list = ManageUser.get_users_info()

    users = fetch_data(users_info_list)

    socket =
      socket
      |> put_flash(:info, "successfully activated #{id}")
      |> assign(users: users |> sort_by_date())

    {:noreply, socket}
  end

  @impl true
  def handle_event("suspend-user", %{"user-id" => id}, socket) do
    ManageUser.suspend_user(id)

    users_info_list = ManageUser.get_users_info()

    users = fetch_data(users_info_list)

    socket =
      socket
      |> put_flash(:info, "successfully activated #{id}")
      |> assign(users: users |> sort_by_date())

    {:noreply, socket}
  end

  def handle_event("do_search", %{"search" => search}, socket) do
    users = MazarynWeb.HomeLive.Home.search_user_by_username(search)

    {:noreply, assign(socket, search: search, users: users || [])}
  end

  def sort_by_date(list) when is_list(list) do
    Enum.sort(list, fn %{last_activity: date1}, %{last_activity: date2} ->
      NaiveDateTime.from_iso8601!(format_date(date1)) >=
        NaiveDateTime.from_iso8601!(format_date(date2))
    end)
  end

  def fetch_data(data) do
    Enum.map(data, fn
      {:user, id, p2p_node_address, ipfs_key, ai_user_id, business_id, ads_id, quantum_id, username, password_hash, email,
       address, knode, media, posts, blog_post, notif, following, follower, blocked, saved_posts,
       other_info, private, date_created, date_updated, avatar_url, banner_url, token_id, chat,
       verified, report, level, last_activity, suspend, data} ->
        naive_datetime = NaiveDateTime.from_erl!(last_activity)
        _human_readable_time = NaiveDateTime.to_string(naive_datetime)

        %{
          id: id,
          p2p_node_address: p2p_node_address,
          ipfs_key: ipfs_key,
          ai_user_id: ai_user_id,
          business_id: business_id,
          ads_id: ads_id,
          quantum_id: quantum_id,
          username: username,
          password: password_hash,
          email: email,
          address: address,
          knode: knode,
          media: media,
          posts: posts,
          blog_post: blog_post,
          following: following,
          follower: follower,
          blocked: blocked,
          saved_posts: saved_posts,
          other_info: other_info,
          private: private,
          date_created: date_created,
          date_updated: date_updated,
          avatar_url: avatar_url,
          banner_url: banner_url,
          token_id: token_id,
          notif: notif,
          chat: chat,
          verified: verified,
          report: report,
          level: level,
          last_activity: last_activity,
          suspend: suspend,
          data: data
        }
    end)
  end

  defp format_date(date) do
    date
    |> NaiveDateTime.from_erl!()
    |> NaiveDateTime.to_string()
  end

  @spec check_if_is_admin(String.t()) :: boolean()
  defp check_if_is_admin(admin_username) do
    ["arvand", "mazaryn", "zaryn"]
    |> Enum.member?(admin_username)
  end
end
