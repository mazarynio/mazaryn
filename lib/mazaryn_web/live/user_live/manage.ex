defmodule MazarynWeb.UserLive.Manage do
  use MazarynWeb, :live_view

  require Logger

  alias Account.User
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

    ManageUser.verify_user(id, "mazaryn")
    |> IO.inspect(label: "[ACTIVATE USER]")

    users_info_list = ManageUser.get_users_info()

    users = fetch_data(users_info_list)

    socket =
      socket
      |> put_flash(:info, "successfully activated #{id}")
      |> assign(users: users |> sort_by_date())

      {:noreply, socket}
  end

  @impl true
  def handle_event("deactivate-user", %{"user-id" => id}, socket) do
    ManageUser.unverify_user(id, "mazaryn")
    |> IO.inspect(label: "[ACTIVATE USER]")

    users_info_list = ManageUser.get_users_info()

    users = fetch_data(users_info_list)

    socket =
      socket
      |> put_flash(:info, "successfully activated #{id}")
      |> assign(users: users |> sort_by_date())

      {:noreply, socket}
  end

  def sort_by_date(list) when is_list(list) do
    Enum.sort(list, fn %{last_activity: date1}, %{last_activity: date2} ->
      NaiveDateTime.from_iso8601!(date2) >= NaiveDateTime.from_iso8601!(date1)
    end)
  end

  def fetch_data(data) do
    Enum.map(data, fn
      {:user, id, username, password_hash, email, address, knode, media, post, blog_post, notif,
       following, follower, blocked, saved_posts, other_info, private, date_created, date_updated,
       avatar_url, banner_url, token_id, chat, verified, report, level, last_activity, suspend,
       data} ->
        naive_datetime = NaiveDateTime.from_erl!(last_activity)
        human_readable_time = NaiveDateTime.to_string(naive_datetime)

        %{
          id: id,
          username: username,
          password: password_hash,
          email: email,
          address: address,
          knode: knode,
          media: media,
          post: post,
          blog_post: blog_post,
          notif: notif,
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
          chat: chat,
          verified: verified,
          report: report,
          level: level,
          last_activity: human_readable_time,
          suspend: suspend,
          data: data
        }
    end)
  end
end
