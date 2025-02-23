defmodule MazarynWeb.UserLive.Profile do
  use MazarynWeb, :live_view

  require Logger

  import MazarynWeb.Live.Helper
  import Phoenix.HTML.Form
  import Phoenix.Component
  alias Core.UserClient
  alias Account.User
  alias Account.Users
  alias Mazaryn.Schema.Post
  alias Core.PostClient
  alias Mazaryn.Posts
  alias Phoenix.LiveView.JS
  alias MazarynWeb.Router.Helpers, as: Routes

  # this is
  @impl true
  def mount(
        %{"username" => username} = _params,
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    {:ok, current_user} = Users.get_by_session_uuid(session_uuid)

    post_changeset = Post.changeset(%Post{})

    {:ok, user} = get_user_by_username(username)
    IO.inspect(user, label: "user--->")
    # {:ok, user} = get_user_by_username(current_user.username)

    user_changeset = User.changeset(user)

    privacy = if user.private, do: "private", else: "public"

    socket =
      socket
      |> assign(session_uuid: session_uuid)
      |> assign(post_changeset: post_changeset)
      |> assign(user_changeset: user_changeset)
      |> assign(user: user)
      |> assign(current_user: current_user)
      |> handle_assigns(current_user.id, username)
      |> assign(edit_action: false)
      |> assign(follower_action: false)
      |> assign(follows_action: false)
      |> assign(form: to_form(user_changeset))
      |> assign(privacy: privacy)
      |> assign(report_user_action: false)
      |> assign(verified_action: false)
      |> assign(admins: ["arvand"])
      |> assign(results: [])
      |> assign(followers: [])

    {:ok, socket}
  end

  def mount(
        _params,
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    {:ok, current_user} = Users.get_by_session_uuid(session_uuid)

    post_changeset = Post.changeset(%Post{})
    user_changeset = User.changeset(%User{})

    socket =
      socket
      |> assign(session_uuid: session_uuid)
      |> assign(posts: PostClient.get_posts_by_author(current_user.username))
      |> assign(post_changeset: post_changeset)
      |> assign(user_changeset: user_changeset)
      |> assign(current_user: current_user)
      |> assign(results: [])

    {:ok, socket}
  end

  @impl true
  def handle_info(:reload_posts, socket) do
    current_user = socket.assigns.current_user

    posts = Posts.get_posts_by_author(current_user.username)
    {:noreply, assign(socket, posts: posts)}
  end

  @impl true
  def handle_params(%{"username" => username} = _params, _uri, socket) do
    current_user = socket.assigns.current_user
    post_changeset = Post.changeset(%Post{})
    user_changeset = User.changeset(%User{})

    posts = Mazaryn.Posts.get_posts_by_author(username)

    # {:ok, user} = get_user_by_username(current_user.username)
    {:ok, user} = get_user_by_username(username)

    socket =
      socket
      |> assign(post_changeset: post_changeset)
      |> assign(user_changeset: user_changeset)
      |> assign(user: user)
      |> assign(search: nil)
      |> assign(current_user: current_user)
      |> assign(posts: posts)
      |> handle_assigns(current_user.id, user.id)

    {:noreply, socket}
  end

  def handle_params(_params, _uri, socket) do
    current_user = socket.assigns.current_user
    post_changeset = Post.changeset(%Post{})
    user_changeset = User.changeset(%User{})

    socket =
      socket
      |> assign(post_changeset: post_changeset)
      |> assign(user_changeset: user_changeset)
      |> assign(search: nil)
      |> assign(user: current_user)
      |> assign(current_user: current_user)

    {:noreply, socket}
  end

  @impl true
  def handle_event("do_search", %{"search" => search}, socket) do
    user = search_user_by_username(search)
    {:noreply, assign(socket, search: search, results: user || [])}
  end

  @impl true
  def handle_event("follow_user", %{"userid" => id}, socket) do
    id = to_charlist(id)
    user_id = socket.assigns.current_user.id
    UserClient.follow(user_id, id)

    Core.NotifEvent.follow(user_id, id)

    {:noreply, handle_assigns(socket, user_id, id)}
  end

  def handle_event("unfollow_user", %{"userid" => id}, socket) do
    id = to_charlist(id)
    user_id = socket.assigns.current_user.id
    UserClient.unfollow(user_id, id)
    {:noreply, handle_assigns(socket, user_id, id)}
  end

  def handle_event("open_modal", %{"action" => "follower"}, socket) do
    followers =
      socket.assigns.user.follower
      |> Enum.map(fn user ->
        {:ok, user} = Account.Users.one_by_id(user)
        user
      end)
      |> IO.inspect(label: "followerss --<>")

    {:noreply,
     socket
     |> assign(
       follower_action: true,
       followers: followers,
       edit_action: false,
       follows_action: false
     )}
  end

  def handle_event("open_modal", %{"action" => "edit"}, socket) do
    {:noreply, socket |> assign(edit_action: true, follower_action: false, follows_action: false)}
  end

  def handle_event("open_modal", %{"action" => "follows"}, socket) do
    {:noreply, socket |> assign(follows_action: true, edit_action: false, follower_action: false)}
  end

  def handle_event("open_modal", %{"action" => "report-user"}, socket) do
    {:noreply,
     socket
     |> assign(
       report_user_action: true,
       follows_action: false,
       edit_action: false,
       follower_action: false
     )}
  end

  def handle_event("open_modal", %{"action" => "verify-user"}, socket) do
    {:noreply,
     socket
     |> assign(
       report_user_action: false,
       follows_action: false,
       edit_action: false,
       follower_action: false,
       verified_action: true
     )}
  end

  def handle_event("open_modal", %{"action" => "unverify-user"}, socket) do
    {:noreply,
     socket
     |> assign(
       report_user_action: false,
       follows_action: false,
       edit_action: false,
       follower_action: false,
       verified_action: true
     )}
  end

  # def handle_event("block_user", %{"id" => id}, socket) do
  # id = socket.assigns.current_user.id
  # UserClient.block(id, blocked)
  # {:noreply, socket}
  # end

  # def handle_event("unblock_user", %{"id" => id}, socket) do
  # id = socket.assigns.current_user.id
  # UserClient.unblock(id, umnlocked)
  # {:noreply, socket}
  # end

  def handle_event("get-follower", %{"id" => id}, socket) do
    id = id |> to_charlist
    UserClient.get_follower(id)
    {:noreply, socket}
  end

  def handle_event("get-following", %{"id" => id}, socket) do
    id = id |> to_charlist
    UserClient.get_following(id)
    {:noreply, socket}
  end

  def handle_event("delete_user", %{"username" => username}, socket) do
    UserClient.delete_user(username)
    session_id = socket.assigns.session_uuid
    :ets.delete(:mazaryn_auth_table, :"#{session_id}")

    {:noreply,
     socket
     |> put_flash(:info, "successfully deleted")
     |> push_redirect(to: Routes.page_path(socket, :index))}
  end

  def handle_event("privacy", %{"user" => %{"privacy" => privacy}}, socket) do
    if privacy == "private" do
      UserClient.make_private(socket.assigns.current_user.id)
    else
      UserClient.make_public(socket.assigns.current_user.id)
    end

    {:ok, user} = get_user_by_username(socket.assigns.current_user.username)

    user_changeset = User.changeset(user)

    {:noreply, socket |> assign(form: to_form(user_changeset)) |> assign(privacy: privacy)}
  end

  def handle_event(
        "verify_user",
        %{"username" => username, "admin_username" => admin_username},
        socket
      ) do
    ManageUser.verify_user(username, admin_username)

    {:noreply,
     socket
     |> put_flash(:info, "Update successful")
     |> push_redirect(to: Routes.live_path(socket, __MODULE__, socket.assings.locale, username))}
  end

  def handle_event(
        "unverify_user",
        %{"username" => username, "admin_username" => admin_username},
        socket
      ) do
    ManageUser.unverify_user(username, admin_username)

    {:noreply,
     socket
     |> put_flash(:info, "Update successful")
     |> push_redirect(to: Routes.live_path(socket, __MODULE__, socket.assings.locale, username))}
  end

  defp handle_assigns(socket, user_id, id) do
    socket
    |> assign(:follow_event, follow_event(user_id, id))
    |> assign(:follow_text, follow_text(user_id, id))
    |> assign(:followers, followers(user_id))
    |> assign(:followings, followings(user_id))
  end

  defp get_user_by_username(username), do: Users.one_by_username(username)

  defp one_of_following?(id, username) do
    id
    |> UserClient.get_following()
    |> Enum.any?(&(&1 == username))
  end

  defp follow_text(id, username) do
    if one_of_following?(id, username),
      do: "Unfollow",
      else: "Follow"
  end

  defp follow_event(id, username) do
    if one_of_following?(id, username),
      do: "unfollow_user",
      else: "follow_user"
  end

  defp followers(user_id) do
    user_id
    |> UserClient.get_follower()
    |> Enum.count()
  end

  defp followings(user_id) do
    user_id
    |> UserClient.get_following()
    |> Enum.count()
  end

  defp search_user_by_username(username) do
    case username |> Core.UserClient.search_user() do
      :username_not_exist ->
        nil

      erl_user ->
        [
          erl_user
          |> User.erl_changeset()
          |> User.build()
          |> elem(1)
        ]
    end
  end
end
