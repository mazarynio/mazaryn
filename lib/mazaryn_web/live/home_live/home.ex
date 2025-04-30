defmodule MazarynWeb.HomeLive.Home do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Mazaryn.Schema.Post
  alias Mazaryn.Posts
  alias Account.Users
  alias Account.User
  require Logger

  # Case reload home page
  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info("user_id: #{user_id}")
    socket = assign(socket, results: [])
    {:ok, do_mount(user_id, socket)}
  end

  # Case redirect from login, signup
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    socket = assign(socket, results: [])
    {:ok, do_mount(get_user_id(session_uuid), socket)}
  end

  @impl true
  def handle_params(_params, url, socket) do
    socket = assign(socket, current_path: URI.parse(url).path)
    {:noreply, socket}
  end

  @impl true
  def handle_event("show-comments", %{"id" => post_id}, socket) do
    import Phoenix.LiveView.JS
    JS.toggle(to: "#test-toggle", in: "fade-in-scale", out: "fade-out-scale")

    comments = post_id |> to_charlist() |> Mazaryn.Posts.get_comment_by_post_id()
    Logger.info("Comments for post #{post_id}: #{inspect(comments)}")

    {:noreply, socket |> assign(:comments, comments)}
  end

  @impl true
  def handle_event("messages", _param, socket) do
    random_id = "/messages/" <> "1"
    {:noreply, push_redirect(socket, to: random_id)}
  end

  def handle_event("do_search", %{"search" => search}, socket) do
    Logger.info("Searching for: #{search}")
    user = search_user_by_username(search)
    {:noreply, assign(socket, search: search, results: user || [])}
  end

  def handle_event("select_emoji", %{"name" => name}, socket) do
    Logger.info("Selected emoji: #{name}")
    {:noreply, socket}
  end

  def search_user_by_username(username) do
    case username |> Core.UserClient.search_user() do
      :username_not_exist ->
        nil

      erl_user ->
        [
          erl_user |> User.erl_changeset() |> User.build() |> elem(1)
        ]
    end
  end

  @impl true
  def handle_info(:reload_posts, socket) do
    user = socket.assigns.user
    posts = get_user_and_following_posts(user.id)
    Logger.info("Loaded #{length(posts)} posts for home feed")
    {:noreply, assign(socket, posts: posts)}
  end

  defp get_user_and_following_posts(user_id) do
    following_user_ids = Users.get_following(user_id)
    Logger.info("Following user IDs: #{inspect(following_user_ids)}")

    user_posts = Posts.get_posts_by_user_id(user_id)
    Logger.info("Current user posts: #{length(user_posts)}")

    following_posts = following_user_ids
    |> Enum.flat_map(fn following_user_id ->
      Posts.get_posts_by_user_id(following_user_id)
    end)

    (user_posts ++ following_posts)
    |> Enum.sort_by(& &1.date_created, &>=/2)
  end

  defp do_mount(email, socket) do
    post_changeset = Post.changeset(%Post{})

    {:ok, user} = Users.one_by_email(email)

    posts = get_user_and_following_posts(user.id)

    socket
    |> assign(post_changeset: post_changeset)
    |> assign(search: "")
    |> assign(results: [])
    |> assign(user: user)
    |> assign(posts: posts)
  end
end
