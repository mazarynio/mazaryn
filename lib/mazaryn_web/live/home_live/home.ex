defmodule MazarynWeb.HomeLive.Home do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper
  alias Mazaryn.Schema.Post
  alias Mazaryn.Posts
  alias Account.Users
  alias Account.User
  alias Core.PostClient
  alias Phoenix.LiveView.JS

  require Logger

  # case reload home page
  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    IO.inspect(socket.assigns, label: "home assign")
    Logger.info(user_id: user_id)
    socket |> assign(results: [])
    {:ok, do_mount(user_id, socket)}
  end

  # case redirect form login, signup
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    socket |> assign(results: [])
    {:ok, do_mount(get_user_id(session_uuid), socket)}
  end

  def handle_params(_params, url, socket) do
    socket = assign(socket, current_path: URI.parse(url).path)
    IO.inspect(label: URI.parse(url).path)
    {:noreply, socket}
  end

  @impl true
  def handle_event("show-comments", %{"id" => post_id}, socket) do
    JS.toggle(to: "#test-toggle", in: "fade-in-scale", out: "fade-out-scale")

    comments =
      post_id
      |> to_charlist()
      |> Mazaryn.Posts.get_comment_by_post_id()

    IO.inspect(comments, label: "this comments are workin")

    {:noreply, socket |> assign(:comments, comments)}
  end

  @impl true
  def handle_event("messages", _param, socket) do
    random_id = "/messages/" <> "1"
    {:noreply, push_redirect(socket, to: random_id)}
  end

  def handle_event("do_search", %{"search" => search}, socket) do
    user = search_user_by_username(search)
    {:noreply, assign(socket, search: search, results: user || [])}
  end

  def handle_event("select_emoji", %{"name" => name}, socket) do
    name |> IO.inspect(label: "ni emoji gani imechaguliwa")
    # {:noreply, push_redirect(socket, to: Routes.emoji_path(socket, :show, name))}
    # post_id = post_id |> to_charlist
    # PostClient.delete_post(post_id)
    send(self(), :reload_posts)

    {:noreply, socket}
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

  @impl true
  def handle_info(:reload_posts, socket) do
    {:noreply, assign(socket, posts: get_posts())}
  end

  @impl true
  def handle_params(_params, _uri, socket) do
    {:noreply, socket}
  end

  defp get_posts, do: Posts.get_home_posts()

  defp do_mount(email, socket) do
    post_changeset = Post.changeset(%Post{})

    {:ok, user} = Users.one_by_email(email)

    posts = get_posts()

    socket
    |> assign(post_changeset: post_changeset)
    |> assign(search: "")
    |> assign(results: [])
    |> assign(user: user)
    |> assign(posts: posts)
  end
end
