defmodule MazarynWeb.HomeLive.Home do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper
  alias Mazaryn.Schema.Post
  alias Mazaryn.Posts
  alias Account.Users
  require Logger

  # case reload home page
  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info(user_id: user_id)
    {:ok, do_mount(user_id, socket)}
  end

  # case redirect form login, signup
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    {:ok, do_mount(get_user_id(session_uuid), socket)}
  end

  @impl true
  def handle_event("messages", _param, socket) do
    random_id = "/messages/" <> "1"
    {:noreply, push_redirect(socket, to: random_id)}
  end

  def handle_event("do_search", %{"search" => search}, socket) do
    socket = assign(socket, search: search)

    {:noreply, socket |> push_redirect(to: Routes.live_path(socket, MazarynWeb.SearchLive.Index))}
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
    |> assign(user: user)
    |> assign(posts: posts)
  end
end
