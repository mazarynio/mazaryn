defmodule MazarynWeb.HashtagLive.Index do
  use MazarynWeb, :live_view
  alias Mazaryn.Posts
  alias Mazaryn.Schema.Post
  alias Account.Users
  require Logger

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    {:ok, user} = Users.get_by_session_uuid(session_uuid)

    socket =
      socket
      |> assign(current_user: user)
      |> assign(search: "")
      |> assign(posts: [])
      |> assign(comments: [])
      |> assign(hashtag_name: nil)
      |> assign(post_changeset: Post.changeset(%Post{}))
      |> assign(loading: false)

    {:ok, socket}
  end

  def handle_params(%{"hashtag_name" => hashtag}, _uri, socket) do
    Logger.info("Loading hashtag page for: ##{hashtag}")

    posts = Posts.get_posts_by_hashtag(hashtag)

    Logger.info("Found #{length(posts)} posts for ##{hashtag}")

    socket =
      socket
      |> assign(posts: posts)
      |> assign(hashtag_name: hashtag)

    {:noreply, socket}
  end

  def handle_params(_params, _uri, socket) do
    {:noreply, socket}
  end

  def handle_event("do_search", %{"search" => search}, socket) do
    socket = assign(socket, search: search)
    {:noreply, push_navigate(socket, to: ~p"/#{socket.assigns.locale}/search")}
  end

  def handle_event("show-comments", %{"id" => post_id}, socket) do
    comments = get_comments_for_post(post_id)
    {:noreply, assign(socket, comments: comments)}
  end

  defp get_comments_for_post(post_id) do
    try do
      post_id |> to_charlist() |> Mazaryn.Posts.get_comment_by_post_id()
    rescue
      e ->
        Logger.error("Error loading comments: #{inspect(e)}")
        []
    end
  end
end
