defmodule MazarynWeb.HashtagLive.Index do
  use MazarynWeb, :live_view

  alias Mazaryn.Posts
  alias Account.Users

  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    {:ok, user} = Users.get_by_session_uuid(session_uuid)
    {:ok, assign(socket, current_user: user, search: "")}
  end

  def handle_params(%{"hashtag_name" => hashtag}, _uri, socket) do
    posts = Posts.get_posts_by_hashtag(hashtag)
    {:noreply, assign(socket, posts: posts)}
  end

  def handle_params(_params, _uri, socket) do
    {:noreply, socket}
  end

  def handle_event("do_search", %{"search" => search}, socket) do
    socket = assign(socket, search: search)

    {:noreply,
     socket
     |> push_redirect(
       to: Routes.live_path(socket, MazarynWeb.SearchLive.Index, socket.assigns.locale)
     )}
  end
end
