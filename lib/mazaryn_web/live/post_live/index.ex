defmodule MazarynWeb.PostLive.Index do
  use MazarynWeb, :live_view

  alias Mazaryn.DataAbstractor
  alias Core.PostClient, as: PostClient
  alias Mazaryn.Schema.Post

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    # start post genserver
    start_post_server()
    # Get the posts from the database.
    {:ok, assign(socket, :posts, get_post(user_id))}
  end

  @impl true
  def handle_event("add_post", %Post{} = post, socket) do
    # Add the post to the database.
    add_post(socket, post)
  end

  @impl true
  def handle_event("edit_post", %{"id" => id}, socket) do
    post =
      get_session_posts(socket)
      |> DataAbstractor.get_item_by_id(id)

    {:noreply, assign(socket, :edit, post)}
  end

  @impl true
  def handle_event("update_post", params, socket) do
    # Update the post in the socket session.
    updated_post =
      get_session_posts(socket)
      |> DataAbstractor.update_item(%{
        id: params["id"],
        content: params["content"]
      })

    post = Map.merge(%Post{}, socket.assigns.edit)

    # Update the post in the database.
    # PostClient.update_post(post, %{ title: params["title"], description:  params["description"]})

    {:noreply, assign(socket, posts: updated_post, info: "Post updated!")}
  end

  @impl true
  def handle_event("delete_post", %{"id" => id}, socket) do
    post =
      get_session_posts(socket)
      |> DataAbstractor.get_item_by_id(id)

    {:noreply, assign(socket, :delete, post)}
  end

  @impl true
  def handle_event("confirmed_delete_post", params, socket) do
    posts =
      get_session_posts(socket)
      |> DataAbstractor.filter_item_by_id(params["id"])

    # Delete the post from the database.
    Post.delete_post(params)

    {:noreply, assign(socket, posts: posts, info: "Post deleted!")}
  end

  defp start_post_server, do: PostClient.start()

  defp get_post(user_id), do: Post.posts_from_user_following(user_id)

  defp get_post_by_author(author), do: Post.posts_from_user(author)

  defp get_session_posts(socket) do
    socket.assigns.posts
  end

  defp add_post(socket, post) do
    case Post.create_post(post) do
      {:ok, _post} ->
        previous_posts = get_session_posts(socket)
        {:noreply, assign(socket, posts: [post | previous_posts], info: "Post added!")}

      {:error, message} ->
        {:noreply, assign(socket, posts: get_session_posts(socket), error: message)}
    end
  end
end
