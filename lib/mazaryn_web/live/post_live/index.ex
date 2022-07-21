defmodule MazarynWeb.PostLive.Index do
  use MazarynWeb, :live_view

  alias Mazaryn.DataAbstractor
  alias Core.PostClient, as: PostClient
  alias Post

  def mount(_params, session, socket) do
    # start post genserver
    start_post_server()
    # Get the posts from the database.
    {:ok, assign(socket, :posts, get_post())}
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
    PostClient.delete_post(params["id"])

    {:noreply, assign(socket, posts: posts, info: "Post deleted!")}
  end

  defp start_post_server, do: PostClient.start()

  defp get_post, do: PostClient.get_posts()

  defp get_post_by_author(author), do: PostClient.get_posts_by_author(author)


  defp get_session_posts(socket) do
    socket.assigns.posts
  end

  defp add_post(socket, post) do
    case PostClient.create_post(
           post.author,
           post.content
         ) do
      {:ok, _post} ->
        previous_posts = get_session_posts(socket)
        {:noreply, assign(socket, posts: [post | previous_posts], info: "Post added!")}

      {:error, message} ->
        {:noreply, assign(socket, posts: get_session_posts(socket), error: message)}
    end
  end




end
