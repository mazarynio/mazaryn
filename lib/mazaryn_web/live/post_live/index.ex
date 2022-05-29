defmodule MazarynWeb.PostLive.Index do
  use MazarynWeb, :live_view

  alias Core.PostClient, as: PostClient

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :posts, list_posts())}
  end

  @impl true
  def handle_params(params, _url, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  defp apply_action(socket, :new, _params) do
    socket
    |> aasign(:page_title, "New Post")
    |> assign(:post, PostClient.create_post(author, content))
  end

  defp apply_action(socket, :edit, %{"id" => id}) do
    socket
    |> assign(:page_title, "Edit Post")
    |> assign(:post, PostClient.get_post_by_id(id))
  end

  defp apply_action(socket, :index, _params) do
    socket
    |> assign(:page_title, "Listing Posts")
    |> assign(:post, nil)
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
     PostClient.get_post_by_id(id)
    {:ok, _} = PostClient.delete_post(id)

    {:noreply, assign(socket, :posts, list_posts())}
  end

  defp list_posts do
    PostClient.get_posts()
  end
end
