defmodule MazarynWeb.HomeLive.CreatePostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Component.SelectLive
  alias Mazaryn.Schema.Post
  alias Mazaryn.Posts
  alias Account.Users

  @impl true
  def mount(socket) do
    {:ok, handle_assign(socket)}
  end

  @impl true
  def handle_event("cancel-entry", %{"ref" => ref} = _params, socket) do
    {:noreply, cancel_upload(socket, :media, ref)}
  end

  def handle_event("validate-post", params, socket) do
    {:noreply, handle_validate_post(socket, params)}
  end

  def handle_event("save-post", params, socket) do
    {:noreply, handle_save_post(socket, params)}
  end

  defp handle_assign(socket) do
    socket
    |> assign(:uploaded_files, [])
    |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)
  end

  defp handle_save_post(socket, %{"post" => post_params} = _params) do
    {:ok, user} =
      socket.assigns.user.id
      |> Users.one_by_id()

    urls = consume_upload(socket)

    post_params =
      post_params
      |> Map.put("author", user.username)
      |> Map.put("media", urls)

    %Post{}
    |> Post.changeset(post_params)
    |> Posts.create_post()
    |> case do
      {:ok, %Post{}} ->
        # send event to parent live-view
        send(self(), :reload_posts)
        socket

      _other ->
        socket
    end
  end

  defp handle_validate_post(socket, %{"post" => post_params} = _params) do
    post_params = Map.put(post_params, "author", "thewestdevop")

    changeset =
      %Post{}
      |> Post.changeset(post_params)
      |> Map.put(:action, :validate)

    assign(socket, :changeset, changeset)
  end

  defp ext(entry) do
    [ext | _] = MIME.extensions(entry.client_type)
    ext
  end

  defp consume_upload(socket) do
    consume_uploaded_entries(socket, :media, fn %{path: path}, entry ->
      dir = Mazaryn.config([:media, :uploads_dir])

      dest = Path.join(dir, "#{entry.uuid}.#{ext(entry)}")
      File.mkdir_p!(Path.dirname(dest))
      File.cp!(path, dest)
      {:ok, Routes.static_path(socket, "/uploads/#{Path.basename(dest)}")}
    end)
  end
end
