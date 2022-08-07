defmodule MazarynWeb.HomeLive.CreatePostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Component.SelectLive
  alias Account.Users
  alias Mazaryn.Schema.Post

  @impl true
  def mount(socket) do
    socket =
      socket
      |> assign(:uploaded_files, [])
      |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)

    {:ok, socket}
  end

  @impl true
  def handle_event("cancel-entry", %{"ref" => ref} = _params, socket) do
    {:noreply, cancel_upload(socket, :media, ref)}
  end

  def handle_event("validate-post", %{"post" => post_params} = _params, socket) do
    post_params = Map.put(post_params, "author", "thewestdevop")

    changeset =
      %Post{}
      |> Post.changeset(post_params)
      |> Map.put(:action, :validate)

    socket =
      socket
      |> assign(:changeset, changeset)

    {:noreply, socket}
  end

  def handle_event("save-post", %{"post" => post_params} = _params, socket) do
    {:ok, user} =
      socket.assigns.user_id
      |> Users.one_by_id()

    {completed, []} = uploaded_entries(socket, :media)

    urls =
      for entry <- completed do
        Routes.static_path(socket, "/uploads/#{entry.uuid}.#{ext(entry)}")
      end

    post_params =
      post_params
      |> Map.put("author", user.username)
      |> Map.put("media", urls)

    changeset = Post.changeset(%Post{}, post_params)

    case Post.create_post(changeset, &consume_photos(socket, &1)) do
      %Post{} ->
        {:noreply, socket}

      _other ->
        {:noreply, socket}
    end
  end

  defp ext(entry) do
    [ext | _] = MIME.extensions(entry.client_type)
    ext
  end

  defp consume_photos(socket, %Post{} = post) do
    consume_uploaded_entries(socket, :media, fn meta, entry ->
      dest = Path.join("priv/static/uploads", "#{entry.uuid}.#{ext(entry)}")
      File.cp!(meta.path, dest)
    end)

    post
  end
end
