defmodule MazarynWeb.HomeLive.CreatePostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Live.Helper
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
    |> allow_upload(:media,
      accept: ~w(.png .jpg .jpeg .mp4 .gif),
      max_entries: 2,
      max_file_size: 20_000_000,
      chunk_size: 64_000 * 3
    )
  end

  defp handle_save_post(socket, %{"post" => post_params} = _params) do
    IO.inspect(post_params, label: "pppppppppp")
    markdown = "[\#hashtag](https://twitter.com/hashtag/ChezaKamaWewe?src=hashtag_click)"

    content
    |> String.split(content)
    |> Enum.map(fn content ->
      regex = fetch_regex(content)
      String.replace("#hashtag", markdown)
    end)
    |> Enum.join(" ")
    |> Earmark.as_html!(markdown, compact_output: true)
    |> IO.inspect(label: "]]]]]]]")

    {:ok, user} =
      socket.assigns.user.id
      |> Users.one_by_id()

    urls = consume_upload(socket)

    post_params =
      post_params
      |> Map.put("author", user.username)
      |> Map.put("media", urls)

    hashtags = fetch_from_content(~r/#\S[a-zA-Z]*/, post_params)
    mentions = fetch_from_content(~r/@\S[a-zA-Z]*/, post_params)

    post_params =
      case {hashtags, mentions} do
        {"", ""} ->
          post_params

        {hashtags, ""} ->
          Map.put(post_params, "hashtag", hashtags)

        {"", mentions} ->
          Map.put(post_params, "mention", mentions)

        {hashtags, mentions} ->
          post_params
          |> Map.put("hashtag", hashtags)
          |> Map.put("mention", mentions)
      end

    %Post{}
    |> Post.changeset(post_params)
    |> Posts.create_post()
    |> IO.inspect(label: "[[[[[[[[[[[[[[[[[[[[[[[[CreatePostComponent")
    |> case do
      {:ok, %Post{}} ->
        # send event to parent live-view
        send(self(), :reload_posts)
        socket

      other ->
        socket
    end
  end

  defp fetch_from_content(regex, %{"content" => content}) do
    regex
    |> Regex.scan(content)
    |> List.flatten()
    |> Enum.join(", ")
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
