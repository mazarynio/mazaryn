defmodule MazarynWeb.HomeLive.CreatePostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Live.Helper
  #alias MazarynWeb.Component.CustomComponents
  alias MazarynWeb.Component.SelectLive
  alias Mazaryn.Schema.Post
  alias Mazaryn.Posts
  alias Account.{Users, User}

  @impl true
  def mount(socket) do
    emojis = [
      %{:grinning_face => "\u{1F600}"},
      %{:beaming_face => "\u{1F601}"},
      %{:joy => "\u{1F602}"},
      %{:cowboy_hat_face => "\u{1F920}"},
      %{:winking_face => "\u{1F609}"},
      %{:relieved_face => "\u{1F60C}"}
    ]

    socket =
      socket
      |> assign(:emojis, emojis)

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

  def handle_event("select_emoji", params, socket) do
    params |> IO.inspect(label: "ni emoji gani imechaguliwa xxxx")
    # post_id |> IO.inspect(label: "ni post_id gani imechaguliwa")
    # {:noreply, push_redirect(socket, to: Routes.emoji_path(socket, :show, name))}
    # post_id = post_id |> to_charlist
    # PostClient.delete_post(post_id)
    # send(self(), :reload_posts)
    {:noreply, handle_validate_post(socket, params)}

    # {:noreply, socket}
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
    {:ok, user} =
      socket.assigns.user.id
      |> Users.one_by_id()

    urls = consume_upload(socket)

    post_params =
      post_params
      |> Map.put("author", user.username)
      |> Map.put("media", urls)

    hashtags = fetch_from_content(~r/#\S[a-zA-Z]*/, post_params)

    mentions =
      ~r/@\S[a-zA-Z]*/
      |> fetch_from_content(post_params)

    link_urls =
      ~r/([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#\.]?[\w-]+)*\/?/
      |> fetch_link_urls_from_content(post_params)

    post_params =
      case {hashtags, mentions, link_urls} do
        {"", "", []} ->
          post_params

        {hashtags, "", []} ->
          Map.put(post_params, "hashtag", hashtags)

        {"", mentions, []} ->
          Map.put(post_params, "mention", mentions)

        {"", "", link_urls} ->
          Map.put(post_params, "link_url", link_urls)

        {hashtags, mentions, link_urls} ->
          post_params
          |> Map.put("hashtag", hashtags)
          |> Map.put("mention", mentions)
          |> Map.put("link_url", link_urls)
      end

    %Post{}
    |> Post.changeset(post_params)
    |> Posts.create_post()
    |> case do
      {:ok, %Post{}} ->
        add_mention_to_notif(mentions, socket.assigns.user.id)
        # send event to parent live-view
        send(self(), :reload_posts)

        socket
        |> assign(:changeset, Post.changeset(%Post{}))

      _other ->
        socket
    end
  end

  defp add_mention_to_notif("", _user_id) do
    :ok
  end

  defp add_mention_to_notif(mention, user_id) do
    mention = String.replace(mention, "@", "")

    with {:ok, %User{id: id}} <- Users.one_by_username(mention) do
      Core.NotifEvent.mention(user_id, id)
    else
      {:error, _reason} -> :ok
      _other -> :ok
    end
  end


  defp fetch_from_content(regex, %{"content" => content}) do
    regex
    |> Regex.scan(content)
    |> List.flatten()
    |> Enum.join(", ")
  end

  defp fetch_link_urls_from_content(regex, %{"content" => content}) do
    regex
    |> Regex.scan(content)
    |> Enum.map(fn links -> List.first(links) end)
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
