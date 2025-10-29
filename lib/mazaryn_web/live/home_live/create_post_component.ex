defmodule MazarynWeb.HomeLive.CreatePostComponent do
  use MazarynWeb, :live_component

  import Phoenix.Component

  alias MazarynWeb.Live.Helper
  alias MazarynWeb.Component.SelectLive
  alias Mazaryn.Schema.Post
  alias Mazaryn.Posts
  alias Account.{Users, User}

  @max_chars 280

  @impl true
  def mount(socket) do
    socket =
      socket
      |> assign(:show_emoji_panel, false)
      |> assign(:changeset, Post.changeset(%Post{}))

    {:ok, handle_assign(socket)}
  end

  @impl true
  def handle_event("cancel-entry", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :media, ref)}
  end

  def handle_event("validate-post", params, socket) do
    {:noreply, handle_validate_post(socket, params)}
  end

  def handle_event("save-post", params, socket) do
    {:noreply, handle_save_post(socket, params)}
  end

  def handle_event("select_emoji", %{"emoji" => emoji_character}, socket) do
    {:noreply,
     socket
     |> assign(:show_emoji_panel, false)
     |> push_event("insert_emoji", %{
       emoji: emoji_character,
       component_id: to_string(socket.assigns.myself)
     })}
  end

  def handle_event("toggle_emoji_panel", _params, socket) do
    {:noreply, assign(socket, :show_emoji_panel, !socket.assigns.show_emoji_panel)}
  end

  def handle_event("close_emoji_panel", _params, socket) do
    {:noreply, assign(socket, :show_emoji_panel, false)}
  end

  defp handle_assign(socket) do
    socket
    |> assign(:uploaded_files, [])
    |> allow_upload(:media,
      accept: ~w(.png .jpg .jpeg .mp4 .gif),
      max_entries: 1,
      max_file_size: 20_000_000,
      chunk_size: 64_000 * 3
    )
  end

  defp handle_save_post(socket, %{"post" => post_params}) do
    content = Map.get(post_params, "content", "")

    if String.length(content) > @max_chars do
      socket
    else
      {:ok, user} = Users.one_by_id(socket.assigns.user.id)

      urls = consume_upload(socket)

      post_params =
        post_params
        |> Map.put("author", user.username)
        |> Map.put("media", urls)

      hashtags = fetch_from_content(~r/#\S[a-zA-Z]*/, post_params)
      mentions = fetch_from_content(~r/@\S[a-zA-Z]*/, post_params)

      link_urls =
        fetch_link_urls_from_content(
          ~r/([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#\.]?[\w-]+)*\/?/,
          post_params
        )

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
        {:ok, %Post{} = new_post} ->
          add_mention_to_notif(mentions, socket.assigns.user.id)

          Task.start(fn ->
            Process.sleep(5000)
            MazarynWeb.HomeLive.IpnsManager.ensure_ipns_background(new_post.id)
          end)

          send(self(), :reload_posts)

          socket
          |> assign(:changeset, Post.changeset(%Post{}))
          |> assign(:show_emoji_panel, false)
          |> push_event("clear_textarea", %{component_id: to_string(socket.assigns.myself)})

        _other ->
          socket
      end
    end
  end

  defp add_mention_to_notif("", _user_id), do: :ok

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

  defp handle_validate_post(socket, %{"post" => post_params}) do
    post_params = Map.put(post_params, "author", socket.assigns.user.username)

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
      {:ok, dest}
    end)
  end
end
