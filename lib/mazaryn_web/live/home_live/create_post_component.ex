defmodule MazarynWeb.HomeLive.CreatePostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Live.Helper
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
      %{:heart_eyes => "\u{1F60D}"},
      %{:winking_face => "\u{1F609}"},
      %{:relieved_face => "\u{1F60C}"},
      %{:thinking_face => "\u{1F914}"},
      %{:smiling_face_with_sunglasses => "\u{1F60E}"},
      %{:fire => "\u{1F525}"},
      %{:red_heart => "\u{2764}\u{FE0F}"},
      %{:thumbs_up => "\u{1F44D}"},
      %{:clapping_hands => "\u{1F44F}"},
      %{:party_popper => "\u{1F389}"},
      %{:star_struck => "\u{1F929}"},
      %{:crying_with_laughter => "\u{1F923}"},
      %{:smiling_face_with_hearts => "\u{1F970}"},
      %{:hundred_points => "\u{1F4AF}"},
      %{:rocket => "\u{1F680}"},
      %{:rainbow => "\u{1F308}"},
      %{:smiling_face => "\u{1F60A}"},
      %{:raising_hands => "\u{1F64C}"},
      %{:partying_face => "\u{1F973}"},
      %{:crying_face => "\u{1F622}"},
      %{:flushed_face => "\u{1F633}"},
      %{:folded_hands => "\u{1F64F}"},
      %{:glowing_star => "\u{1F31F}"},
      %{:kissing_face => "\u{1F618}"},
      %{:winking_face_with_tongue => "\u{1F61C}"},
      %{:hugging_face => "\u{1F917}"},
      %{:sleeping_face => "\u{1F634}"},
      %{:sweating_smiling_face => "\u{1F605}"},
      %{:smiling_face_with_halo => "\u{1F607}"},
      %{:pouting_face => "\u{1F623}"},
      %{:nerd_face => "\u{1F913}"},
      %{:yum_face => "\u{1F60B}"},
      %{:tongue_out_face => "\u{1F61B}"},
      %{:smiling_face_with_three_hearts => "\u{1F970}"},
      %{:grimacing_face => "\u{1F62C}"},
      %{:squinting_face_with_tongue => "\u{1F61D}"},
      %{:face_with_steam => "\u{1F624}"},
      %{:expressionless_face => "\u{1F611}"},
      %{:grinning_squinting_face => "\u{1F606}"},
      %{:open_mouth_face => "\u{1F62E}"},
      %{:smiling_cat_with_heart_eyes => "\u{1F63B}"},
      %{:smiling_face_with_open_mouth => "\u{1F603}"},
      %{:grinning_face_with_big_eyes => "\u{1F604}"},
      %{:sad_face => "\u{1F61E}"},
      %{:angry_face => "\u{1F621}"},
      %{:confused_face => "\u{1F615}"},
      %{:surprised_face => "\u{1F62F}"},
      %{:smiling_devil => "\u{1F608}"},
      %{:crying_sad_face => "\u{1F625}"},
      %{:face_with_medical_mask => "\u{1F637}"},
      %{:pained_face => "\u{1F616}"},
      %{:worried_face => "\u{1F61F}"},
      %{:anxious_face => "\u{1F630}"},
      %{:raised_hand => "\u{270B}"},
      %{:ok_hand => "\u{1F44C}"},
      %{:point_left => "\u{1F448}"},
      %{:point_right => "\u{1F449}"},
      %{:point_up => "\u{1F446}"},
      %{:point_down => "\u{1F447}"},
      %{:sign_of_the_horns => "\u{1F918}"},
      %{:vulcan_salute => "\u{1F596}"},
      %{:raising_hand => "\u{1F64B}"},
      %{:balloon => "\u{1F388}"},
      %{:gift => "\u{1F381}"},
      %{:birthday_cake => "\u{1F382}"},
      %{:camera => "\u{1F4F8}"},
      %{:laptop => "\u{1F4BB}"},
      %{:smartphone => "\u{1F4F1}"},
      %{:money_with_wings => "\u{1F4B8}"},
      %{:lock => "\u{1F512}"},
      %{:bell => "\u{1F514}"},
      %{:cat => "\u{1F431}"},
      %{:dog => "\u{1F436}"},
      %{:bear => "\u{1F43B}"},
      %{:lion => "\u{1F981}"},
      %{:panda => "\u{1F43C}"},
      %{:frog => "\u{1F438}"},
      %{:monkey => "\u{1F435}"},
      %{:unicorn => "\u{1F984}"},
      %{:sparkling_heart => "\u{1F496}"},
      %{:collision => "\u{1F4A5}"},
      %{:rainbow_symbol => "\u{1F308}"},
      %{:sun => "\u{2600}\u{FE0F}"},
      %{:star => "\u{2B50}"},
      %{:apple => "\u{1F34E}"},
      %{:pizza => "\u{1F355}"},
      %{:hamburger => "\u{1F354}"},
      %{:coffee => "\u{2615}"},
      %{:ice_cream => "\u{1F366}"},
      %{:wine_glass => "\u{1F377}"},
      %{:rocket_ship => "\u{1F680}"},
      %{:airplane => "\u{2708}\u{FE0F}"},
      %{:globe => "\u{1F30D}"},
      %{:trophy => "\u{1F3C6}"}
    ]

    socket =
      socket
      |> assign(:emojis, emojis)
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
     |> push_event("insert_emoji", %{emoji: emoji_character, component_id: to_string(socket.assigns.myself)})}
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
    {:ok, user} = Users.one_by_id(socket.assigns.user.id)

    urls = consume_upload(socket)

    post_params =
      post_params
      |> Map.put("author", user.username)
      |> Map.put("media", urls)

    hashtags = fetch_from_content(~r/#\S[a-zA-Z]*/, post_params)
    mentions = fetch_from_content(~r/@\S[a-zA-Z]*/, post_params)
    link_urls = fetch_link_urls_from_content(~r/([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#\.]?[\w-]+)*\/?/, post_params)

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
        send(self(), :reload_posts)
        socket
        |> assign(:changeset, Post.changeset(%Post{}))
        |> assign(:show_emoji_panel, false)

      _other ->
        socket
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
