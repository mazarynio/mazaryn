defmodule MazarynWeb.HomeLive.CreatePostComponent do
  use MazarynWeb, :live_component

  import Phoenix.Component

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
      %{:rolling_on_the_floor_laughing => "\u{1F923}"},
      %{:tears_of_joy => "\u{1F602}"},
      %{:slightly_smiling_face => "\u{1F642}"},
      %{:upside_down_face => "\u{1F643}"},
      %{:winking_face => "\u{1F609}"},
      %{:smiling_face_with_smiling_eyes => "\u{1F60A}"},
      %{:smiling_face_with_halo => "\u{1F607}"},
      %{:smiling_face_with_3_hearts => "\u{1F970}"},
      %{:heart_eyes => "\u{1F60D}"},
      %{:star_struck => "\u{1F929}"},
      %{:kissing_face => "\u{1F618}"},
      %{:kissing_face_with_closed_eyes => "\u{1F61A}"},
      %{:kissing_face_with_smiling_eyes => "\u{1F619}"},
      %{:kissing_smiling_eyes => "\u{263A}"},
      %{:relieved_face => "\u{1F60C}"},
      %{:pensive_face => "\u{1F614}"},
      %{:sleepy_face => "\u{1F62A}"},
      %{:drooling_face => "\u{1F924}"},
      %{:sleeping_face => "\u{1F634}"},
      %{:face_with_medical_mask => "\u{1F637}"},
      %{:face_with_thermometer => "\u{1F912}"},
      %{:nauseated_face => "\u{1F922}"},
      %{:vomiting_face => "\u{1F92E}"},
      %{:sneezing_face => "\u{1F927}"},
      %{:hot_face => "\u{1F975}"},
      %{:cold_face => "\u{1F976}"},
      %{:woozy_face => "\u{1F974}"},
      %{:dizzy_face => "\u{1F635}"},
      %{:exploding_head => "\u{1F92F}"},
      %{:thinking_face => "\u{1F914}"},
      %{:face_with_raised_eyebrow => "\u{1F928}"},
      %{:neutral_face => "\u{1F610}"},
      %{:expressionless_face => "\u{1F611}"},
      %{:face_without_mouth => "\u{1F636}"},
      %{:smirking_face => "\u{1F60F}"},
      %{:unamused_face => "\u{1F612}"},
      %{:face_with_rolling_eyes => "\u{1F644}"},
      %{:grimacing_face => "\u{1F62C}"},
      %{:lying_face => "\u{1F925}"},
      %{:shushing_face => "\u{1F92B}"},
      %{:face_with_hand_over_mouth => "\u{1F92D}"},
      %{:smiling_face_with_sunglasses => "\u{1F60E}"},
      %{:nerd_face => "\u{1F913}"},
      %{:face_with_monocle => "\u{1F9D0}"},
      %{:confused_face => "\u{1F615}"},
      %{:worried_face => "\u{1F61F}"},
      %{:slightly_frowning_face => "\u{1F641}"},
      %{:frowning_face => "\u{2639}"},
      %{:face_with_open_mouth => "\u{1F62E}"},
      %{:hushed_face => "\u{1F62F}"},
      %{:astonished_face => "\u{1F632}"},
      %{:flushed_face => "\u{1F633}"},
      %{:pleading_face => "\u{1F97A}"},
      %{:frowning_face_with_open_mouth => "\u{1F626}"},
      %{:anguished_face => "\u{1F627}"},
      %{:fearful_face => "\u{1F628}"},
      %{:anxious_face_with_sweat => "\u{1F630}"},
      %{:sad_but_relieved_face => "\u{1F625}"},
      %{:crying_face => "\u{1F622}"},
      %{:loudly_crying_face => "\u{1F62D}"},
      %{:face_screaming_in_fear => "\u{1F631}"},
      %{:confounded_face => "\u{1F616}"},
      %{:persevering_face => "\u{1F623}"},
      %{:disappointed_face => "\u{1F61E}"},
      %{:downcast_face_with_sweat => "\u{1F613}"},
      %{:weary_face => "\u{1F629}"},
      %{:tired_face => "\u{1F62B}"},
      %{:yawning_face => "\u{1F971}"},
      %{:face_with_steam_from_nose => "\u{1F624}"},
      %{:pouting_face => "\u{1F621}"},
      %{:angry_face => "\u{1F620}"},
      %{:face_with_symbols_on_mouth => "\u{1F92C}"},
      %{:smiling_face_with_horns => "\u{1F608}"},
      %{:angry_face_with_horns => "\u{1F47F}"},
      %{:skull => "\u{1F480}"},
      %{:skull_and_crossbones => "\u{2620}"},

      %{:thumbs_up => "\u{1F44D}"},
      %{:thumbs_down => "\u{1F44E}"},
      %{:clapping_hands => "\u{1F44F}"},
      %{:raised_hands => "\u{1F64C}"},
      %{:folded_hands => "\u{1F64F}"},
      %{:handshake => "\u{1F91D}"},
      %{:crossed_fingers => "\u{1F91E}"},
      %{:victory_hand => "\u{270C}"},
      %{:love_you_gesture => "\u{1F91F}"},
      %{:sign_of_the_horns => "\u{1F918}"},
      %{:call_me_hand => "\u{1F919}"},
      %{:backhand_index_pointing_left => "\u{1F448}"},
      %{:backhand_index_pointing_right => "\u{1F449}"},
      %{:backhand_index_pointing_up => "\u{1F446}"},
      %{:backhand_index_pointing_down => "\u{1F447}"},
      %{:index_pointing_up => "\u{261D}"},
      %{:raised_fist => "\u{270A}"},
      %{:oncoming_fist => "\u{1F44A}"},
      %{:left_facing_fist => "\u{1F91B}"},
      %{:right_facing_fist => "\u{1F91C}"},
      %{:raised_hand => "\u{270B}"},
      %{:vulcan_salute => "\u{1F596}"},
      %{:ok_hand => "\u{1F44C}"},
      %{:pinched_fingers => "\u{1F90C}"},
      %{:pinching_hand => "\u{1F90F}"},
      %{:waving_hand => "\u{1F44B}"},
      %{:muscle => "\u{1F4AA}"},

      %{:red_heart => "\u{2764}"},
      %{:orange_heart => "\u{1F9E1}"},
      %{:yellow_heart => "\u{1F49B}"},
      %{:green_heart => "\u{1F49A}"},
      %{:blue_heart => "\u{1F499}"},
      %{:purple_heart => "\u{1F49C}"},
      %{:brown_heart => "\u{1F90E}"},
      %{:black_heart => "\u{1F5A4}"},
      %{:white_heart => "\u{1F90D}"},
      %{:broken_heart => "\u{1F494}"},
      %{:heart_exclamation => "\u{2763}"},
      %{:two_hearts => "\u{1F495}"},
      %{:revolving_hearts => "\u{1F49E}"},
      %{:beating_heart => "\u{1F493}"},
      %{:growing_heart => "\u{1F497}"},
      %{:sparkling_heart => "\u{1F496}"},
      %{:heart_with_arrow => "\u{1F498}"},
      %{:heart_with_ribbon => "\u{1F49D}"},
      %{:heart_decoration => "\u{1F49F}"},
      %{:fire => "\u{1F525}"},
      %{:hundred_points => "\u{1F4AF}"},
      %{:collision => "\u{1F4A5}"},
      %{:party_popper => "\u{1F389}"},
      %{:confetti_ball => "\u{1F38A}"},
      %{:star => "\u{2B50}"},
      %{:glowing_star => "\u{1F31F}"},
      %{:dizzy => "\u{1F4AB}"},
      %{:sparkles => "\u{2728}"},
      %{:rocket => "\u{1F680}"},
      %{:rainbow => "\u{1F308}"},

      %{:balloon => "\u{1F388}"},
      %{:gift => "\u{1F381}"},
      %{:birthday_cake => "\u{1F382}"},
      %{:trophy => "\u{1F3C6}"},
      %{:first_place_medal => "\u{1F947}"},
      %{:second_place_medal => "\u{1F948}"},
      %{:third_place_medal => "\u{1F949}"},
      %{:camera => "\u{1F4F8}"},
      %{:video_camera => "\u{1F4F9}"},
      %{:laptop => "\u{1F4BB}"},
      %{:smartphone => "\u{1F4F1}"},
      %{:money_with_wings => "\u{1F4B8}"},
      %{:gem_stone => "\u{1F48E}"},
      %{:crown => "\u{1F451}"},
      %{:ring => "\u{1F48D}"},
      %{:lock => "\u{1F512}"},
      %{:key => "\u{1F511}"},
      %{:bell => "\u{1F514}"},
      %{:light_bulb => "\u{1F4A1}"},
      %{:books => "\u{1F4DA}"},
      %{:graduation_cap => "\u{1F393}"},
      %{:artist_palette => "\u{1F3A8}"},
      %{:microphone => "\u{1F3A4}"},
      %{:headphone => "\u{1F3A7}"},
      %{:musical_note => "\u{1F3B5}"},
      %{:guitar => "\u{1F3B8}"},

      %{:sun => "\u{2600}"},
      %{:moon => "\u{1F319}"},
      %{:star_and_crescent => "\u{262A}"},
      %{:cloud => "\u{2601}"},
      %{:snowflake => "\u{2744}"},
      %{:lightning => "\u{26A1}"},
      %{:droplet => "\u{1F4A7}"},
      %{:ocean => "\u{1F30A}"},
      %{:earth_globe_europe_africa => "\u{1F30D}"},
      %{:earth_globe_americas => "\u{1F30E}"},
      %{:earth_globe_asia_australia => "\u{1F30F}"},
      %{:cat => "\u{1F431}"},
      %{:dog => "\u{1F436}"},
      %{:bear => "\u{1F43B}"},
      %{:lion => "\u{1F981}"},
      %{:tiger => "\u{1F42F}"},
      %{:panda => "\u{1F43C}"},
      %{:frog => "\u{1F438}"},
      %{:monkey => "\u{1F435}"},
      %{:unicorn => "\u{1F984}"},
      %{:butterfly => "\u{1F98B}"},
      %{:bee => "\u{1F41D}"},
      %{:eagle => "\u{1F985}"},
      %{:dove => "\u{1F54A}"},

      %{:apple => "\u{1F34E}"},
      %{:banana => "\u{1F34C}"},
      %{:grapes => "\u{1F347}"},
      %{:strawberry => "\u{1F353}"},
      %{:watermelon => "\u{1F349}"},
      %{:peach => "\u{1F351}"},
      %{:cherries => "\u{1F352}"},
      %{:pizza => "\u{1F355}"},
      %{:hamburger => "\u{1F354}"},
      %{:french_fries => "\u{1F35F}"},
      %{:hot_dog => "\u{1F32D}"},
      %{:taco => "\u{1F32E}"},
      %{:burrito => "\u{1F32F}"},
      %{:popcorn => "\u{1F37F}"},
      %{:doughnut => "\u{1F369}"},
      %{:cookie => "\u{1F36A}"},
      %{:chocolate_bar => "\u{1F36B}"},
      %{:candy => "\u{1F36C}"},
      %{:ice_cream => "\u{1F366}"},
      %{:soft_ice_cream => "\u{1F366}"},
      %{:birthday_cake => "\u{1F382}"},
      %{:coffee => "\u{2615}"},
      %{:tea => "\u{1F375}"},
      %{:wine_glass => "\u{1F377}"},
      %{:beer_mug => "\u{1F37A}"},
      %{:cocktail_glass => "\u{1F378}"},

      %{:soccer_ball => "\u{26BD}"},
      %{:basketball => "\u{1F3C0}"},
      %{:american_football => "\u{1F3C8}"},
      %{:baseball => "\u{26BE}"},
      %{:tennis => "\u{1F3BE}"},
      %{:volleyball => "\u{1F3D0}"},
      %{:ping_pong => "\u{1F3D3}"},
      %{:badminton => "\u{1F3F8}"},
      %{:boxing_glove => "\u{1F94A}"},
      %{:running_shoe => "\u{1F45F}"},
      %{:bicycle => "\u{1F6B2}"},
      %{:motorcycle => "\u{1F3CD}"},
      %{:car => "\u{1F697}"},
      %{:airplane => "\u{2708}"},
      %{:ship => "\u{1F6A2}"},
      %{:train => "\u{1F686}"},
      %{:rocket_ship => "\u{1F680}"},

      %{:eyes => "\u{1F440}"},
      %{:see_no_evil_monkey => "\u{1F648}"},
      %{:hear_no_evil_monkey => "\u{1F649}"},
      %{:speak_no_evil_monkey => "\u{1F64A}"},
      %{:kiss_mark => "\u{1F48B}"},
      %{:footprints => "\u{1F463}"},
      %{:sweat_droplets => "\u{1F4A6}"},
      %{:zzz => "\u{1F4A4}"},
      %{:speech_balloon => "\u{1F4AC}"},
      %{:thought_balloon => "\u{1F4AD}"},
      %{:anger_symbol => "\u{1F4A2}"},
      %{:check_mark => "\u{2705}"},
      %{:cross_mark => "\u{274C}"},
      %{:question_mark => "\u{2753}"},
      %{:exclamation_mark => "\u{2757}"},
      %{:warning => "\u{26A0}"},
      %{:no_entry => "\u{26D4}"},
      %{:recycling_symbol => "\u{267E}"},
      %{:peace_symbol => "\u{262E}"},
      %{:yin_yang => "\u{262F}"}
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
