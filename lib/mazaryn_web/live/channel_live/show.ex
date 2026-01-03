defmodule MazarynWeb.ChannelLive.Show do
  use MazarynWeb, :live_view
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"id" => channel_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("🔵 CHANNEL SHOW MOUNT - SESSION UUID")
    Logger.info("=" |> String.duplicate(80))
    Logger.info("📍 Channel ID: #{channel_id}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      Logger.info("✅ User loaded successfully")

      case load_channel(channel_id) do
        nil ->
          Logger.error("❌ CHANNEL NOT FOUND")

          {:ok,
           socket
           |> put_flash(:error, "Channel not found")
           |> redirect(to: "/#{socket.assigns.locale}/groups")}

        channel ->
          Logger.info("✅ Channel loaded successfully")
          Logger.info("   Channel Name: #{channel.name}")
          Logger.info("   Channel Category: #{inspect(channel.category)}")
          Logger.info("   Subscriber Count: #{inspect(channel.subscriber_count)}")

          user_id_str = extract_user_id(user)
          is_subscriber = is_subscriber?(channel, user_id_str)
          is_owner = is_owner?(channel, user_id_str)
          is_admin = is_admin?(channel, user_id_str)

          Logger.info("🔐 User Permissions:")
          Logger.info("   Is Subscriber: #{is_subscriber}")
          Logger.info("   Is Owner: #{is_owner}")
          Logger.info("   Is Admin: #{is_admin}")

          posts = load_channel_posts(channel_id)
          Logger.info("📨 Loaded #{length(posts)} posts")

          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(channel: channel)
            |> assign(is_subscriber: is_subscriber)
            |> assign(is_owner: is_owner)
            |> assign(is_admin: is_admin)
            |> assign(posts: posts)
            |> assign(post_content: "")
            |> assign(active_tab: "posts")
            |> assign(show_invite_modal: false)
            |> assign(invite_search: "")
            |> assign(found_users: [])

          {:ok, socket}
      end
    else
      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(%{"id" => channel_id}, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        case load_channel(channel_id) do
          nil ->
            {:ok,
             socket
             |> put_flash(:error, "Channel not found")
             |> redirect(to: "/#{socket.assigns.locale}/groups")}

          channel ->
            user_id_str = extract_user_id(user)
            is_subscriber = is_subscriber?(channel, user_id_str)
            is_owner = is_owner?(channel, user_id_str)
            is_admin = is_admin?(channel, user_id_str)

            posts = load_channel_posts(channel_id)

            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(channel: channel)
              |> assign(is_subscriber: is_subscriber)
              |> assign(is_owner: is_owner)
              |> assign(is_admin: is_admin)
              |> assign(posts: posts)
              |> assign(post_content: "")
              |> assign(active_tab: "posts")
              |> assign(show_invite_modal: false)
              |> assign(invite_search: "")
              |> assign(found_users: [])

            {:ok, socket}
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("subscribe_channel", _params, socket) do
    user_id_str = extract_user_id(socket.assigns.user)
    channel_id = socket.assigns.channel.id

    try do
      :groupdb.subscribe_channel(to_charlist(user_id_str), to_charlist(channel_id))

      channel = load_channel(channel_id)
      is_subscriber = is_subscriber?(channel, user_id_str)

      {:noreply,
       socket
       |> assign(channel: channel)
       |> assign(is_subscriber: is_subscriber)
       |> put_flash(:info, "You have subscribed to the channel!")}
    rescue
      error ->
        Logger.error("❌ FAILED TO SUBSCRIBE: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to subscribe")}
    end
  end

  @impl true
  def handle_event("unsubscribe_channel", _params, socket) do
    user_id_str = extract_user_id(socket.assigns.user)
    channel_id = socket.assigns.channel.id

    try do
      :groupdb.unsubscribe_channel(to_charlist(user_id_str), to_charlist(channel_id))

      {:noreply,
       socket
       |> put_flash(:info, "You have unsubscribed from the channel")
       |> redirect(to: "/#{socket.assigns.locale}/groups")}
    rescue
      error ->
        Logger.error("❌ FAILED TO UNSUBSCRIBE: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to unsubscribe")}
    end
  end

  @impl true
  def handle_event("create_post", %{"content" => content}, socket) do
    content_trimmed = String.trim(content)

    if content_trimmed == "" do
      {:noreply, socket}
    else
      user_id_str = extract_user_id(socket.assigns.user)
      channel_id = socket.assigns.channel.id

      try do
        :groupdb.create_channel_post(
          to_charlist(channel_id),
          to_charlist(user_id_str),
          to_charlist(content_trimmed),
          []
        )

        posts = load_channel_posts(channel_id)

        {:noreply,
         socket
         |> assign(posts: posts)
         |> assign(post_content: "")}
      rescue
        error ->
          Logger.error("❌ FAILED TO CREATE POST: #{inspect(error)}")
          {:noreply, put_flash(socket, :error, "Failed to create post")}
      end
    end
  end

  @impl true
  def handle_event("update_post_content", %{"content" => content}, socket) do
    {:noreply, assign(socket, post_content: content)}
  end

  @impl true
  def handle_event("delete_post", %{"post_id" => post_id}, socket) do
    user_id_str = extract_user_id(socket.assigns.user)

    try do
      :groupdb.delete_channel_post(to_charlist(post_id), to_charlist(user_id_str))
      posts = load_channel_posts(socket.assigns.channel.id)

      {:noreply,
       socket
       |> assign(posts: posts)
       |> put_flash(:info, "Post deleted")}
    rescue
      error ->
        Logger.error("Failed to delete post: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to delete post")}
    end
  end

  @impl true
  def handle_event("react_to_post", %{"post_id" => post_id, "reaction" => reaction}, socket) do
    user_id_str = extract_user_id(socket.assigns.user)

    try do
      :groupdb.react_to_channel_post(
        to_charlist(post_id),
        to_charlist(user_id_str),
        String.to_atom(reaction)
      )

      posts = load_channel_posts(socket.assigns.channel.id)
      {:noreply, assign(socket, posts: posts)}
    rescue
      error ->
        Logger.error("Failed to react to post: #{inspect(error)}")
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("open_invite_modal", _params, socket) do
    Logger.info("📧 Opening invite modal")
    {:noreply, assign(socket, show_invite_modal: true, invite_search: "", found_users: [])}
  end

  @impl true
  def handle_event("close_invite_modal", _params, socket) do
    Logger.info("❌ Closing invite modal")
    {:noreply, assign(socket, show_invite_modal: false, invite_search: "", found_users: [])}
  end

  @impl true
  def handle_event("search_users", %{"query" => query}, socket) do
    Logger.info("🔍 Searching users: #{query}")

    if String.trim(query) == "" do
      {:noreply, assign(socket, invite_search: query, found_users: [])}
    else
      found_users = search_users(query)
      Logger.info("   Found #{length(found_users)} users")
      {:noreply, assign(socket, invite_search: query, found_users: found_users)}
    end
  end

  @impl true
  def handle_event("send_invite", %{"invitee_id" => invitee_id}, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("📨 SEND INVITE EVENT")
    Logger.info("=" |> String.duplicate(80))

    user_id_str = extract_user_id(socket.assigns.user)
    channel_id = socket.assigns.channel.id

    Logger.info("📍 Inviter ID: #{user_id_str}")
    Logger.info("📍 Invitee ID: #{invitee_id}")
    Logger.info("📍 Channel ID: #{channel_id}")

    try do
      Logger.info("🔵 Calling :groupdb.send_channel_invite...")

      invite_id =
        :groupdb.send_channel_invite(
          to_charlist(channel_id),
          to_charlist(user_id_str),
          to_charlist(invitee_id),
          to_charlist("Join my channel!")
        )

      Logger.info("✅ Invite sent, ID: #{inspect(invite_id)}")
      Logger.info("=" |> String.duplicate(80))

      {:noreply,
       socket
       |> assign(show_invite_modal: false)
       |> assign(invite_search: "")
       |> assign(found_users: [])
       |> put_flash(:info, "Invitation sent!")}
    rescue
      error ->
        Logger.error("❌ FAILED TO SEND INVITE")
        Logger.error("   Error: #{inspect(error)}")
        Logger.info("=" |> String.duplicate(80))
        {:noreply, put_flash(socket, :error, "Failed to send invitation")}
    end
  end

  defp extract_user_id(user) do
    if is_binary(user.id), do: user.id, else: to_string(user.id)
  end

  defp load_channel(channel_id) do
    Logger.info("🔍 load_channel called with ID: #{channel_id}")

    try do
      Logger.info("🔵 Calling :groupdb.get_channel...")
      channel = :groupdb.get_channel(to_charlist(channel_id))
      Logger.info("✅ Raw channel record received")
      Logger.info("   Raw tuple size: #{tuple_size(channel)}")
      Logger.info("   Element 1 (id): #{inspect(elem(channel, 1))}")
      Logger.info("   Element 3 (name): #{inspect(elem(channel, 3))}")
      Logger.info("   Element 9 (category): #{inspect(elem(channel, 9))}")
      Logger.info("   Element 11 (subscriber_count): #{inspect(elem(channel, 11))}")

      converted = convert_channel_record(channel)
      Logger.info("✅ Converted channel")
      Logger.info("   Converted name: #{converted.name}")
      Logger.info("   Converted category: #{converted.category}")
      Logger.info("   Converted subscriber_count: #{converted.subscriber_count}")
      converted
    rescue
      error ->
        Logger.error("❌ Failed to load channel")
        Logger.error("   Error: #{inspect(error)}")
        nil
    catch
      :throw, {:transaction_failed, {:throw, :channel_not_found}} ->
        Logger.error("❌ Channel not found in database")
        nil

      :throw, reason ->
        Logger.error("❌ Caught throw: #{inspect(reason)}")
        nil
    end
  end

  defp load_channel_posts(channel_id) do
    try do
      case :groupdb.get_channel_posts(to_charlist(channel_id), 50) do
        posts when is_list(posts) ->
          Enum.map(posts, &convert_post_record/1)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp is_subscriber?(channel, user_id) do
    if is_owner?(channel, user_id) do
      true
    else
      Enum.any?(channel.subscribers, fn subscriber_id ->
        to_string(subscriber_id) == to_string(user_id)
      end)
    end
  end

  defp is_owner?(channel, user_id) do
    to_string(channel.owner_id) == to_string(user_id)
  end

  defp is_admin?(channel, user_id) do
    member_result =
      Enum.any?(channel.admins, fn admin_id ->
        to_string(admin_id) == to_string(user_id)
      end)

    owner_result = is_owner?(channel, user_id)
    member_result || owner_result
  end

  defp convert_channel_record(channel) do
    admins_raw = elem(channel, 7)
    subscribers_raw = elem(channel, 8)
    category_raw = elem(channel, 9)
    subscriber_count_raw = elem(channel, 11)

    Logger.info("🔄 Converting channel record...")
    Logger.info("   Raw category: #{inspect(category_raw)}")
    Logger.info("   Raw subscriber_count: #{inspect(subscriber_count_raw)}")

    admins =
      case admins_raw do
        :undefined -> []
        list when is_list(list) -> Enum.map(list, &to_string/1)
        _ -> []
      end

    subscribers =
      case subscribers_raw do
        :undefined -> []
        list when is_list(list) -> Enum.map(list, &to_string/1)
        _ -> []
      end

    category =
      case category_raw do
        :undefined ->
          ""
        [] ->
          ""
        cat when is_list(cat) ->
          cat_string = to_string(cat)
          if valid_category?(cat_string), do: cat_string, else: ""
        cat when is_binary(cat) ->
          if valid_category?(cat), do: cat, else: ""
        _ ->
          ""
      end

    subscriber_count =
      case subscriber_count_raw do
        :undefined -> 0
        count when is_integer(count) -> count
        _ -> 0
      end

    Logger.info("✅ Converted values:")
    Logger.info("   category: #{inspect(category)}")
    Logger.info("   subscriber_count: #{inspect(subscriber_count)}")

    %{
      id: to_string(elem(channel, 1)),
      unique_name: to_string(elem(channel, 2)),
      name: to_string(elem(channel, 3)),
      description: to_string(elem(channel, 4) || ""),
      privacy: elem(channel, 5),
      owner_id: to_string(elem(channel, 6)),
      admins: admins,
      subscribers: subscribers,
      category: category,
      subscriber_count: subscriber_count,
      date_created: elem(channel, 12)
    }
  end

  defp convert_post_record(post) do
    %{
      id: to_string(elem(post, 1)),
      channel_id: to_string(elem(post, 2)),
      user_id: to_string(elem(post, 3)),
      content: to_string(elem(post, 4)),
      media: elem(post, 5),
      reactions: elem(post, 8),
      reaction_counts: elem(post, 9),
      pinned: elem(post, 10),
      edited: elem(post, 11),
      deleted: elem(post, 12),
      date_created: elem(post, 13),
      date_updated: elem(post, 14)
    }
  end

  defp valid_category?(category) when is_binary(category) do
    if String.trim(category) == "" do
      false
    else
      len = String.length(category)
      if len > 50 do
        false
      else
        has_space = String.contains?(category, " ")
        has_comma = String.contains?(category, ",")
        is_short = len <= 20

        has_space or has_comma or is_short
      end
    end
  end

  defp valid_category?(_), do: false

  defp search_users(query) do
    Logger.info("🔍 search_users called with query: #{query}")

    try do
      case Core.UserClient.search_user_pattern(to_charlist(query)) do
        users when is_list(users) ->
          Logger.info("✅ Found #{length(users)} users")

          converted =
            Enum.map(users, fn user ->
              username = elem(user, 8) |> to_string()
              avatar_url = get_user_avatar(username)

              %{
                id: to_string(elem(user, 1)),
                username: username,
                avatar_url: avatar_url
              }
            end)

          Logger.info("✅ Converted #{length(converted)} user records")
          converted

        other ->
          Logger.warning("⚠️ Unexpected result: #{inspect(other)}")
          []
      end
    rescue
      error ->
        Logger.error("❌ Failed to search users")
        Logger.error("   Error: #{inspect(error)}")
        []
    end
  end

  def get_user_info(user_id) do
    try do
      charlist_id = if is_binary(user_id), do: to_charlist(user_id), else: user_id

      case Core.UserClient.get_user_by_id(charlist_id) do
        {:error, _reason} ->
          %{id: to_string(user_id), username: "Unknown", avatar_url: "/images/default-avatar.png"}

        :user_not_exist ->
          %{id: to_string(user_id), username: "Unknown", avatar_url: "/images/default-avatar.png"}

        user_tuple when is_tuple(user_tuple) ->
          username = elem(user_tuple, 8) |> to_string()
          avatar_url = get_user_avatar(username)

          %{
            id: to_string(user_id),
            username: username,
            avatar_url: avatar_url
          }

        _ ->
          %{id: to_string(user_id), username: "Unknown", avatar_url: "/images/default-avatar.png"}
      end
    rescue
      _ ->
        %{id: to_string(user_id), username: "Unknown", avatar_url: "/images/default-avatar.png"}
    end
  end

  def get_user_avatar(username) when is_binary(username) do
    try do
      case Account.Users.one_by_username(username) do
        {:ok, user} ->
          case user.avatar_url do
            nil -> "/images/default-avatar.png"
            "" -> "/images/default-avatar.png"
            url -> url
          end

        {:error, _} ->
          "/images/default-avatar.png"
      end
    rescue
      _ -> "/images/default-avatar.png"
    end
  end

  def get_user_avatar(_), do: "/images/default-avatar.png"

  def format_datetime(datetime) do
    case datetime do
      {{year, month, day}, {hour, minute, _second}} ->
        "#{day}/#{month}/#{year} #{String.pad_leading(to_string(hour), 2, "0")}:#{String.pad_leading(to_string(minute), 2, "0")}"

      _ ->
        "Unknown"
    end
  end

  def reaction_emoji(reaction_type) do
    case reaction_type do
      :like -> "👍"
      :love -> "❤️"
      :laugh -> "😂"
      :wow -> "😮"
      _ -> "👍"
    end
  end
end
