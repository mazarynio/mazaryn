defmodule MazarynWeb.GroupLive.Show do
  use MazarynWeb, :live_view
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"id" => group_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("🔵 GROUP SHOW MOUNT - SESSION UUID")
    Logger.info("=" |> String.duplicate(80))
    Logger.info("📍 Group ID: #{group_id}")
    Logger.info("📍 Session UUID: #{session_uuid}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      Logger.info("✅ User loaded successfully")
      Logger.info("   User ID: #{inspect(user.id)}")
      Logger.info("   Username: #{user.username}")

      Logger.info("🔍 Loading group...")

      case load_group(group_id) do
        nil ->
          Logger.error("❌ GROUP NOT FOUND")
          Logger.error("   Group ID: #{group_id}")
          Logger.info("=" |> String.duplicate(80))

          {:ok,
           socket
           |> put_flash(:error, "Group not found")
           |> redirect(to: "/#{socket.assigns.locale}/groups")}

        group ->
          Logger.info("✅ Group loaded successfully")
          Logger.info("   Group Name: #{group.name}")
          Logger.info("   Group ID: #{group.id}")

          user_id_str = extract_user_id(user)
          Logger.info("👤 User ID String: #{user_id_str}")

          is_member = is_member?(group, user_id_str)
          is_owner = is_owner?(group, user_id_str)
          is_admin = is_admin?(group, user_id_str)

          Logger.info("🔐 User Permissions:")
          Logger.info("   Is Member: #{is_member}")
          Logger.info("   Is Owner: #{is_owner}")
          Logger.info("   Is Admin: #{is_admin}")

          messages =
            if is_member do
              Logger.info("📨 Loading messages for member...")
              msgs = load_group_messages(group_id)
              Logger.info("   Loaded #{length(msgs)} messages")
              msgs
            else
              Logger.info("⚠️ User is not a member, skipping message load")
              []
            end

          members = load_group_members(group_id)
          Logger.info("👥 Loaded #{length(members)} members")

          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(group: group)
            |> assign(is_member: is_member)
            |> assign(is_owner: is_owner)
            |> assign(is_admin: is_admin)
            |> assign(messages: messages)
            |> assign(members: members)
            |> assign(message_content: "")
            |> assign(active_tab: "chat")

          Logger.info("✅ Socket assigned successfully")
          Logger.info("=" |> String.duplicate(80))
          {:ok, socket}
      end
    else
      {:error, reason} ->
        Logger.error("❌ FAILED TO GET USER")
        Logger.error("   Reason: #{inspect(reason)}")
        Logger.info("=" |> String.duplicate(80))

        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(%{"id" => group_id}, %{"user_id" => user_id} = _session, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("🔵 GROUP SHOW MOUNT - USER ID")
    Logger.info("=" |> String.duplicate(80))
    Logger.info("📍 Group ID: #{group_id}")
    Logger.info("📍 User ID: #{user_id}")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        Logger.info("✅ User loaded successfully")
        Logger.info("   User ID: #{inspect(user.id)}")
        Logger.info("   Username: #{user.username}")

        Logger.info("🔍 Loading group...")

        case load_group(group_id) do
          nil ->
            {:ok,
             socket
             |> put_flash(:error, "Group not found")
             |> redirect(to: "/#{socket.assigns.locale}/groups")}

          group ->
            Logger.info("✅ Group loaded successfully")
            Logger.info("   Group Name: #{group.name}")

            user_id_str = extract_user_id(user)
            is_member = is_member?(group, user_id_str)
            is_owner = is_owner?(group, user_id_str)
            is_admin = is_admin?(group, user_id_str)

            messages =
              if is_member do
                load_group_messages(group_id)
              else
                []
              end

            members = load_group_members(group_id)

            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(group: group)
              |> assign(is_member: is_member)
              |> assign(is_owner: is_owner)
              |> assign(is_admin: is_admin)
              |> assign(messages: messages)
              |> assign(members: members)
              |> assign(message_content: "")
              |> assign(active_tab: "chat")

            {:ok, socket}
        end

      {:error, reason} ->
        Logger.error("❌ FAILED TO GET USER")
        Logger.error("   Reason: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    Logger.info("🔄 Switching tab to: #{tab}")
    {:noreply, assign(socket, active_tab: tab)}
  end

  @impl true
  def handle_event("join_group", _params, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("🚀 JOIN GROUP EVENT")
    Logger.info("=" |> String.duplicate(80))

    user_id_str = extract_user_id(socket.assigns.user)
    group_id = socket.assigns.group.id

    Logger.info("📍 User ID: #{user_id_str}")
    Logger.info("📍 Group ID: #{group_id}")

    if socket.assigns.is_owner do
      Logger.warning("⚠️ User is the owner, already a member")
      {:noreply, socket}
    else
      try do
        Logger.info("🔵 Calling :groupdb.join_group...")
        result = :groupdb.join_group(to_charlist(user_id_str), to_charlist(group_id))
        Logger.info("✅ Join result: #{inspect(result)}")

        Logger.info("🔄 Reloading group data...")
        group = load_group(group_id)
        messages = load_group_messages(group_id)
        members = load_group_members(group_id)

        user_id_str = extract_user_id(socket.assigns.user)
        is_member = is_member?(group, user_id_str)
        is_owner = is_owner?(group, user_id_str)
        is_admin = is_admin?(group, user_id_str)

        Logger.info("✅ Successfully joined group")
        Logger.info("=" |> String.duplicate(80))

        {:noreply,
         socket
         |> assign(group: group)
         |> assign(is_member: is_member)
         |> assign(is_owner: is_owner)
         |> assign(is_admin: is_admin)
         |> assign(messages: messages)
         |> assign(members: members)
         |> put_flash(:info, "You have joined the group!")}
      rescue
        error ->
          error_msg =
            case error do
              %RuntimeError{message: msg} when is_binary(msg) ->
                cond do
                  String.contains?(msg, "already_member") ->
                    Logger.warning("⚠️ User is already a member, reloading state...")
                    group = load_group(group_id)
                    messages = load_group_messages(group_id)
                    members = load_group_members(group_id)
                    is_member = is_member?(group, user_id_str)
                    is_owner = is_owner?(group, user_id_str)
                    is_admin = is_admin?(group, user_id_str)

                    send(
                      self(),
                      {:update_state,
                       %{
                         group: group,
                         is_member: is_member,
                         is_owner: is_owner,
                         is_admin: is_admin,
                         messages: messages,
                         members: members
                       }}
                    )

                    "You are already a member of this group"

                  String.contains?(msg, "invite_required") ->
                    "This is a private group. You need an invitation to join."

                  String.contains?(msg, "user_banned") ->
                    "You have been banned from this group"

                  true ->
                    "Failed to join group"
                end

              _ ->
                "Failed to join group"
            end

          Logger.error("❌ FAILED TO JOIN GROUP")
          Logger.error("   Error: #{inspect(error)}")
          Logger.info("=" |> String.duplicate(80))

          {:noreply, put_flash(socket, :error, error_msg)}
      end
    end
  end

  @impl true
  def handle_info({:update_state, new_state}, socket) do
    {:noreply,
     socket
     |> assign(new_state)
     |> put_flash(:info, "Welcome back to the group!")}
  end

  @impl true
  def handle_event("leave_group", _params, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("🚪 LEAVE GROUP EVENT")
    Logger.info("=" |> String.duplicate(80))

    user_id_str = extract_user_id(socket.assigns.user)
    group_id = socket.assigns.group.id

    Logger.info("📍 User ID: #{user_id_str}")
    Logger.info("📍 Group ID: #{group_id}")

    try do
      Logger.info("🔵 Calling :groupdb.leave_group...")
      :groupdb.leave_group(to_charlist(user_id_str), to_charlist(group_id))
      Logger.info("✅ Successfully left group")
      Logger.info("=" |> String.duplicate(80))

      {:noreply,
       socket
       |> put_flash(:info, "You have left the group")
       |> redirect(to: "/#{socket.assigns.locale}/groups")}
    rescue
      error ->
        Logger.error("❌ FAILED TO LEAVE GROUP")
        Logger.error("   Error: #{inspect(error)}")
        Logger.info("=" |> String.duplicate(80))
        {:noreply, put_flash(socket, :error, "Failed to leave group")}
    end
  end

  @impl true
  def handle_event("send_message", %{"content" => content}, socket) do
    content_trimmed = String.trim(content)

    if content_trimmed == "" do
      {:noreply, socket}
    else
      Logger.info("=" |> String.duplicate(80))
      Logger.info("💬 SEND MESSAGE EVENT")
      Logger.info("=" |> String.duplicate(80))

      user_id_str = extract_user_id(socket.assigns.user)
      group_id = socket.assigns.group.id

      Logger.info("📍 User ID: #{user_id_str}")
      Logger.info("📍 Group ID: #{group_id}")
      Logger.info("📍 Content: #{String.slice(content_trimmed, 0, 50)}...")

      try do
        Logger.info("🔵 Calling :groupdb.send_message...")

        message_id =
          :groupdb.send_message(
            to_charlist(group_id),
            to_charlist(user_id_str),
            to_charlist(content_trimmed),
            []
          )

        Logger.info("✅ Message sent, ID: #{inspect(message_id)}")

        Process.sleep(100)

        Logger.info("🔄 Reloading messages...")
        messages = load_group_messages(group_id)
        Logger.info("✅ Loaded #{length(messages)} messages")
        Logger.info("=" |> String.duplicate(80))

        {:noreply,
         socket
         |> assign(messages: messages)
         |> assign(message_content: "")}
      rescue
        error ->
          Logger.error("❌ FAILED TO SEND MESSAGE")
          Logger.error("   Error: #{inspect(error)}")
          Logger.info("=" |> String.duplicate(80))
          {:noreply, put_flash(socket, :error, "Failed to send message")}
      end
    end
  end

  @impl true
  def handle_event("update_message_content", %{"content" => content}, socket) do
    {:noreply, assign(socket, message_content: content)}
  end

  @impl true
  def handle_event("delete_message", %{"message_id" => message_id}, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("🗑️ DELETE MESSAGE EVENT")
    Logger.info("=" |> String.duplicate(80))
    Logger.info("📍 Message ID: #{message_id}")

    user_id_str = extract_user_id(socket.assigns.user)
    Logger.info("📍 User ID: #{user_id_str}")

    try do
      Logger.info("🔵 Calling :groupdb.delete_message...")
      :groupdb.delete_message(to_charlist(message_id), to_charlist(user_id_str))
      Logger.info("✅ Message marked as deleted in database")

      Logger.info("⏳ Waiting 150ms for database consistency...")
      Process.sleep(150)

      Logger.info("🔄 Reloading all messages from database...")
      messages = load_group_messages(socket.assigns.group.id)

      Logger.info("✅ Final message list contains #{length(messages)} messages")
      Logger.info("=" |> String.duplicate(80))

      {:noreply,
       socket
       |> assign(messages: messages)
       |> put_flash(:info, "Message deleted")}
    rescue
      error ->
        Logger.error("❌ FAILED TO DELETE MESSAGE")
        Logger.error("   Error: #{inspect(error)}")
        Logger.info("=" |> String.duplicate(80))
        {:noreply, put_flash(socket, :error, "Failed to delete message")}
    end
  end

  @impl true
  def handle_event(
        "react_to_message",
        %{"message_id" => message_id, "reaction" => reaction},
        socket
      ) do
    Logger.info("👍 React to message: #{message_id} with #{reaction}")
    user_id_str = extract_user_id(socket.assigns.user)

    try do
      :groupdb.react_to_message(
        to_charlist(message_id),
        to_charlist(user_id_str),
        String.to_atom(reaction)
      )

      messages = load_group_messages(socket.assigns.group.id)
      {:noreply, assign(socket, messages: messages)}
    rescue
      error ->
        Logger.error("Failed to react to message: #{inspect(error)}")
        {:noreply, socket}
    end
  end

  @impl true
  def handle_event("remove_member", %{"member_id" => member_id}, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("🚫 REMOVE MEMBER EVENT")
    Logger.info("=" |> String.duplicate(80))

    user_id_str = extract_user_id(socket.assigns.user)
    group_id = socket.assigns.group.id

    Logger.info("📍 Admin ID: #{user_id_str}")
    Logger.info("📍 Member ID to remove: #{member_id}")
    Logger.info("📍 Group ID: #{group_id}")

    try do
      Logger.info("🔵 Calling :groupdb.remove_member...")

      :groupdb.remove_member(
        to_charlist(group_id),
        to_charlist(member_id),
        to_charlist(user_id_str)
      )

      Logger.info("✅ Member removed successfully")

      Logger.info("🔄 Reloading group data...")
      members = load_group_members(group_id)
      group = load_group(group_id)
      Logger.info("=" |> String.duplicate(80))

      {:noreply,
       socket
       |> assign(members: members)
       |> assign(group: group)
       |> put_flash(:info, "Member removed")}
    rescue
      error ->
        Logger.error("❌ FAILED TO REMOVE MEMBER")
        Logger.error("   Error: #{inspect(error)}")
        Logger.info("=" |> String.duplicate(80))
        {:noreply, put_flash(socket, :error, "Failed to remove member")}
    end
  end

  defp extract_user_id(user) do
    if is_binary(user.id), do: user.id, else: to_string(user.id)
  end

  defp load_group(group_id) do
    Logger.info("🔍 load_group called with ID: #{group_id}")

    try do
      Logger.info("🔵 Calling :groupdb.get_group...")
      group = :groupdb.get_group(to_charlist(group_id))
      Logger.info("✅ Raw group record received")

      converted = convert_group_record(group)
      Logger.info("✅ Converted group")
      converted
    rescue
      error ->
        Logger.error("❌ Failed to load group")
        Logger.error("   Error: #{inspect(error)}")
        nil
    catch
      :throw, {:transaction_failed, {:throw, :group_not_found}} ->
        Logger.error("❌ Group not found in database")
        nil

      :throw, reason ->
        Logger.error("❌ Caught throw: #{inspect(reason)}")
        nil
    end
  end

  defp load_group_messages(group_id) do
    Logger.info("📨 load_group_messages called for group: #{group_id}")

    try do
      Logger.info("🔵 Calling :groupdb.get_group_messages...")

      case :groupdb.get_group_messages(to_charlist(group_id), 50) do
        messages when is_list(messages) ->
          Logger.info("✅ Received #{length(messages)} raw messages from database")

          converted = Enum.map(messages, &convert_message_record/1)

          non_deleted =
            Enum.filter(converted, fn msg ->
              not msg.deleted
            end)

          Logger.info(
            "✅ Filtered to #{length(non_deleted)} non-deleted messages (removed #{length(converted) - length(non_deleted)} deleted)"
          )

          non_deleted

        other ->
          Logger.warning("⚠️ Unexpected result from get_group_messages: #{inspect(other)}")
          []
      end
    rescue
      error ->
        Logger.error("❌ Failed to load messages")
        Logger.error("   Error: #{inspect(error)}")
        []
    end
  end

  defp load_group_members(group_id) do
    Logger.info("👥 load_group_members called for group: #{group_id}")

    try do
      case :groupdb.get_group_members(to_charlist(group_id)) do
        members when is_list(members) ->
          Logger.info("✅ Received #{length(members)} members")
          converted = Enum.map(members, &convert_member_record/1)
          Logger.info("✅ Converted #{length(converted)} members")
          converted

        other ->
          Logger.warning("⚠️ Unexpected result: #{inspect(other)}")
          []
      end
    rescue
      error ->
        Logger.error("❌ Failed to load members")
        Logger.error("   Error: #{inspect(error)}")
        []
    end
  end

  defp is_member?(group, user_id) do
    if is_owner?(group, user_id) do
      true
    else
      Enum.any?(group.members, fn member_id ->
        to_string(member_id) == to_string(user_id)
      end)
    end
  end

  defp is_owner?(group, user_id) do
    to_string(group.owner_id) == to_string(user_id)
  end

  defp is_admin?(group, user_id) do
    member_result =
      Enum.any?(group.admins, fn admin_id ->
        to_string(admin_id) == to_string(user_id)
      end)

    owner_result = is_owner?(group, user_id)
    member_result || owner_result
  end

  defp convert_group_record(group) do
    admins_raw = elem(group, 10)
    members_raw = elem(group, 11)

    admins =
      case admins_raw do
        :undefined -> []
        list when is_list(list) -> Enum.map(list, &to_string/1)
        _ -> []
      end

    members =
      case members_raw do
        :undefined -> []
        list when is_list(list) -> Enum.map(list, &to_string/1)
        _ -> []
      end

    %{
      id: to_string(elem(group, 1)),
      unique_name: to_string(elem(group, 2)),
      name: to_string(elem(group, 3)),
      description: to_string(elem(group, 4) || ""),
      type: elem(group, 5),
      privacy: elem(group, 6),
      owner_id: to_string(elem(group, 7)),
      admins: admins,
      members: members,
      settings: elem(group, 15),
      member_count: elem(group, 19),
      date_created: elem(group, 20)
    }
  end

  defp convert_message_record(message) do
    %{
      id: to_string(elem(message, 1)),
      group_id: to_string(elem(message, 2)),
      user_id: to_string(elem(message, 3)),
      content: to_string(elem(message, 4)),
      media: elem(message, 5),
      reactions: elem(message, 9),
      reaction_counts: elem(message, 10),
      pinned: elem(message, 11),
      edited: elem(message, 12),
      deleted: elem(message, 13),
      date_created: elem(message, 14),
      date_updated: elem(message, 15)
    }
  end

  defp convert_member_record(member) do
    %{
      id: to_string(elem(member, 1)),
      group_id: to_string(elem(member, 2)),
      user_id: to_string(elem(member, 3)),
      role: elem(member, 4),
      permissions: elem(member, 5),
      join_date: elem(member, 6),
      invited_by: if(elem(member, 7) == :undefined, do: nil, else: to_string(elem(member, 7))),
      muted: elem(member, 8),
      notifications: elem(member, 9)
    }
  end

  def get_user_info(user_id) do
    Logger.debug("🔍 get_user_info for: #{inspect(user_id)}")

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

          Logger.debug("✅ Retrieved user: #{username}, avatar: #{inspect(avatar_url)}")

          %{
            id: to_string(user_id),
            username: username,
            avatar_url: avatar_url
          }

        _ ->
          %{id: to_string(user_id), username: "Unknown", avatar_url: "/images/default-avatar.png"}
      end
    rescue
      error ->
        Logger.error("❌ Error in get_user_info: #{inspect(error)}")
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

  def format_date(datetime) do
    case datetime do
      {{year, month, day}, _time} ->
        "#{day}/#{month}/#{year}"

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
