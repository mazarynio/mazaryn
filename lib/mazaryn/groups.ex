defmodule Mazaryn.Groups do
  require Logger

  def create_group(owner_id, unique_name, name, description, type, privacy, _category, settings) do
    Logger.info("=== CREATE GROUP DEBUG ===")
    Logger.info("Owner ID: #{owner_id}")
    Logger.info("Unique name: #{unique_name}")
    Logger.info("Name: #{name}")
    Logger.info("Type: #{type}")
    Logger.info("Privacy: #{privacy}")
    Logger.info("Settings: #{inspect(settings)}")
    try do
      group_id =
        :groupdb.create_group(
          to_charlist(owner_id),
          to_charlist(unique_name),
          to_charlist(name),
          to_charlist(description || ""),
          String.to_atom(type || "group"),
          String.to_atom(privacy || "public"),
          settings || %{}
        )
      Logger.info("Group created successfully with ID: #{inspect(group_id)}")
      {:ok, to_string(group_id)}
    catch
      kind, error ->
        Logger.error("Failed to create group - #{kind}: #{inspect(error)}")
        Logger.error("Stack trace: #{inspect(__STACKTRACE__)}")
        {:error, inspect(error)}
    end
  end

  def get_group(group_id) do
    Logger.info("=== GET GROUP DEBUG ===")
    Logger.info("Group ID: #{group_id}")
    try do
      group = :groupdb.get_group(to_charlist(group_id))
      Logger.info("Group found, tuple size: #{tuple_size(group)}")
      converted = convert_group_to_map(group)
      {:ok, converted}
    catch
      kind, error ->
        Logger.error("Get group failed - #{kind}: #{inspect(error)}")
        {:error, :not_found}
    end
  end

  def get_group_by_unique_name(unique_name) when is_binary(unique_name) do
    Logger.info("=== GET GROUP BY UNIQUE NAME DEBUG ===")
    Logger.info("Unique name: #{unique_name}")
    try do
      groups = :groupdb.search_groups(to_charlist(unique_name), :all)
      Logger.info("Search returned #{length(groups)} groups")
      case Enum.find(groups, fn g ->
             elem(g, 2) == to_charlist(unique_name)
           end) do
        nil ->
          Logger.error("No group found with unique_name: #{unique_name}")
          {:error, :not_found}
        group ->
          Logger.info("Found group!")
          converted = convert_group_to_map(group)
          {:ok, converted}
      end
    catch
      kind, error ->
        Logger.error("Search failed - #{kind}: #{inspect(error)}")
        {:error, :not_found}
    end
  end

  def get_group_by_unique_name(_), do: {:error, :not_found}

  def get_user_groups(user_id) do
    Logger.info("=== GET USER GROUPS DEBUG ===")
    Logger.info("User ID: #{user_id}")
    try do
      groups = :groupdb.get_member_groups(to_charlist(user_id))
      Logger.info("Found #{length(groups)} groups for user")
      Enum.map(groups, &convert_group_to_map/1)
    catch
      kind, error ->
        Logger.error("Get user groups failed - #{kind}: #{inspect(error)}")
        []
    end
  end

  def search_public_groups(query, limit \\ 20) do
    Logger.info("=== SEARCH PUBLIC GROUPS DEBUG ===")
    try do
      groups = :groupdb.search_groups(to_charlist(query || ""), :public)
      Logger.info("Search returned #{length(groups)} groups")
      groups
      |> Enum.map(&convert_group_to_map/1)
      |> Enum.take(limit)
    catch
      kind, error ->
        Logger.error("Search failed - #{kind}: #{inspect(error)}")
        []
    end
  end

  def join_group(user_id, group_id) do
    Logger.info("=== JOIN GROUP DEBUG ===")
    Logger.info("User: #{user_id}, Group: #{group_id}")
    try do
      result = :groupdb.join_group(to_charlist(user_id), to_charlist(group_id))
      Logger.info("Join successful: #{inspect(result)}")
      {:ok, to_string(result)}
    catch
      kind, error ->
        Logger.error("Join failed - #{kind}: #{inspect(error)}")
        {:error, error}
    end
  end

  def leave_group(user_id, group_id) do
    Logger.info("=== LEAVE GROUP DEBUG ===")
    try do
      :groupdb.leave_group(to_charlist(user_id), to_charlist(group_id))
      Logger.info("Leave successful")
      :ok
    catch
      kind, error ->
        Logger.error("Leave failed - #{kind}: #{inspect(error)}")
        {:error, error}
    end
  end

  def send_message(group_id, user_id, content, media \\ []) do
    Logger.info("=== SEND MESSAGE DEBUG ===")
    Logger.info("Group: #{group_id}, User: #{user_id}")
    try do
      message_id =
        :groupdb.send_message(
          to_charlist(group_id),
          to_charlist(user_id),
          to_charlist(content),
          media
        )
      Logger.info("Message sent: #{inspect(message_id)}")
      {:ok, to_string(message_id)}
    catch
      kind, error ->
        Logger.error("Send message failed - #{kind}: #{inspect(error)}")
        {:error, :send_failed}
    end
  end

  def get_group_messages(group_id, limit \\ 50) do
    Logger.info("=== GET GROUP MESSAGES DEBUG ===")
    Logger.info("Group: #{group_id}, Limit: #{limit}")
    try do
      messages = :groupdb.get_group_messages(to_charlist(group_id), limit)
      Logger.info("Found #{length(messages)} messages")
      Enum.map(messages, &convert_message_to_map/1)
    catch
      kind, error ->
        Logger.error("Get messages failed - #{kind}: #{inspect(error)}")
        []
    end
  end

  def get_group_members(group_id) do
    Logger.info("=== GET GROUP MEMBERS DEBUG ===")
    Logger.info("Group ID: #{group_id}")
    try do
      members = :groupdb.get_group_members(to_charlist(group_id))
      Logger.info("Found #{length(members)} member records")
      converted = Enum.map(members, &convert_member_to_map/1)
      Logger.info("Member user_ids: #{inspect(Enum.map(converted, & &1.user_id))}")
      converted
    catch
      kind, error ->
        Logger.error("Get members failed - #{kind}: #{inspect(error)}")
        []
    end
  end

  def send_invite(group_id, inviter_id, invitee_username, message) do
    Logger.info("=== SEND INVITE DEBUG ===")
    try do
      case Account.Users.one_by_username(to_charlist(invitee_username)) do
        {:ok, user} ->
          invite_id =
            :groupdb.send_invite(
              to_charlist(group_id),
              to_charlist(inviter_id),
              to_charlist(user.id),
              to_charlist(message || "")
            )
          Logger.info("Invite sent: #{inspect(invite_id)}")
          {:ok, to_string(invite_id)}
        _ ->
          {:error, :user_not_found}
      end
    catch
      kind, error ->
        Logger.error("Send invite failed - #{kind}: #{inspect(error)}")
        {:error, :invite_failed}
    end
  end

  def add_admin(group_id, user_id, assigned_by, permissions) do
    Logger.info("=== ADD ADMIN DEBUG ===")
    Logger.info("Group: #{group_id}, User: #{user_id}, Assigned by: #{assigned_by}")
    Logger.info("Permissions: #{inspect(permissions)}")
    try do
      :groupdb.add_admin(
        to_charlist(group_id),
        to_charlist(user_id),
        to_charlist(assigned_by),
        permissions
      )
      Logger.info("Admin added successfully")
      :ok
    catch
      kind, error ->
        Logger.error("Add admin failed - #{kind}: #{inspect(error)}")
        {:error, :failed}
    end
  end

  def remove_admin(group_id, user_id) do
    Logger.info("=== REMOVE ADMIN DEBUG ===")
    Logger.info("Group: #{group_id}, User: #{user_id}")
    try do
      :groupdb.remove_admin(to_charlist(group_id), to_charlist(user_id))
      Logger.info("Admin removed successfully")
      :ok
    catch
      kind, error ->
        Logger.error("Remove admin failed - #{kind}: #{inspect(error)}")
        {:error, :failed}
    end
  end

  def remove_member(group_id, member_id, admin_id) do
    Logger.info("=== REMOVE MEMBER DEBUG ===")
    Logger.info("Group: #{group_id}, Member: #{member_id}, Admin: #{admin_id}")
    try do
      :groupdb.remove_member(
        to_charlist(group_id),
        to_charlist(member_id),
        to_charlist(admin_id)
      )
      Logger.info("Member removed successfully")
      :ok
    catch
      kind, error ->
        Logger.error("Remove member failed - #{kind}: #{inspect(error)}")
        {:error, :failed}
    end
  end

  def ban_user(group_id, user_id, admin_id) do
    Logger.info("=== BAN USER DEBUG ===")
    Logger.info("Group: #{group_id}, User: #{user_id}, Admin: #{admin_id}")
    try do
      :groupdb.ban_user(
        to_charlist(group_id),
        to_charlist(user_id),
        to_charlist(admin_id)
      )
      Logger.info("User banned successfully")
      :ok
    catch
      kind, error ->
        Logger.error("Ban user failed - #{kind}: #{inspect(error)}")
        {:error, :failed}
    end
  end

  def react_to_message(message_id, user_id, reaction_type) do
    Logger.info("=== REACT TO MESSAGE DEBUG ===")
    Logger.info("Message: #{message_id}, User: #{user_id}, Reaction: #{reaction_type}")
    try do
      reaction_atom = if is_binary(reaction_type), do: String.to_atom(reaction_type), else: reaction_type
      result = :groupdb.react_to_message(
        to_charlist(message_id),
        to_charlist(user_id),
        reaction_atom
      )
      Logger.info("Reaction result: #{inspect(result)}")
      {:ok, result}
    catch
      kind, error ->
        Logger.error("React failed - #{kind}: #{inspect(error)}")
        {:error, :failed}
    end
  end

  def delete_message(message_id, user_id) do
    Logger.info("=== DELETE MESSAGE DEBUG ===")
    Logger.info("Message: #{message_id}, User: #{user_id}")
    try do
      :groupdb.delete_message(to_charlist(message_id), to_charlist(user_id))
      Logger.info("Message deleted successfully")
      :ok
    catch
      kind, error ->
        Logger.error("Delete message failed - #{kind}: #{inspect(error)}")
        {:error, :failed}
    end
  end

  def delete_group(group_id, user_id) do
    Logger.info("=== DELETE GROUP DEBUG ===")
    Logger.info("Group: #{group_id}, User: #{user_id}")
    try do
      case get_group_by_id(group_id) do
        {:ok, group} ->
          Logger.info("Group found, Owner: #{group.owner_id}, User: #{user_id}")
          if group.owner_id == user_id do
            :groupdb.delete_group(to_charlist(group_id))
            Logger.info("Delete successful")
            :ok
          else
            Logger.error("User is not the owner - Owner: #{group.owner_id}, User: #{user_id}")
            {:error, :not_owner}
          end
        {:error, reason} ->
          Logger.error("Group not found: #{inspect(reason)}")
          {:error, reason}
      end
    catch
      kind, error ->
        Logger.error("Delete failed - #{kind}: #{inspect(error)}")
        {:error, :failed}
    end
  end

  def get_group_by_id(group_id) when is_binary(group_id) do
    Logger.info("=== GET GROUP BY ID DEBUG ===")
    Logger.info("Group ID: #{group_id}")
    try do
      group = :groupdb.get_group(to_charlist(group_id))
      Logger.info("Group found by ID")
      converted = convert_group_to_map(group)
      Logger.info("Converted group - ID: #{converted.id}, Owner: #{converted.owner_id}, Admins: #{inspect(converted.admins)}, Members: #{inspect(converted.members)}")
      {:ok, converted}
    catch
      :throw, :group_not_found ->
        Logger.warn("Group not found for ID: #{group_id}")
        {:error, :not_found}
      kind, reason ->
        Logger.error("Error fetching group #{group_id}: #{kind} #{inspect(reason)}")
        {:error, :not_found}
    end
  end

  def get_group_by_id(_), do: {:error, :not_found}

  defp convert_group_to_map(group) do
    Logger.debug("Converting group tuple (size: #{tuple_size(group)})")
    converted = %{
      id: safe_to_string(elem(group, 1)),
      unique_name: safe_to_string(elem(group, 2)),
      name: safe_to_string(elem(group, 3)),
      description: safe_to_string(elem(group, 4)),
      type: safe_to_string(elem(group, 5)),
      privacy: safe_to_string(elem(group, 6)),
      owner_id: safe_to_string(elem(group, 7)),
      avatar_url: safe_to_string(elem(group, 8)),
      banner_url: safe_to_string(elem(group, 9)),
      admins: safe_list_to_strings(elem(group, 10)),
      members: safe_list_to_strings(elem(group, 11)),
      settings: elem(group, 16) || %{},
      category: safe_to_string(elem(group, 18)),
      member_count: elem(group, 19) || 0,
      date_created: elem(group, 21),
      date_updated: elem(group, 22),
      last_activity: elem(group, 23)
    }
    Logger.debug("Converted: Owner=#{converted.owner_id}, Admins=#{inspect(converted.admins)}, Members=#{inspect(converted.members)}")
    converted
  end

  defp convert_message_to_map(message) do
    %{
      id: safe_to_string(elem(message, 1)),
      group_id: safe_to_string(elem(message, 2)),
      sender_id: safe_to_string(elem(message, 3)),
      content: safe_to_string(elem(message, 4)),
      media: elem(message, 5) || [],
      reactions: elem(message, 9) || %{},
      edited: elem(message, 11) || false,
      deleted: elem(message, 12) || false,
      pinned: elem(message, 13) || false,
      timestamp: elem(message, 14),
      date_created: elem(message, 14),
      date_updated: elem(message, 15)
    }
  end

  defp convert_member_to_map(member) do
    %{
      id: safe_to_string(elem(member, 1)),
      group_id: safe_to_string(elem(member, 2)),
      user_id: safe_to_string(elem(member, 3)),
      role: elem(member, 4) || :member,
      permissions: elem(member, 5) || [],
      join_date: elem(member, 6),
      invited_by: safe_to_string(elem(member, 7)),
      muted: elem(member, 8) || false,
      notifications: elem(member, 9) || true
    }
  end

  defp safe_to_string(nil), do: nil
  defp safe_to_string(:undefined), do: nil
  defp safe_to_string(value) when is_list(value), do: to_string(value)
  defp safe_to_string(value) when is_binary(value), do: value
  defp safe_to_string(value) when is_atom(value), do: Atom.to_string(value)
  defp safe_to_string(value), do: to_string(value)

  defp safe_list_to_strings(nil), do: []
  defp safe_list_to_strings(:undefined), do: []
  defp safe_list_to_strings(list) when is_list(list) do
    Enum.map(list, &safe_to_string/1)
  end
  defp safe_list_to_strings(_), do: []
end
