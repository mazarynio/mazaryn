defmodule Core.GroupClient do
  def create_group(owner_id, unique_name, name, description, type, privacy, settings) do
    :groupdb.create_group(
      to_charlist(owner_id),
      to_charlist(unique_name),
      to_charlist(name),
      to_charlist(description),
      type,
      privacy,
      settings
    )
  end

  def get_group(group_id) do
    :groupdb.get_group(to_charlist(group_id))
  end

  def update_group(group_id, update_map) do
    :groupdb.update_group(to_charlist(group_id), update_map)
  end

  def delete_group(group_id) do
    :groupdb.delete_group(to_charlist(group_id))
  end

  def get_group_by_username(username) do
    :groupdb.get_group_by_username(to_charlist(username))
  end

  def join_group(user_id, group_id) do
    :groupdb.join_group(to_charlist(user_id), to_charlist(group_id))
  end

  def leave_group(user_id, group_id) do
    :groupdb.leave_group(to_charlist(user_id), to_charlist(group_id))
  end

  def remove_member(group_id, member_id, admin_id) do
    :groupdb.remove_member(
      to_charlist(group_id),
      to_charlist(member_id),
      to_charlist(admin_id)
    )
  end

  def get_group_members(group_id) do
    :groupdb.get_group_members(to_charlist(group_id))
  end

  def get_member_groups(user_id) do
    :groupdb.get_member_groups(to_charlist(user_id))
  end

  def update_member_settings(group_id, user_id, settings_map) do
    :groupdb.update_member_settings(
      to_charlist(group_id),
      to_charlist(user_id),
      settings_map
    )
  end

  def add_admin(group_id, user_id, assigned_by, permissions) do
    :groupdb.add_admin(
      to_charlist(group_id),
      to_charlist(user_id),
      to_charlist(assigned_by),
      permissions
    )
  end

  def remove_admin(group_id, user_id) do
    :groupdb.remove_admin(to_charlist(group_id), to_charlist(user_id))
  end

  def update_admin_permissions(group_id, user_id, new_permissions) do
    :groupdb.update_admin_permissions(
      to_charlist(group_id),
      to_charlist(user_id),
      new_permissions
    )
  end

  def send_invite(group_id, inviter_id, invitee_id, message) do
    :groupdb.send_invite(
      to_charlist(group_id),
      to_charlist(inviter_id),
      to_charlist(invitee_id),
      to_charlist(message)
    )
  end

  def accept_invite(invite_id, user_id) do
    :groupdb.accept_invite(to_charlist(invite_id), to_charlist(user_id))
  end

  def reject_invite(invite_id, user_id) do
    :groupdb.reject_invite(to_charlist(invite_id), to_charlist(user_id))
  end

  def cancel_invite(invite_id, inviter_id) do
    :groupdb.cancel_invite(to_charlist(invite_id), to_charlist(inviter_id))
  end

  def get_pending_invites(group_id) do
    :groupdb.get_pending_invites(to_charlist(group_id))
  end

  def get_user_invites(user_id) do
    :groupdb.get_user_invites(to_charlist(user_id))
  end

  def send_message(group_id, user_id, content, media \\ []) do
    :groupdb.send_message(
      to_charlist(group_id),
      to_charlist(user_id),
      to_charlist(content),
      media
    )
  end

  def edit_message(message_id, user_id, new_content) do
    :groupdb.edit_message(
      to_charlist(message_id),
      to_charlist(user_id),
      to_charlist(new_content)
    )
  end

  def delete_message(message_id, user_id) do
    :groupdb.delete_message(to_charlist(message_id), to_charlist(user_id))
  end

  def pin_message(message_id, user_id) do
    :groupdb.pin_message(to_charlist(message_id), to_charlist(user_id))
  end

  def unpin_message(message_id, user_id) do
    :groupdb.unpin_message(to_charlist(message_id), to_charlist(user_id))
  end

  def react_to_message(message_id, user_id, reaction_type) do
    :groupdb.react_to_message(
      to_charlist(message_id),
      to_charlist(user_id),
      reaction_type
    )
  end

  def get_group_messages(group_id, limit \\ 50) do
    :groupdb.get_group_messages(to_charlist(group_id), limit)
  end

  def ban_user(group_id, user_id, admin_id) do
    :groupdb.ban_user(
      to_charlist(group_id),
      to_charlist(user_id),
      to_charlist(admin_id)
    )
  end

  def unban_user(group_id, user_id) do
    :groupdb.unban_user(to_charlist(group_id), to_charlist(user_id))
  end

  def mute_member(group_id, user_id) do
    :groupdb.mute_member(to_charlist(group_id), to_charlist(user_id))
  end

  def unmute_member(group_id, user_id) do
    :groupdb.unmute_member(to_charlist(group_id), to_charlist(user_id))
  end

  def search_groups(query, privacy \\ :all) do
    :groupdb.search_groups(to_charlist(query), privacy)
  end

  def search_users_by_username(query) do
    :groupdb.search_users_by_username(to_charlist(query))
  end

  def get_user_by_username(username) do
    :groupdb.get_user_by_username(to_charlist(username))
  end

  def create_channel(owner_id, unique_name, name, description, privacy, category, settings) do
    :groupdb.create_channel(
      to_charlist(owner_id),
      to_charlist(unique_name),
      to_charlist(name),
      to_charlist(description),
      privacy,
      to_charlist(category),
      settings
    )
  end

  def get_channel(channel_id) do
    :groupdb.get_channel(to_charlist(channel_id))
  end

  def update_channel(channel_id, update_map) do
    :groupdb.update_channel(to_charlist(channel_id), update_map)
  end

  def delete_channel(channel_id) do
    :groupdb.delete_channel(to_charlist(channel_id))
  end

  def subscribe_channel(user_id, channel_id) do
    :groupdb.subscribe_channel(to_charlist(user_id), to_charlist(channel_id))
  end

  def unsubscribe_channel(user_id, channel_id) do
    :groupdb.unsubscribe_channel(to_charlist(user_id), to_charlist(channel_id))
  end

  def create_channel_post(channel_id, user_id, content, media) do
    :groupdb.create_channel_post(
      to_charlist(channel_id),
      to_charlist(user_id),
      to_charlist(content),
      media
    )
  end

  def edit_channel_post(post_id, user_id, new_content) do
    :groupdb.edit_channel_post(
      to_charlist(post_id),
      to_charlist(user_id),
      to_charlist(new_content)
    )
  end

  def delete_channel_post(post_id, user_id) do
    :groupdb.delete_channel_post(to_charlist(post_id), to_charlist(user_id))
  end

  def pin_channel_post(post_id, user_id) do
    :groupdb.pin_channel_post(to_charlist(post_id), to_charlist(user_id))
  end

  def unpin_channel_post(post_id, user_id) do
    :groupdb.unpin_channel_post(to_charlist(post_id), to_charlist(user_id))
  end

  def react_to_channel_post(post_id, user_id, reaction_type) do
    :groupdb.react_to_channel_post(
      to_charlist(post_id),
      to_charlist(user_id),
      reaction_type
    )
  end

  def comment_on_channel_post(post_id, user_id, content, media) do
    :groupdb.comment_on_channel_post(
      to_charlist(post_id),
      to_charlist(user_id),
      to_charlist(content),
      media
    )
  end

  def get_channel_posts(channel_id, limit \\ 50) do
    :groupdb.get_channel_posts(to_charlist(channel_id), limit)
  end

  def search_channels(query, privacy \\ :all) do
    :groupdb.search_channels(to_charlist(query), privacy)
  end

  def get_user_channels(user_id) do
    :groupdb.get_user_channels(to_charlist(user_id))
  end

  def add_channel_admin(channel_id, user_id, assigned_by) do
    :groupdb.add_channel_admin(
      to_charlist(channel_id),
      to_charlist(user_id),
      to_charlist(assigned_by)
    )
  end

  def remove_channel_admin(channel_id, user_id) do
    :groupdb.remove_channel_admin(to_charlist(channel_id), to_charlist(user_id))
  end

  def send_channel_invite(channel_id, inviter_id, invitee_id, message) do
    :groupdb.send_channel_invite(
      to_charlist(channel_id),
      to_charlist(inviter_id),
      to_charlist(invitee_id),
      to_charlist(message)
    )
  end

  def get_pending_channel_invites(channel_id) do
    :groupdb.get_pending_channel_invites(to_charlist(channel_id))
  end

  def get_user_channel_invites(user_id) do
    :groupdb.get_user_channel_invites(to_charlist(user_id))
  end

  def cancel_channel_invite(invite_id, inviter_id) do
    :groupdb.cancel_channel_invite(to_charlist(invite_id), to_charlist(inviter_id))
  end

  def accept_channel_invite(invite_id, user_id) do
    :groupdb.accept_channel_invite(to_charlist(invite_id), to_charlist(user_id))
  end

  def reject_channel_invite(invite_id, user_id) do
    :groupdb.reject_channel_invite(to_charlist(invite_id), to_charlist(user_id))
  end

  def get_channel_subscribers(channel_id) do
    :groupdb.get_channel_subscribers(to_charlist(channel_id))
  end
end
