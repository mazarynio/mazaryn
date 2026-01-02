defmodule Core.GroupClient do
  def create_group(owner_id, unique_name, name, description, type, privacy, category, settings) do
    :groupdb.create_group(owner_id, unique_name, name, description, type, privacy, settings)
  end

  def get_group(group_id) do
    :groupdb.get_group(group_id)
  end

  def update_group(group_id, updates) do
    :groupdb.update_group(group_id, updates)
  end

  def delete_group(group_id) do
    :groupdb.delete_group(group_id)
  end

  def join_group(user_id, group_id) do
    :groupdb.join_group(user_id, group_id)
  end

  def leave_group(user_id, group_id) do
    :groupdb.leave_group(user_id, group_id)
  end

  def remove_member(group_id, member_id, admin_id) do
    :groupdb.remove_member(group_id, member_id, admin_id)
  end

  def add_admin(group_id, user_id, assigned_by, permissions) do
    :groupdb.add_admin(group_id, user_id, assigned_by, permissions)
  end

  def remove_admin(group_id, user_id) do
    :groupdb.remove_admin(group_id, user_id)
  end

  def send_invite(group_id, inviter_id, invitee_id, message) do
    :groupdb.send_invite(group_id, inviter_id, invitee_id, message)
  end

  def accept_invite(invite_id, user_id) do
    :groupdb.accept_invite(invite_id, user_id)
  end

  def reject_invite(invite_id, user_id) do
    :groupdb.reject_invite(invite_id, user_id)
  end

  def send_message(group_id, user_id, content, media) do
    :groupdb.send_message(group_id, user_id, content, media)
  end

  def edit_message(message_id, user_id, new_content) do
    :groupdb.edit_message(message_id, user_id, new_content)
  end

  def delete_message(message_id, user_id) do
    :groupdb.delete_message(message_id, user_id)
  end

  def pin_message(message_id, user_id) do
    :groupdb.pin_message(message_id, user_id)
  end

  def unpin_message(message_id, user_id) do
    :groupdb.unpin_message(message_id, user_id)
  end

  def react_to_message(message_id, user_id, reaction_type) do
    :groupdb.react_to_message(message_id, user_id, reaction_type)
  end

  def get_group_messages(group_id, limit) do
    :groupdb.get_group_messages(group_id, limit)
  end

  def get_member_groups(user_id) do
    :groupdb.get_member_groups(user_id)
  end

  def ban_user(group_id, user_id, admin_id) do
    :groupdb.ban_user(group_id, user_id, admin_id)
  end

  def unban_user(group_id, user_id) do
    :groupdb.unban_user(group_id, user_id)
  end

  def mute_member(group_id, user_id) do
    :groupdb.mute_member(group_id, user_id)
  end

  def unmute_member(group_id, user_id) do
    :groupdb.unmute_member(group_id, user_id)
  end

  def search_groups(query, privacy) do
    :groupdb.search_groups(query, privacy)
  end

  def get_group_members(group_id) do
    :groupdb.get_group_members(group_id)
  end

  def create_channel(owner_id, unique_name, name, description, privacy, category, settings) do
    :groupdb.create_channel(owner_id, unique_name, name, description, privacy, category, settings)
  end

  def get_channel(channel_id) do
    :groupdb.get_channel(channel_id)
  end

  def subscribe_channel(user_id, channel_id) do
    :groupdb.subscribe_channel(user_id, channel_id)
  end

  def unsubscribe_channel(user_id, channel_id) do
    :groupdb.unsubscribe_channel(user_id, channel_id)
  end

  def create_channel_post(channel_id, user_id, content, media) do
    :groupdb.create_channel_post(channel_id, user_id, content, media)
  end

  def get_channel_posts(channel_id, limit) do
    :groupdb.get_channel_posts(channel_id, limit)
  end

  def search_channels(query, privacy) do
    :groupdb.search_channels(query, privacy)
  end

  def get_user_channels(user_id) do
    :groupdb.get_user_channels(user_id)
  end
end
