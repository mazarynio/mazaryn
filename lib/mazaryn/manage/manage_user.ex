defmodule ManageUser do
  @doc """
  iex> ManageUser.get_users()
  [~c"qP}v@KffxeXQOA<rHnScjUzUBX%S%7nldr_2LrB{oAMmTjWfoIM8%wnol-D]m",
   ~c"[5tIvof9zcq@r-IqEYv[4S2W!KVha<K434@q{?c0IqO$$pQ^Iflvj0x<gdjgm"]
  """
  def get_users() do
    :manage_user.get_users()
  end

  ## Get all Users with their informations
  def get_users_info() do
    :manage_user.get_users_info()
  end

  ## Get user information based on user id
  def get_user_info(user_id) do
    :manage_user.get_user_info(user_id)
  end

  ## Get user by username
  def get_user(username) do
    :manage_user.get_user(username)
  end

  ## Remove user account by username
  def delete_account(username, admin_username) do
    :manage_user.delete_account(username, admin_username)
  end

  ## Verify user by username or user_id
  def verify_user(username_or_id, admin_username) do
    :manage_user.verify_user(username_or_id, admin_username)
  end

  ## Unverify user by username or user_id
  def unverify_user(username_or_id, admin_username) do
    :manage_user.unverify_user(username_or_id, admin_username)
  end

  ## Suspend user based on user_id and duration (target time)
  def suspend_user(user_id, duration) do
    :manage_user.suspend_user(user_id, duration)
  end

  ## Unsuspend user based on user_id
  def unsuspend_user(user_id) do
    :manage_user.unsuspend_user(user_id)
  end
end
