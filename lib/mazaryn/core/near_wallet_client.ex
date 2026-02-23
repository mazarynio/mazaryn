defmodule Core.NearWalletClient do
  def create_wallet(user_id, account_id, encrypted_data) do
    :near_wallet_server.create_wallet(
      to_erl_charlist(user_id),
      to_erl_charlist(account_id),
      normalize_map_keys(encrypted_data)
    )
  end

  def create_wallet(user_id, account_id, encrypted_data, label) do
    :near_wallet_server.create_wallet(
      to_erl_charlist(user_id),
      to_erl_charlist(account_id),
      normalize_map_keys(encrypted_data),
      to_erl_charlist(label)
    )
  end

  def get_wallet(wallet_id) do
    :near_wallet_server.get_wallet(to_erl_charlist(wallet_id))
  end

  def get_wallet_by_account_id(account_id) do
    :near_wallet_server.get_wallet_by_account_id(to_erl_charlist(account_id))
  end

  def get_user_wallets(user_id) do
    :near_wallet_server.get_user_wallets(to_erl_charlist(user_id))
  end

  def get_primary_wallet(user_id) do
    :near_wallet_server.get_primary_wallet(to_erl_charlist(user_id))
  end

  def set_primary_wallet(user_id, wallet_id) do
    :near_wallet_server.set_primary_wallet(
      to_erl_charlist(user_id),
      to_erl_charlist(wallet_id)
    )
  end

  def update_wallet_last_used(wallet_id) do
    :near_wallet_server.update_wallet_last_used(to_erl_charlist(wallet_id))
  end

  def update_wallet_label(wallet_id, new_label) do
    :near_wallet_server.update_wallet_label(
      to_erl_charlist(wallet_id),
      to_erl_charlist(new_label)
    )
  end

  def delete_wallet(wallet_id) do
    :near_wallet_server.delete_wallet(to_erl_charlist(wallet_id))
  end

  def wallet_exists(wallet_id) do
    :near_wallet_server.wallet_exists(to_erl_charlist(wallet_id))
  end

  def account_id_exists(account_id) do
    :near_wallet_server.account_id_exists(to_erl_charlist(account_id))
  end

  def create_transaction(wallet_id, tx_data) do
    :near_wallet_server.create_transaction(
      to_erl_charlist(wallet_id),
      normalize_map_keys(tx_data)
    )
  end

  def get_transaction(tx_id) do
    :near_wallet_server.get_transaction(to_erl_charlist(tx_id))
  end

  def get_wallet_transactions(wallet_id) do
    :near_wallet_server.get_wallet_transactions(to_erl_charlist(wallet_id))
  end

  def get_wallet_transactions(wallet_id, limit, offset) do
    :near_wallet_server.get_wallet_transactions(
      to_erl_charlist(wallet_id),
      limit,
      offset
    )
  end

  def get_user_transactions(user_id) do
    :near_wallet_server.get_user_transactions(to_erl_charlist(user_id))
  end

  def get_transactions_by_type(wallet_id, tx_type) do
    :near_wallet_server.get_transactions_by_type(
      to_erl_charlist(wallet_id),
      to_atom(tx_type)
    )
  end

  def create_access_key(wallet_id, key_data) do
    :near_wallet_server.create_access_key(
      to_erl_charlist(wallet_id),
      normalize_map_keys(key_data)
    )
  end

  def get_access_key(key_id) do
    :near_wallet_server.get_access_key(to_erl_charlist(key_id))
  end

  def get_wallet_access_keys(wallet_id) do
    :near_wallet_server.get_wallet_access_keys(to_erl_charlist(wallet_id))
  end

  def get_user_access_keys(user_id) do
    :near_wallet_server.get_user_access_keys(to_erl_charlist(user_id))
  end

  def delete_access_key(key_id) do
    :near_wallet_server.delete_access_key(to_erl_charlist(key_id))
  end

  def create_stake(wallet_id, stake_data) do
    :near_wallet_server.create_stake(
      to_erl_charlist(wallet_id),
      normalize_map_keys(stake_data)
    )
  end

  def get_stake(stake_id) do
    :near_wallet_server.get_stake(to_erl_charlist(stake_id))
  end

  def get_wallet_stakes(wallet_id) do
    :near_wallet_server.get_wallet_stakes(to_erl_charlist(wallet_id))
  end

  def get_user_stakes(user_id) do
    :near_wallet_server.get_user_stakes(to_erl_charlist(user_id))
  end

  def update_stake(stake_id, updates) do
    :near_wallet_server.update_stake(
      to_erl_charlist(stake_id),
      normalize_map_keys(updates)
    )
  end

  def create_implicit_account(wallet_id, implicit_data) do
    :near_wallet_server.create_implicit_account(
      to_erl_charlist(wallet_id),
      normalize_map_keys(implicit_data)
    )
  end

  def get_implicit_account(implicit_id) do
    :near_wallet_server.get_implicit_account(to_erl_charlist(implicit_id))
  end

  def get_implicit_account_by_account_id(account_id) do
    :near_wallet_server.get_implicit_account_by_account_id(to_erl_charlist(account_id))
  end

  def get_wallet_implicit_accounts(wallet_id) do
    :near_wallet_server.get_wallet_implicit_accounts(to_erl_charlist(wallet_id))
  end

  def mark_implicit_funded(implicit_id, funded_at) do
    :near_wallet_server.mark_implicit_funded(
      to_erl_charlist(implicit_id),
      funded_at
    )
  end

  def create_social_post(wallet_id, post_data) do
    :near_wallet_server.create_social_post(
      to_erl_charlist(wallet_id),
      normalize_map_keys(post_data)
    )
  end

  def get_social_post(post_id) do
    :near_wallet_server.get_social_post(to_erl_charlist(post_id))
  end

  def get_wallet_social_posts(wallet_id) do
    :near_wallet_server.get_wallet_social_posts(to_erl_charlist(wallet_id))
  end

  def get_user_social_posts(user_id) do
    :near_wallet_server.get_user_social_posts(to_erl_charlist(user_id))
  end

  defp to_erl_charlist(value) when is_list(value), do: value
  defp to_erl_charlist(value) when is_binary(value), do: String.to_charlist(value)
  defp to_erl_charlist(value) when is_atom(value), do: Atom.to_charlist(value)
  defp to_erl_charlist(value) when is_integer(value), do: Integer.to_charlist(value)
  defp to_erl_charlist(value), do: value

  defp to_atom(value) when is_atom(value), do: value
  defp to_atom(value) when is_binary(value), do: String.to_existing_atom(value)
  defp to_atom(value) when is_list(value), do: List.to_existing_atom(value)

  defp normalize_map_keys(map) when is_map(map) do
    Map.new(map, fn
      {k, v} when is_binary(k) -> {String.to_atom(k), normalize_value(v)}
      {k, v} -> {k, normalize_value(v)}
    end)
  end

  defp normalize_map_keys(value), do: value

  defp normalize_value(v) when is_binary(v), do: String.to_charlist(v)
  defp normalize_value(v) when is_map(v), do: normalize_map_keys(v)

  defp normalize_value(v) when is_list(v) do
    if Enum.all?(v, &is_integer/1) do
      v
    else
      Enum.map(v, &normalize_value/1)
    end
  end

  defp normalize_value(v), do: v
end
