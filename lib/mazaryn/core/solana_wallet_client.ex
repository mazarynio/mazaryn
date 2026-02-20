defmodule Core.SolanaWalletClient do
  def create_wallet(user_id, label, user_password) do
    :solana_wallet_server.create_wallet(
      to_erl_charlist(user_id),
      to_erl_charlist(label),
      to_erl_charlist(user_password)
    )
  end

  def create_wallet(user_id, public_key, encrypted_private_key, iv, auth_tag, label) do
    :solana_wallet_server.create_wallet(
      to_erl_charlist(user_id),
      to_erl_charlist(public_key),
      to_erl_charlist(encrypted_private_key),
      to_erl_charlist(iv),
      to_erl_charlist(auth_tag),
      to_erl_charlist(label)
    )
  end

  def import_wallet(
        user_id,
        public_key,
        encrypted_private_key,
        iv,
        auth_tag,
        derivation_path,
        label
      ) do
    :solana_wallet_server.import_wallet(
      to_erl_charlist(user_id),
      to_erl_charlist(public_key),
      to_erl_charlist(encrypted_private_key),
      to_erl_charlist(iv),
      to_erl_charlist(auth_tag),
      to_erl_charlist(derivation_path),
      to_erl_charlist(label)
    )
  end

  def get_wallet(wallet_id) do
    :solana_wallet_server.get_wallet(to_erl_charlist(wallet_id))
  end

  def get_wallet_by_public_key(public_key) do
    :solana_wallet_server.get_wallet_by_public_key(to_erl_charlist(public_key))
  end

  def get_user_wallets(user_id) do
    :solana_wallet_server.get_user_wallets(to_erl_charlist(user_id))
  end

  def update_wallet_last_used(wallet_id) do
    :solana_wallet_server.update_wallet_last_used(to_erl_charlist(wallet_id))
  end

  def update_wallet_label(wallet_id, new_label) do
    :solana_wallet_server.update_wallet_label(
      to_erl_charlist(wallet_id),
      to_erl_charlist(new_label)
    )
  end

  def set_primary_wallet(user_id, wallet_id) do
    :solana_wallet_server.set_primary_wallet(
      to_erl_charlist(user_id),
      to_erl_charlist(wallet_id)
    )
  end

  def get_primary_wallet(user_id) do
    :solana_wallet_server.get_primary_wallet(to_erl_charlist(user_id))
  end

  def delete_wallet(wallet_id) do
    :solana_wallet_server.delete_wallet(to_erl_charlist(wallet_id))
  end

  def wallet_exists(wallet_id) do
    :solana_wallet_server.wallet_exists(to_erl_charlist(wallet_id))
  end

  def public_key_exists(public_key) do
    :solana_wallet_server.public_key_exists(to_erl_charlist(public_key))
  end

  def export_private_key(wallet_id, user_password) do
    :solana_wallet_server.export_private_key(
      to_erl_charlist(wallet_id),
      to_erl_charlist(user_password)
    )
  end

  def create_transaction(wallet_id, tx_data) do
    :solana_wallet_server.create_transaction(
      to_erl_charlist(wallet_id),
      normalize_map_keys(tx_data)
    )
  end

  def get_transaction(tx_id) do
    :solana_wallet_server.get_transaction(to_erl_charlist(tx_id))
  end

  def get_wallet_transactions(wallet_id) do
    :solana_wallet_server.get_wallet_transactions(to_erl_charlist(wallet_id))
  end

  def get_wallet_transactions(wallet_id, limit, offset) do
    :solana_wallet_server.get_wallet_transactions(
      to_erl_charlist(wallet_id),
      limit,
      offset
    )
  end

  def get_user_transactions(user_id) do
    :solana_wallet_server.get_user_transactions(to_erl_charlist(user_id))
  end

  def get_transactions_by_type(wallet_id, tx_type) do
    :solana_wallet_server.get_transactions_by_type(
      to_erl_charlist(wallet_id),
      to_atom(tx_type)
    )
  end

  def update_transaction_status(tx_id, status, confirmed_at) do
    :solana_wallet_server.update_transaction_status(
      to_erl_charlist(tx_id),
      to_atom(status),
      confirmed_at
    )
  end

  def create_airdrop(wallet_id, airdrop_data) do
    :solana_wallet_server.create_airdrop(
      to_erl_charlist(wallet_id),
      normalize_map_keys(airdrop_data)
    )
  end

  def get_airdrop(airdrop_id) do
    :solana_wallet_server.get_airdrop(to_erl_charlist(airdrop_id))
  end

  def update_airdrop(airdrop_id, updates) do
    :solana_wallet_server.update_airdrop(
      to_erl_charlist(airdrop_id),
      normalize_map_keys(updates)
    )
  end

  def get_user_airdrops(user_id) do
    :solana_wallet_server.get_user_airdrops(to_erl_charlist(user_id))
  end

  def get_wallet_airdrops(wallet_id) do
    :solana_wallet_server.get_wallet_airdrops(to_erl_charlist(wallet_id))
  end

  def create_airdrop_recipient(airdrop_id, recipient_data) do
    :solana_wallet_server.create_airdrop_recipient(
      to_erl_charlist(airdrop_id),
      normalize_map_keys(recipient_data)
    )
  end

  def get_airdrop_recipients(airdrop_id) do
    :solana_wallet_server.get_airdrop_recipients(to_erl_charlist(airdrop_id))
  end

  def update_airdrop_recipient(recipient_id, updates) do
    :solana_wallet_server.update_airdrop_recipient(
      to_erl_charlist(recipient_id),
      normalize_map_keys(updates)
    )
  end

  def create_stake_account(wallet_id, stake_data) do
    :solana_wallet_server.create_stake_account(
      to_erl_charlist(wallet_id),
      normalize_map_keys(stake_data)
    )
  end

  def get_stake_account(stake_id) do
    :solana_wallet_server.get_stake_account(to_erl_charlist(stake_id))
  end

  def get_stake_by_address(stake_account_address) do
    :solana_wallet_server.get_stake_by_address(to_erl_charlist(stake_account_address))
  end

  def update_stake_account(stake_id, updates) do
    :solana_wallet_server.update_stake_account(
      to_erl_charlist(stake_id),
      normalize_map_keys(updates)
    )
  end

  def get_user_stakes(user_id) do
    :solana_wallet_server.get_user_stakes(to_erl_charlist(user_id))
  end

  def get_wallet_stakes(wallet_id) do
    :solana_wallet_server.get_wallet_stakes(to_erl_charlist(wallet_id))
  end

  def create_token_account(wallet_id, token_data) do
    :solana_wallet_server.create_token_account(
      to_erl_charlist(wallet_id),
      normalize_map_keys(token_data)
    )
  end

  def get_token_account(token_account_id) do
    :solana_wallet_server.get_token_account(to_erl_charlist(token_account_id))
  end

  def get_wallet_token_accounts(wallet_id) do
    :solana_wallet_server.get_wallet_token_accounts(to_erl_charlist(wallet_id))
  end

  def update_token_account_balance(token_account_id, new_balance, last_synced) do
    :solana_wallet_server.update_token_account_balance(
      to_erl_charlist(token_account_id),
      to_erl_charlist(new_balance),
      last_synced
    )
  end

  def create_nft(wallet_id, nft_data) do
    :solana_wallet_server.create_nft(
      to_erl_charlist(wallet_id),
      normalize_map_keys(nft_data)
    )
  end

  def get_nft(nft_id) do
    :solana_wallet_server.get_nft(to_erl_charlist(nft_id))
  end

  def get_wallet_nfts(wallet_id) do
    :solana_wallet_server.get_wallet_nfts(to_erl_charlist(wallet_id))
  end

  def update_nft(nft_id, updates) do
    :solana_wallet_server.update_nft(
      to_erl_charlist(nft_id),
      normalize_map_keys(updates)
    )
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
