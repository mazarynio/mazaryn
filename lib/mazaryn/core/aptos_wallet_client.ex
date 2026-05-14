defmodule Core.AptosWalletClient do
  def create_wallet(user_id, account_address, encrypted_data) do
    :aptos_wallet_server.create_wallet(
      to_erl(user_id),
      to_erl(account_address),
      normalize_map_keys(encrypted_data)
    )
  end

  def create_wallet(user_id, account_address, encrypted_data, label) do
    :aptos_wallet_server.create_wallet(
      to_erl(user_id),
      to_erl(account_address),
      normalize_map_keys(encrypted_data),
      to_erl(label)
    )
  end

  def get_wallet(wallet_id) do
    :aptos_wallet_server.get_wallet(to_erl(wallet_id))
  end

  def get_wallet_by_address(address) do
    :aptos_wallet_server.get_wallet_by_address(to_erl(address))
  end

  def get_user_wallets(user_id) do
    :aptos_wallet_server.get_user_wallets(to_erl(user_id))
  end

  def get_primary_wallet(user_id) do
    :aptos_wallet_server.get_primary_wallet(to_erl(user_id))
  end

  def set_primary_wallet(user_id, wallet_id) do
    :aptos_wallet_server.set_primary_wallet(to_erl(user_id), to_erl(wallet_id))
  end

  def update_wallet_last_used(wallet_id) do
    :aptos_wallet_server.update_wallet_last_used(to_erl(wallet_id))
  end

  def update_wallet_label(wallet_id, new_label) do
    :aptos_wallet_server.update_wallet_label(to_erl(wallet_id), to_erl(new_label))
  end

  def delete_wallet(wallet_id) do
    :aptos_wallet_server.delete_wallet(to_erl(wallet_id))
  end

  def wallet_exists(wallet_id) do
    :aptos_wallet_server.wallet_exists(to_erl(wallet_id))
  end

  def address_exists(address) do
    :aptos_wallet_server.address_exists(to_erl(address))
  end

  def create_transaction(wallet_id, tx_data) do
    :aptos_wallet_server.create_transaction(
      to_erl(wallet_id),
      normalize_map_keys(tx_data)
    )
  end

  def get_transaction(tx_id) do
    :aptos_wallet_server.get_transaction(to_erl(tx_id))
  end

  def get_wallet_transactions(wallet_id) do
    :aptos_wallet_server.get_wallet_transactions(to_erl(wallet_id))
  end

  def get_wallet_transactions(wallet_id, limit, offset) do
    :aptos_wallet_server.get_wallet_transactions(to_erl(wallet_id), limit, offset)
  end

  def get_user_transactions(user_id) do
    :aptos_wallet_server.get_user_transactions(to_erl(user_id))
  end

  def get_transactions_by_type(wallet_id, tx_type) do
    :aptos_wallet_server.get_transactions_by_type(to_erl(wallet_id), to_erl(tx_type))
  end

  def create_nft_collection(wallet_id, collection_data) do
    :aptos_wallet_server.create_nft_collection(
      to_erl(wallet_id),
      normalize_map_keys(collection_data)
    )
  end

  def get_nft_collection(collection_id) do
    :aptos_wallet_server.get_nft_collection(to_erl(collection_id))
  end

  def get_wallet_nft_collections(wallet_id) do
    :aptos_wallet_server.get_wallet_nft_collections(to_erl(wallet_id))
  end

  def get_user_nft_collections(user_id) do
    :aptos_wallet_server.get_user_nft_collections(to_erl(user_id))
  end

  def create_nft_token(wallet_id, token_data) do
    :aptos_wallet_server.create_nft_token(
      to_erl(wallet_id),
      normalize_map_keys(token_data)
    )
  end

  def get_nft_token(token_id) do
    :aptos_wallet_server.get_nft_token(to_erl(token_id))
  end

  def get_wallet_nft_tokens(wallet_id) do
    :aptos_wallet_server.get_wallet_nft_tokens(to_erl(wallet_id))
  end

  def get_collection_nft_tokens(collection_id) do
    :aptos_wallet_server.get_collection_nft_tokens(to_erl(collection_id))
  end

  def create_fungible_asset(wallet_id, fa_data) do
    :aptos_wallet_server.create_fungible_asset(
      to_erl(wallet_id),
      normalize_map_keys(fa_data)
    )
  end

  def get_fungible_asset(fa_id) do
    :aptos_wallet_server.get_fungible_asset(to_erl(fa_id))
  end

  def get_wallet_fungible_assets(wallet_id) do
    :aptos_wallet_server.get_wallet_fungible_assets(to_erl(wallet_id))
  end

  def get_user_fungible_assets(user_id) do
    :aptos_wallet_server.get_user_fungible_assets(to_erl(user_id))
  end

  @doc false
  def to_erl(v) when is_binary(v), do: String.to_charlist(v)
  def to_erl(v) when is_atom(v), do: v
  def to_erl(v) when is_integer(v), do: v
  def to_erl(nil), do: :undefined
  def to_erl(v), do: v

  @doc false
  def normalize_map_keys(map) when is_map(map) do
    Map.new(map, fn {k, v} ->
      erl_key = normalize_key(k)
      erl_val = normalize_value(v)
      {erl_key, erl_val}
    end)
  end

  defp normalize_key(k) when is_atom(k), do: k
  defp normalize_key(k) when is_binary(k), do: String.to_atom(k)
  defp normalize_key(k), do: k

  defp normalize_value(v) when is_binary(v), do: String.to_charlist(v)

  defp normalize_value(v) when is_list(v) do
    if Enum.all?(v, &is_integer/1) and length(v) > 0 do
      v
    else
      Enum.map(v, &normalize_value/1)
    end
  end

  defp normalize_value(v) when is_map(v), do: normalize_map_keys(v)
  defp normalize_value(v), do: v
end
