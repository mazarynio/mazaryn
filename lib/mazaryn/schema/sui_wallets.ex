defmodule Mazaryn.SuiWallets do
  alias Context.SuiWallet, as: Ctx

  defdelegate create_wallet(user_id, sui_address, encrypted_data), to: Ctx
  defdelegate create_wallet(user_id, sui_address, network, label), to: Ctx

  defdelegate get_wallet(wallet_id), to: Ctx
  defdelegate get_wallet_by_address(sui_address), to: Ctx
  defdelegate get_user_wallets(user_id), to: Ctx
  defdelegate get_user_wallets_by_network(user_id, network), to: Ctx
  defdelegate get_primary_wallet(user_id), to: Ctx
  defdelegate set_primary_wallet(user_id, wallet_id), to: Ctx
  defdelegate update_wallet_label(wallet_id, label), to: Ctx
  defdelegate update_wallet_last_used(wallet_id), to: Ctx
  defdelegate delete_wallet(wallet_id), to: Ctx
  defdelegate wallet_exists(wallet_id), to: Ctx
  defdelegate address_exists(sui_address), to: Ctx

  defdelegate create_transaction(wallet_id, tx_data), to: Ctx
  defdelegate get_transaction(tx_id), to: Ctx
  defdelegate get_transaction_by_digest(digest), to: Ctx
  defdelegate get_wallet_transactions(wallet_id), to: Ctx
  defdelegate get_wallet_transactions(wallet_id, limit, offset), to: Ctx
  defdelegate get_user_transactions(user_id), to: Ctx
  defdelegate get_transactions_by_type(wallet_id, tx_type), to: Ctx
  defdelegate update_transaction_status(digest, status, effects_status, checkpoint), to: Ctx

  defdelegate create_stake(wallet_id, stake_data), to: Ctx
  defdelegate get_stake(stake_id), to: Ctx
  defdelegate get_stake_by_staked_sui_id(staked_sui_id), to: Ctx
  defdelegate get_wallet_stakes(wallet_id), to: Ctx
  defdelegate get_user_stakes(user_id), to: Ctx
  defdelegate get_stakes_by_validator(wallet_id, validator_address), to: Ctx
  defdelegate update_stake(stake_id, updates), to: Ctx
  defdelegate delete_stake(stake_id), to: Ctx

  defdelegate upsert_object(wallet_id, object_data), to: Ctx
  defdelegate get_object(object_id), to: Ctx
  defdelegate get_wallet_objects(wallet_id), to: Ctx
  defdelegate get_wallet_nfts(wallet_id), to: Ctx
  defdelegate get_user_objects(user_id), to: Ctx
  defdelegate delete_object(object_id), to: Ctx

  defdelegate upsert_coin_balance(wallet_id, balance_data), to: Ctx
  defdelegate get_coin_balance(wallet_id, coin_type), to: Ctx
  defdelegate get_wallet_coin_balances(wallet_id), to: Ctx
  defdelegate get_user_coin_balances(user_id), to: Ctx
  defdelegate delete_coin_balance(balance_id), to: Ctx
end

defmodule Mazaryn.AptosWallets do
  alias Context.AptosWallet, as: Ctx

  defdelegate create_wallet(user_id, address, encrypted_data), to: Ctx
  defdelegate create_wallet(user_id, address, network, label), to: Ctx

  defdelegate get_wallet(wallet_id), to: Ctx
  defdelegate get_wallet_by_address(address), to: Ctx
  defdelegate get_user_wallets(user_id), to: Ctx
  defdelegate get_primary_wallet(user_id), to: Ctx
  defdelegate set_primary_wallet(user_id, wallet_id), to: Ctx
  defdelegate update_wallet_label(wallet_id, new_label), to: Ctx
  defdelegate update_wallet_last_used(wallet_id), to: Ctx
  defdelegate delete_wallet(wallet_id), to: Ctx
  defdelegate wallet_exists(wallet_id), to: Ctx
  defdelegate address_exists(address), to: Ctx

  defdelegate record_transaction(wallet_id, tx_data), to: Ctx
  defdelegate get_transaction(tx_id), to: Ctx
  defdelegate get_transaction_by_hash(hash), to: Ctx
  defdelegate get_wallet_transactions(wallet_id), to: Ctx
  defdelegate get_wallet_transactions(wallet_id, limit, offset), to: Ctx
  defdelegate get_user_transactions(user_id), to: Ctx
  defdelegate get_transactions_by_type(wallet_id, tx_type), to: Ctx

  defdelegate record_nft_collection(wallet_id, collection_data), to: Ctx
  defdelegate get_nft_collection(collection_id), to: Ctx
  defdelegate get_wallet_nft_collections(wallet_id), to: Ctx
  defdelegate get_user_nft_collections(user_id), to: Ctx

  defdelegate record_nft_token(wallet_id, token_data), to: Ctx
  defdelegate get_nft_token(token_id), to: Ctx
  defdelegate get_wallet_nft_tokens(wallet_id), to: Ctx
  defdelegate get_collection_nft_tokens(collection_id), to: Ctx

  defdelegate record_fungible_asset(wallet_id, fa_data), to: Ctx
  defdelegate get_wallet_fungible_assets(wallet_id), to: Ctx
  defdelegate get_user_fungible_assets(user_id), to: Ctx

  defdelegate create_stake(wallet_id, stake_data), to: Ctx
  defdelegate get_stake(stake_id), to: Ctx
  defdelegate get_wallet_stakes(wallet_id), to: Ctx
  defdelegate get_user_stakes(user_id), to: Ctx
  defdelegate get_stakes_by_pool(wallet_id, pool_address), to: Ctx
  defdelegate update_stake(stake_id, updates), to: Ctx
  defdelegate delete_stake(stake_id), to: Ctx

  defdelegate upsert_coin_balance(wallet_id, balance_data), to: Ctx
  defdelegate get_coin_balance(wallet_id, coin_type), to: Ctx
  defdelegate get_wallet_coin_balances(wallet_id), to: Ctx
  defdelegate get_user_coin_balances(user_id), to: Ctx
end
