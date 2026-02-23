defmodule Mazaryn.NearWallets do
  alias Context.NearWallet, as: Ctx

  defdelegate get_user_wallets(user_id), to: Ctx
  defdelegate get_wallet(wallet_id), to: Ctx
  defdelegate get_wallet_by_account_id(account_id), to: Ctx
  defdelegate get_primary_wallet(user_id), to: Ctx
  defdelegate create_wallet(user_id, account_id, encrypted_data), to: Ctx
  defdelegate create_wallet(user_id, account_id, network, label), to: Ctx
  defdelegate set_primary_wallet(user_id, wallet_id), to: Ctx
  defdelegate update_wallet_label(wallet_id, label), to: Ctx
  defdelegate update_wallet_last_used(wallet_id), to: Ctx
  defdelegate delete_wallet(wallet_id), to: Ctx
  defdelegate wallet_exists(wallet_id), to: Ctx
  defdelegate account_id_exists(account_id), to: Ctx

  defdelegate create_transaction(wallet_id, tx_data), to: Ctx
  defdelegate get_transaction(tx_id), to: Ctx
  defdelegate get_wallet_transactions(wallet_id), to: Ctx
  defdelegate get_wallet_transactions(wallet_id, page, page_size), to: Ctx
  defdelegate get_user_transactions(user_id), to: Ctx
  defdelegate get_transactions_by_type(wallet_id, tx_type), to: Ctx

  defdelegate create_access_key(wallet_id, key_data), to: Ctx
  defdelegate get_access_key(key_id), to: Ctx
  defdelegate get_wallet_access_keys(wallet_id), to: Ctx
  defdelegate get_user_access_keys(user_id), to: Ctx
  defdelegate delete_access_key(key_id), to: Ctx

  defdelegate create_stake(wallet_id, stake_data), to: Ctx
  defdelegate get_stake(stake_id), to: Ctx
  defdelegate get_wallet_stakes(wallet_id), to: Ctx
  defdelegate get_user_stakes(user_id), to: Ctx
  defdelegate update_stake(stake_id, updates), to: Ctx

  defdelegate create_implicit_account(wallet_id, account_data), to: Ctx
  defdelegate get_implicit_account(implicit_id), to: Ctx
  defdelegate get_implicit_account_by_account_id(account_id), to: Ctx
  defdelegate get_wallet_implicit_accounts(wallet_id), to: Ctx
  defdelegate mark_implicit_funded(implicit_id, funded_at), to: Ctx

  defdelegate create_social_post(wallet_id, post_data), to: Ctx
  defdelegate get_social_post(post_id), to: Ctx
  defdelegate get_wallet_social_posts(wallet_id), to: Ctx
  defdelegate get_user_social_posts(user_id), to: Ctx
end
