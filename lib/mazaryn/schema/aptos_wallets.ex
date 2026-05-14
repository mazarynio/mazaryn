defmodule Mazaryn.AptosWallets do
  alias Context.AptosWallet

  defdelegate create_wallet(user_id, address, encrypted_data), to: AptosWallet
  defdelegate create_wallet(user_id, address, network, label), to: AptosWallet
  defdelegate get_wallet(wallet_id), to: AptosWallet
  defdelegate get_wallet_by_address(address), to: AptosWallet
  defdelegate get_user_wallets(user_id), to: AptosWallet
  defdelegate get_primary_wallet(user_id), to: AptosWallet
  defdelegate set_primary_wallet(user_id, wallet_id), to: AptosWallet
  defdelegate update_wallet_label(wallet_id, new_label), to: AptosWallet
  defdelegate delete_wallet(wallet_id), to: AptosWallet

  defdelegate record_transaction(wallet_id, tx_data), to: AptosWallet
  defdelegate get_transaction(tx_id), to: AptosWallet
  defdelegate get_wallet_transactions(wallet_id), to: AptosWallet

  defdelegate record_nft_collection(wallet_id, collection_data), to: AptosWallet
  defdelegate get_nft_collection(collection_id), to: AptosWallet
  defdelegate get_wallet_nft_collections(wallet_id), to: AptosWallet
  defdelegate record_nft_token(wallet_id, token_data), to: AptosWallet
  defdelegate get_wallet_nft_tokens(wallet_id), to: AptosWallet

  defdelegate record_fungible_asset(wallet_id, fa_data), to: AptosWallet
  defdelegate get_wallet_fungible_assets(wallet_id), to: AptosWallet
end
