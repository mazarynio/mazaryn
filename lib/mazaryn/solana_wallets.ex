defmodule Mazaryn.SolanaWallets do
  alias Schema.SolanaWallet
  alias Schema.SolanaTransaction
  alias Schema.SolanaAirdrop
  alias Schema.SolanaAirdropRecipient
  alias Schema.SolanaStakeAccount
  alias Schema.SolanaTokenAccount
  alias Schema.SolanaNFT
  alias Core.SolanaWalletClient
  require Logger

  def create_wallet(user_id, label, user_password) do
    Logger.info("create_wallet - user_id: #{user_id}, label: #{label}")

    try do
      case SolanaWalletClient.create_wallet(user_id, label, user_password) do
        {:ok, wallet_id, public_key} ->
          Logger.info("create_wallet succeeded - wallet_id: #{wallet_id}")
          {:ok, %{wallet_id: to_string(wallet_id), public_key: to_string(public_key)}}

        {:ok, wallet_id} ->
          Logger.info("create_wallet succeeded - wallet_id: #{wallet_id}")
          {:ok, %{wallet_id: to_string(wallet_id)}}

        {:error, :public_key_already_exists} ->
          Logger.warning("create_wallet failed - public key already exists")
          {:error, :public_key_already_exists}

        {:error, reason} ->
          Logger.error("create_wallet failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in create_wallet: #{inspect(error)}")
        {:error, :wallet_creation_failed}
    end
  end

  def create_wallet(user_id, public_key, encrypted_private_key, iv, auth_tag, label) do
    Logger.info("create_wallet legacy - user_id: #{user_id}")

    try do
      case SolanaWalletClient.create_wallet(
             user_id,
             public_key,
             encrypted_private_key,
             iv,
             auth_tag,
             label
           ) do
        {:ok, wallet_id} ->
          Logger.info("create_wallet legacy succeeded - wallet_id: #{wallet_id}")
          {:ok, %{wallet_id: to_string(wallet_id)}}

        {:error, reason} ->
          Logger.error("create_wallet legacy failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in create_wallet legacy: #{inspect(error)}")
        {:error, :wallet_creation_failed}
    end
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
    Logger.info("import_wallet - user_id: #{user_id}")

    try do
      case SolanaWalletClient.import_wallet(
             user_id,
             public_key,
             encrypted_private_key,
             iv,
             auth_tag,
             derivation_path,
             label
           ) do
        {:ok, wallet_id} ->
          Logger.info("import_wallet succeeded - wallet_id: #{wallet_id}")
          {:ok, %{wallet_id: to_string(wallet_id)}}

        {:error, :public_key_already_exists} ->
          Logger.warning("import_wallet failed - public key already exists")
          {:error, :public_key_already_exists}

        {:error, reason} ->
          Logger.error("import_wallet failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in import_wallet: #{inspect(error)}")
        {:error, :wallet_import_failed}
    end
  end

  def get_wallet(wallet_id) do
    Logger.debug("get_wallet - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.get_wallet(wallet_id) do
        {:ok, erl_wallet} ->
          case SolanaWallet.erl_changeset_safe(erl_wallet) do
            {:ok, changeset} ->
              SolanaWallet.build(changeset)

            {:error, reason} ->
              Logger.error("get_wallet invalid data: #{reason}")
              {:error, :invalid_data}
          end

        {:error, :wallet_not_found} ->
          Logger.info("get_wallet not found: #{wallet_id}")
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("get_wallet failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_wallet: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_wallet_by_public_key(public_key) do
    Logger.debug("get_wallet_by_public_key - public_key: #{public_key}")

    try do
      case SolanaWalletClient.get_wallet_by_public_key(public_key) do
        {:ok, erl_wallet} ->
          case SolanaWallet.erl_changeset_safe(erl_wallet) do
            {:ok, changeset} ->
              SolanaWallet.build(changeset)

            {:error, reason} ->
              Logger.error("get_wallet_by_public_key invalid data: #{reason}")
              {:error, :invalid_data}
          end

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("get_wallet_by_public_key failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_wallet_by_public_key: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_user_wallets(user_id) do
    Logger.debug("get_user_wallets - user_id: #{user_id}")

    try do
      case SolanaWalletClient.get_user_wallets(user_id) do
        {:ok, erl_wallets} ->
          wallets =
            erl_wallets
            |> Enum.map(fn erl_wallet ->
              case SolanaWallet.erl_changeset_safe(erl_wallet) do
                {:ok, changeset} ->
                  case SolanaWallet.build(changeset) do
                    {:ok, wallet} -> wallet
                    {:error, _} -> nil
                  end

                {:error, _} ->
                  nil
              end
            end)
            |> Enum.filter(&(&1 != nil))

          {:ok, wallets}

        {:error, reason} ->
          Logger.error("get_user_wallets failed for #{user_id}: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_user_wallets: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_primary_wallet(user_id) do
    Logger.debug("get_primary_wallet - user_id: #{user_id}")

    try do
      case SolanaWalletClient.get_primary_wallet(user_id) do
        {:ok, erl_wallet} ->
          case SolanaWallet.erl_changeset_safe(erl_wallet) do
            {:ok, changeset} ->
              SolanaWallet.build(changeset)

            {:error, reason} ->
              Logger.error("get_primary_wallet invalid data: #{reason}")
              {:error, :invalid_data}
          end

        {:error, :no_wallets_found} ->
          {:error, :no_wallets_found}

        {:error, reason} ->
          Logger.error("get_primary_wallet failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_primary_wallet: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def set_primary_wallet(user_id, wallet_id) do
    Logger.info("set_primary_wallet - user_id: #{user_id}, wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.set_primary_wallet(user_id, wallet_id) do
        :ok ->
          Logger.info("set_primary_wallet succeeded")
          :ok

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, :access_denied} ->
          {:error, :access_denied}

        {:error, reason} ->
          Logger.error("set_primary_wallet failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in set_primary_wallet: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def update_wallet_label(wallet_id, new_label) do
    Logger.info("update_wallet_label - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.update_wallet_label(wallet_id, new_label) do
        :ok ->
          :ok

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("update_wallet_label failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in update_wallet_label: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def update_wallet_last_used(wallet_id) do
    try do
      SolanaWalletClient.update_wallet_last_used(wallet_id)
    rescue
      error ->
        Logger.error("Exception in update_wallet_last_used: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def delete_wallet(wallet_id) do
    Logger.info("delete_wallet - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.delete_wallet(wallet_id) do
        :ok ->
          Logger.info("delete_wallet succeeded")
          :ok

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("delete_wallet failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in delete_wallet: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def wallet_exists(wallet_id) do
    try do
      SolanaWalletClient.wallet_exists(wallet_id)
    rescue
      error ->
        Logger.error("Exception in wallet_exists: #{inspect(error)}")
        false
    end
  end

  def public_key_exists(public_key) do
    try do
      SolanaWalletClient.public_key_exists(public_key)
    rescue
      error ->
        Logger.error("Exception in public_key_exists: #{inspect(error)}")
        false
    end
  end

  def export_private_key(wallet_id, user_password) do
    Logger.info("export_private_key - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.export_private_key(wallet_id, user_password) do
        {:ok, private_key} ->
          {:ok, to_string(private_key)}

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("export_private_key failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in export_private_key: #{inspect(error)}")
        {:error, :export_failed}
    end
  end

  def create_transaction(wallet_id, tx_data) do
    Logger.info("create_transaction - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.create_transaction(wallet_id, tx_data) do
        {:ok, tx_id} ->
          Logger.info("create_transaction succeeded - tx_id: #{tx_id}")
          {:ok, to_string(tx_id)}

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("create_transaction failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in create_transaction: #{inspect(error)}")
        {:error, :transaction_failed}
    end
  end

  def get_transaction(tx_id) do
    Logger.debug("get_transaction - tx_id: #{tx_id}")

    try do
      case SolanaWalletClient.get_transaction(tx_id) do
        {:ok, erl_tx} ->
          case SolanaTransaction.erl_changeset_safe(erl_tx) do
            {:ok, changeset} ->
              SolanaTransaction.build(changeset)

            {:error, reason} ->
              Logger.error("get_transaction invalid data: #{reason}")
              {:error, :invalid_data}
          end

        {:error, :transaction_not_found} ->
          {:error, :transaction_not_found}

        {:error, reason} ->
          Logger.error("get_transaction failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_transaction: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_wallet_transactions(wallet_id) do
    Logger.debug("get_wallet_transactions - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.get_wallet_transactions(wallet_id) do
        {:ok, erl_txs} ->
          transactions = parse_transaction_list(erl_txs)
          {:ok, transactions}

        {:error, reason} ->
          Logger.error("get_wallet_transactions failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_wallet_transactions: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_wallet_transactions(wallet_id, limit, offset) do
    Logger.debug("get_wallet_transactions paginated - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.get_wallet_transactions(wallet_id, limit, offset) do
        {:ok, erl_txs, total} ->
          transactions = parse_transaction_list(erl_txs)
          {:ok, transactions, total}

        {:error, reason} ->
          Logger.error("get_wallet_transactions paginated failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_wallet_transactions paginated: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_user_transactions(user_id) do
    Logger.debug("get_user_transactions - user_id: #{user_id}")

    try do
      case SolanaWalletClient.get_user_transactions(user_id) do
        {:ok, erl_txs} ->
          transactions = parse_transaction_list(erl_txs)
          {:ok, transactions}

        {:error, reason} ->
          Logger.error("get_user_transactions failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_user_transactions: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_transactions_by_type(wallet_id, tx_type) do
    Logger.debug("get_transactions_by_type - wallet_id: #{wallet_id}, type: #{tx_type}")

    try do
      case SolanaWalletClient.get_transactions_by_type(wallet_id, tx_type) do
        {:ok, erl_txs} ->
          transactions = parse_transaction_list(erl_txs)
          {:ok, transactions}

        {:error, reason} ->
          Logger.error("get_transactions_by_type failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_transactions_by_type: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def update_transaction_status(tx_id, status, confirmed_at) do
    Logger.info("update_transaction_status - tx_id: #{tx_id}, status: #{status}")

    try do
      case SolanaWalletClient.update_transaction_status(tx_id, status, confirmed_at) do
        :ok ->
          :ok

        {:error, :transaction_not_found} ->
          {:error, :transaction_not_found}

        {:error, reason} ->
          Logger.error("update_transaction_status failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in update_transaction_status: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def create_airdrop(wallet_id, airdrop_data) do
    Logger.info("create_airdrop - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.create_airdrop(wallet_id, airdrop_data) do
        {:ok, airdrop_id} ->
          Logger.info("create_airdrop succeeded - airdrop_id: #{airdrop_id}")
          {:ok, to_string(airdrop_id)}

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("create_airdrop failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in create_airdrop: #{inspect(error)}")
        {:error, :airdrop_creation_failed}
    end
  end

  def get_airdrop(airdrop_id) do
    Logger.debug("get_airdrop - airdrop_id: #{airdrop_id}")

    try do
      case SolanaWalletClient.get_airdrop(airdrop_id) do
        {:ok, erl_airdrop} ->
          case SolanaAirdrop.erl_changeset_safe(erl_airdrop) do
            {:ok, changeset} ->
              SolanaAirdrop.build(changeset)

            {:error, reason} ->
              Logger.error("get_airdrop invalid data: #{reason}")
              {:error, :invalid_data}
          end

        {:error, :airdrop_not_found} ->
          {:error, :airdrop_not_found}

        {:error, reason} ->
          Logger.error("get_airdrop failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_airdrop: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def update_airdrop(airdrop_id, updates) do
    Logger.info("update_airdrop - airdrop_id: #{airdrop_id}")

    try do
      case SolanaWalletClient.update_airdrop(airdrop_id, updates) do
        :ok ->
          :ok

        {:error, :airdrop_not_found} ->
          {:error, :airdrop_not_found}

        {:error, reason} ->
          Logger.error("update_airdrop failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in update_airdrop: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def get_user_airdrops(user_id) do
    Logger.debug("get_user_airdrops - user_id: #{user_id}")

    try do
      case SolanaWalletClient.get_user_airdrops(user_id) do
        {:ok, erl_airdrops} ->
          airdrops = parse_airdrop_list(erl_airdrops)
          {:ok, airdrops}

        {:error, reason} ->
          Logger.error("get_user_airdrops failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_user_airdrops: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_wallet_airdrops(wallet_id) do
    Logger.debug("get_wallet_airdrops - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.get_wallet_airdrops(wallet_id) do
        {:ok, erl_airdrops} ->
          airdrops = parse_airdrop_list(erl_airdrops)
          {:ok, airdrops}

        {:error, reason} ->
          Logger.error("get_wallet_airdrops failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_wallet_airdrops: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def create_airdrop_recipient(airdrop_id, recipient_data) do
    Logger.info("create_airdrop_recipient - airdrop_id: #{airdrop_id}")

    try do
      case SolanaWalletClient.create_airdrop_recipient(airdrop_id, recipient_data) do
        {:ok, recipient_id} ->
          {:ok, to_string(recipient_id)}

        {:error, :airdrop_not_found} ->
          {:error, :airdrop_not_found}

        {:error, reason} ->
          Logger.error("create_airdrop_recipient failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in create_airdrop_recipient: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def get_airdrop_recipients(airdrop_id) do
    Logger.debug("get_airdrop_recipients - airdrop_id: #{airdrop_id}")

    try do
      case SolanaWalletClient.get_airdrop_recipients(airdrop_id) do
        {:ok, erl_recipients} ->
          recipients = parse_airdrop_recipient_list(erl_recipients)
          {:ok, recipients}

        {:error, reason} ->
          Logger.error("get_airdrop_recipients failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_airdrop_recipients: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def update_airdrop_recipient(recipient_id, updates) do
    Logger.info("update_airdrop_recipient - recipient_id: #{recipient_id}")

    try do
      case SolanaWalletClient.update_airdrop_recipient(recipient_id, updates) do
        :ok ->
          :ok

        {:error, :recipient_not_found} ->
          {:error, :recipient_not_found}

        {:error, reason} ->
          Logger.error("update_airdrop_recipient failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in update_airdrop_recipient: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def create_stake_account(wallet_id, stake_data) do
    Logger.info("create_stake_account - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.create_stake_account(wallet_id, stake_data) do
        {:ok, stake_id} ->
          Logger.info("create_stake_account succeeded - stake_id: #{stake_id}")
          {:ok, to_string(stake_id)}

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("create_stake_account failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in create_stake_account: #{inspect(error)}")
        {:error, :stake_creation_failed}
    end
  end

  def get_stake_account(stake_id) do
    Logger.debug("get_stake_account - stake_id: #{stake_id}")

    try do
      case SolanaWalletClient.get_stake_account(stake_id) do
        {:ok, erl_stake} ->
          case SolanaStakeAccount.erl_changeset_safe(erl_stake) do
            {:ok, changeset} ->
              SolanaStakeAccount.build(changeset)

            {:error, reason} ->
              Logger.error("get_stake_account invalid data: #{reason}")
              {:error, :invalid_data}
          end

        {:error, :stake_account_not_found} ->
          {:error, :stake_account_not_found}

        {:error, reason} ->
          Logger.error("get_stake_account failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_stake_account: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_stake_by_address(stake_account_address) do
    Logger.debug("get_stake_by_address - address: #{stake_account_address}")

    try do
      case SolanaWalletClient.get_stake_by_address(stake_account_address) do
        {:ok, erl_stake} ->
          case SolanaStakeAccount.erl_changeset_safe(erl_stake) do
            {:ok, changeset} ->
              SolanaStakeAccount.build(changeset)

            {:error, reason} ->
              Logger.error("get_stake_by_address invalid data: #{reason}")
              {:error, :invalid_data}
          end

        {:error, :stake_account_not_found} ->
          {:error, :stake_account_not_found}

        {:error, reason} ->
          Logger.error("get_stake_by_address failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_stake_by_address: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def update_stake_account(stake_id, updates) do
    Logger.info("update_stake_account - stake_id: #{stake_id}")

    try do
      case SolanaWalletClient.update_stake_account(stake_id, updates) do
        :ok ->
          :ok

        {:error, :stake_account_not_found} ->
          {:error, :stake_account_not_found}

        {:error, reason} ->
          Logger.error("update_stake_account failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in update_stake_account: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def get_user_stakes(user_id) do
    Logger.debug("get_user_stakes - user_id: #{user_id}")

    try do
      case SolanaWalletClient.get_user_stakes(user_id) do
        {:ok, erl_stakes} ->
          stakes = parse_stake_list(erl_stakes)
          {:ok, stakes}

        {:error, reason} ->
          Logger.error("get_user_stakes failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_user_stakes: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_wallet_stakes(wallet_id) do
    Logger.debug("get_wallet_stakes - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.get_wallet_stakes(wallet_id) do
        {:ok, erl_stakes} ->
          stakes = parse_stake_list(erl_stakes)
          {:ok, stakes}

        {:error, reason} ->
          Logger.error("get_wallet_stakes failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_wallet_stakes: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def create_token_account(wallet_id, token_data) do
    Logger.info("create_token_account - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.create_token_account(wallet_id, token_data) do
        {:ok, token_account_id} ->
          Logger.info("create_token_account succeeded - id: #{token_account_id}")
          {:ok, to_string(token_account_id)}

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("create_token_account failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in create_token_account: #{inspect(error)}")
        {:error, :token_account_creation_failed}
    end
  end

  def get_token_account(token_account_id) do
    Logger.debug("get_token_account - id: #{token_account_id}")

    try do
      case SolanaWalletClient.get_token_account(token_account_id) do
        {:ok, erl_token_account} ->
          case SolanaTokenAccount.erl_changeset_safe(erl_token_account) do
            {:ok, changeset} ->
              SolanaTokenAccount.build(changeset)

            {:error, reason} ->
              Logger.error("get_token_account invalid data: #{reason}")
              {:error, :invalid_data}
          end

        {:error, :token_account_not_found} ->
          {:error, :token_account_not_found}

        {:error, reason} ->
          Logger.error("get_token_account failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_token_account: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_wallet_token_accounts(wallet_id) do
    Logger.debug("get_wallet_token_accounts - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.get_wallet_token_accounts(wallet_id) do
        {:ok, erl_token_accounts} ->
          token_accounts = parse_token_account_list(erl_token_accounts)
          {:ok, token_accounts}

        {:error, reason} ->
          Logger.error("get_wallet_token_accounts failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_wallet_token_accounts: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def update_token_account_balance(token_account_id, new_balance, last_synced) do
    Logger.info("update_token_account_balance - id: #{token_account_id}")

    try do
      case SolanaWalletClient.update_token_account_balance(
             token_account_id,
             new_balance,
             last_synced
           ) do
        :ok ->
          :ok

        {:error, :token_account_not_found} ->
          {:error, :token_account_not_found}

        {:error, reason} ->
          Logger.error("update_token_account_balance failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in update_token_account_balance: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  def create_nft(wallet_id, nft_data) do
    Logger.info("create_nft - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.create_nft(wallet_id, nft_data) do
        {:ok, nft_id} ->
          Logger.info("create_nft succeeded - nft_id: #{nft_id}")
          {:ok, to_string(nft_id)}

        {:error, :wallet_not_found} ->
          {:error, :wallet_not_found}

        {:error, reason} ->
          Logger.error("create_nft failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in create_nft: #{inspect(error)}")
        {:error, :nft_creation_failed}
    end
  end

  def get_nft(nft_id) do
    Logger.debug("get_nft - nft_id: #{nft_id}")

    try do
      case SolanaWalletClient.get_nft(nft_id) do
        {:ok, erl_nft} ->
          case SolanaNFT.erl_changeset_safe(erl_nft) do
            {:ok, changeset} ->
              SolanaNFT.build(changeset)

            {:error, reason} ->
              Logger.error("get_nft invalid data: #{reason}")
              {:error, :invalid_data}
          end

        {:error, :nft_not_found} ->
          {:error, :nft_not_found}

        {:error, reason} ->
          Logger.error("get_nft failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_nft: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def get_wallet_nfts(wallet_id) do
    Logger.debug("get_wallet_nfts - wallet_id: #{wallet_id}")

    try do
      case SolanaWalletClient.get_wallet_nfts(wallet_id) do
        {:ok, erl_nfts} ->
          nfts = parse_nft_list(erl_nfts)
          {:ok, nfts}

        {:error, reason} ->
          Logger.error("get_wallet_nfts failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in get_wallet_nfts: #{inspect(error)}")
        {:error, :lookup_failed}
    end
  end

  def update_nft(nft_id, updates) do
    Logger.info("update_nft - nft_id: #{nft_id}")

    try do
      case SolanaWalletClient.update_nft(nft_id, updates) do
        :ok ->
          :ok

        {:error, :nft_not_found} ->
          {:error, :nft_not_found}

        {:error, reason} ->
          Logger.error("update_nft failed: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Exception in update_nft: #{inspect(error)}")
        {:error, :operation_failed}
    end
  end

  defp parse_transaction_list(erl_txs) do
    erl_txs
    |> Enum.map(fn erl_tx ->
      case SolanaTransaction.erl_changeset_safe(erl_tx) do
        {:ok, changeset} ->
          case SolanaTransaction.build(changeset) do
            {:ok, tx} -> tx
            {:error, _} -> nil
          end

        {:error, _} ->
          nil
      end
    end)
    |> Enum.filter(&(&1 != nil))
  end

  defp parse_airdrop_list(erl_airdrops) do
    erl_airdrops
    |> Enum.map(fn erl_airdrop ->
      case SolanaAirdrop.erl_changeset_safe(erl_airdrop) do
        {:ok, changeset} ->
          case SolanaAirdrop.build(changeset) do
            {:ok, airdrop} -> airdrop
            {:error, _} -> nil
          end

        {:error, _} ->
          nil
      end
    end)
    |> Enum.filter(&(&1 != nil))
  end

  defp parse_airdrop_recipient_list(erl_recipients) do
    erl_recipients
    |> Enum.map(fn erl_recipient ->
      case SolanaAirdropRecipient.erl_changeset_safe(erl_recipient) do
        {:ok, changeset} ->
          case SolanaAirdropRecipient.build(changeset) do
            {:ok, recipient} -> recipient
            {:error, _} -> nil
          end

        {:error, _} ->
          nil
      end
    end)
    |> Enum.filter(&(&1 != nil))
  end

  defp parse_stake_list(erl_stakes) do
    erl_stakes
    |> Enum.map(fn erl_stake ->
      case SolanaStakeAccount.erl_changeset_safe(erl_stake) do
        {:ok, changeset} ->
          case SolanaStakeAccount.build(changeset) do
            {:ok, stake} -> stake
            {:error, _} -> nil
          end

        {:error, _} ->
          nil
      end
    end)
    |> Enum.filter(&(&1 != nil))
  end

  defp parse_token_account_list(erl_token_accounts) do
    erl_token_accounts
    |> Enum.map(fn erl_token_account ->
      case SolanaTokenAccount.erl_changeset_safe(erl_token_account) do
        {:ok, changeset} ->
          case SolanaTokenAccount.build(changeset) do
            {:ok, token_account} -> token_account
            {:error, _} -> nil
          end

        {:error, _} ->
          nil
      end
    end)
    |> Enum.filter(&(&1 != nil))
  end

  defp parse_nft_list(erl_nfts) do
    erl_nfts
    |> Enum.map(fn erl_nft ->
      case SolanaNFT.erl_changeset_safe(erl_nft) do
        {:ok, changeset} ->
          case SolanaNFT.build(changeset) do
            {:ok, nft} -> nft
            {:error, _} -> nil
          end

        {:error, _} ->
          nil
      end
    end)
    |> Enum.filter(&(&1 != nil))
  end
end
