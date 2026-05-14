defmodule Context.SuiWallet do
  require Logger
  alias Schema.SuiWallet
  alias Schema.SuiTransaction
  alias Schema.SuiStake
  alias Schema.SuiObject
  alias Schema.SuiCoinBalance

  @walletdb :sui_walletdb

  def create_wallet(user_id, sui_address, encrypted_data) do
    %{
      encrypted_private_key: enc_key,
      iv: iv,
      tag: tag
    } = encrypted_data

    scheme = Map.get(encrypted_data, :scheme, :ed25519)
    opts   = Map.take(encrypted_data, [:label, :network, :public_key])

    result =
      :erlang.apply(@walletdb, :create_wallet, [
        user_id,
        sui_address,
        enc_key,
        iv,
        tag,
        scheme,
        opts
      ])

    case result do
      {:ok, wallet_id} ->
        get_wallet(wallet_id)

      {:error, :address_already_exists} ->
        {:error, :address_already_exists}

      {:error, reason} ->
        Logger.error("Failed to create Sui wallet: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def create_wallet(user_id, sui_address, network, label) do
    result =
      :erlang.apply(@walletdb, :create_wallet, [
        user_id,
        sui_address,
        <<"">>,
        <<"">>,
        <<"">>,
        :ed25519,
        %{network: network, label: label}
      ])

    case result do
      {:ok, wallet_id} ->
        get_wallet(wallet_id)

      {:error, :address_already_exists} ->
        {:error, :address_already_exists}

      {:error, reason} ->
        Logger.error("Failed to create Sui wallet: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet, [wallet_id])

    case result do
      {:ok, record} ->
        changeset = SuiWallet.erl_changeset(record)
        SuiWallet.build(changeset)

      {:error, :wallet_not_found} ->
        {:error, :wallet_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Sui wallet #{wallet_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet_by_address(sui_address) do
    result = :erlang.apply(@walletdb, :get_wallet_by_address, [sui_address])

    case result do
      {:ok, record} ->
        changeset = SuiWallet.erl_changeset(record)
        SuiWallet.build(changeset)

      {:error, :wallet_not_found} ->
        {:error, :wallet_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Sui wallet by address #{sui_address}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_user_wallets(user_id) do
    records = :erlang.apply(@walletdb, :get_user_wallets, [user_id])

    wallets =
      records
      |> Enum.map(fn record ->
        case SuiWallet.erl_changeset_safe(record) do
          {:ok, changeset} ->
            case SuiWallet.build(changeset) do
              {:ok, wallet} -> wallet
              _ -> nil
            end

          {:error, _} ->
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, wallets}
  end

  def get_user_wallets_by_network(user_id, network) do
    records = :erlang.apply(@walletdb, :get_user_wallets_by_network, [user_id, network])

    wallets =
      records
      |> Enum.map(fn record ->
        case SuiWallet.erl_changeset_safe(record) do
          {:ok, changeset} ->
            case SuiWallet.build(changeset) do
              {:ok, wallet} -> wallet
              _ -> nil
            end

          {:error, _} ->
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, wallets}
  end

  def get_primary_wallet(user_id) do
    result = :erlang.apply(@walletdb, :get_primary_wallet, [user_id])

    case result do
      {:ok, record} ->
        changeset = SuiWallet.erl_changeset(record)
        SuiWallet.build(changeset)

      {:error, :no_primary_wallet} ->
        {:error, :no_primary_wallet}

      {:error, reason} ->
        Logger.error("Failed to get Sui primary wallet for #{user_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def set_primary_wallet(user_id, wallet_id) do
    result = :erlang.apply(@walletdb, :set_primary_wallet, [user_id, wallet_id])

    case result do
      {:ok, ^wallet_id} ->
        {:ok, wallet_id}

      {:error, reason} ->
        Logger.error("Failed to set Sui primary wallet: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def update_wallet_label(wallet_id, label) do
    result = :erlang.apply(@walletdb, :update_wallet_label, [wallet_id, label])

    case result do
      {:ok, ^wallet_id} ->
        {:ok, wallet_id}

      {:error, reason} ->
        Logger.error("Failed to update Sui wallet label: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def update_wallet_last_used(wallet_id) do
    result = :erlang.apply(@walletdb, :update_wallet_last_used, [wallet_id])

    case result do
      {:ok, ^wallet_id} ->
        {:ok, wallet_id}

      {:error, reason} ->
        Logger.error("Failed to update Sui wallet last_used: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def delete_wallet(wallet_id) do
    result = :erlang.apply(@walletdb, :delete_wallet, [wallet_id])

    case result do
      {:ok, :deleted} ->
        {:ok, :deleted}

      {:error, reason} ->
        Logger.error("Failed to delete Sui wallet #{wallet_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def wallet_exists(wallet_id) do
    :erlang.apply(@walletdb, :wallet_exists, [wallet_id])
  end

  def address_exists(sui_address) do
    :erlang.apply(@walletdb, :address_exists, [sui_address])
  end

  def create_transaction(wallet_id, tx_data) do
    result = :erlang.apply(@walletdb, :create_transaction, [wallet_id, tx_data])

    case result do
      {:ok, tx_id} ->
        get_transaction(tx_id)

      {:error, reason} ->
        Logger.error("Failed to create Sui transaction: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_transaction(tx_id) do
    result = :erlang.apply(@walletdb, :get_transaction, [tx_id])

    case result do
      {:ok, record} ->
        changeset = SuiTransaction.erl_changeset(record)
        SuiTransaction.build(changeset)

      {:error, :transaction_not_found} ->
        {:error, :transaction_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Sui transaction #{tx_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_transaction_by_digest(digest) do
    result = :erlang.apply(@walletdb, :get_transaction_by_digest, [digest])

    case result do
      {:ok, record} ->
        changeset = SuiTransaction.erl_changeset(record)
        SuiTransaction.build(changeset)

      {:error, :transaction_not_found} ->
        {:error, :transaction_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Sui transaction by digest: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet_transactions(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_transactions, [wallet_id])

    case result do
      {:ok, records} ->
        parse_transaction_list(records)

      {:error, reason} ->
        Logger.error("Failed to get Sui wallet transactions: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet_transactions(wallet_id, limit, offset) do
    result = :erlang.apply(@walletdb, :get_wallet_transactions, [wallet_id, limit, offset])

    case result do
      {:ok, records, total} ->
        case parse_transaction_list(records) do
          {:ok, txs} -> {:ok, txs, total}
          error -> error
        end

      {:error, reason} ->
        Logger.error("Failed to get Sui wallet transactions (paged): #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_user_transactions(user_id) do
    result = :erlang.apply(@walletdb, :get_user_transactions, [user_id])

    case result do
      {:ok, records} ->
        parse_transaction_list(records)

      {:error, reason} ->
        Logger.error("Failed to get Sui user transactions: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_transactions_by_type(wallet_id, tx_type) do
    result = :erlang.apply(@walletdb, :get_transactions_by_type, [wallet_id, tx_type])

    case result do
      {:ok, records} ->
        parse_transaction_list(records)

      {:error, reason} ->
        Logger.error("Failed to get Sui transactions by type: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def update_transaction_status(digest, status, effects_status, checkpoint) do
    result =
      :erlang.apply(@walletdb, :update_transaction_status, [
        digest,
        status,
        effects_status,
        checkpoint
      ])

    case result do
      :ok -> :ok
      {:error, reason} ->
        Logger.error("Failed to update Sui transaction status: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def create_stake(wallet_id, stake_data) do
    result = :erlang.apply(@walletdb, :create_stake, [wallet_id, stake_data])

    case result do
      {:ok, stake_id} ->
        get_stake(stake_id)

      {:error, reason} ->
        Logger.error("Failed to create Sui stake: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_stake(stake_id) do
    result = :erlang.apply(@walletdb, :get_stake, [stake_id])

    case result do
      {:ok, record} ->
        changeset = SuiStake.erl_changeset(record)
        SuiStake.build(changeset)

      {:error, :stake_not_found} ->
        {:error, :stake_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Sui stake #{stake_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_stake_by_staked_sui_id(staked_sui_id) do
    result = :erlang.apply(@walletdb, :get_stake_by_staked_sui_id, [staked_sui_id])

    case result do
      {:ok, record} ->
        changeset = SuiStake.erl_changeset(record)
        SuiStake.build(changeset)

      {:error, :stake_not_found} ->
        {:error, :stake_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Sui stake by staked_sui_id: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet_stakes(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_stakes, [wallet_id])

    case result do
      {:ok, records} -> parse_stake_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def get_user_stakes(user_id) do
    result = :erlang.apply(@walletdb, :get_user_stakes, [user_id])

    case result do
      {:ok, records} -> parse_stake_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def get_stakes_by_validator(wallet_id, validator_address) do
    result =
      :erlang.apply(@walletdb, :get_stakes_by_validator, [wallet_id, validator_address])

    case result do
      {:ok, records} -> parse_stake_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def update_stake(stake_id, updates) do
    result = :erlang.apply(@walletdb, :update_stake, [stake_id, updates])

    case result do
      {:ok, ^stake_id} ->
        {:ok, stake_id}

      {:error, reason} ->
        Logger.error("Failed to update Sui stake: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def delete_stake(stake_id) do
    result = :erlang.apply(@walletdb, :delete_stake, [stake_id])

    case result do
      {:ok, :deleted} ->
        {:ok, :deleted}

      {:error, reason} ->
        Logger.error("Failed to delete Sui stake #{stake_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def upsert_object(wallet_id, object_data) do
    result = :erlang.apply(@walletdb, :upsert_object, [wallet_id, object_data])

    case result do
      {:ok, object_id} ->
        get_object(object_id)

      {:error, reason} ->
        Logger.error("Failed to upsert Sui object: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_object(object_id) do
    result = :erlang.apply(@walletdb, :get_object, [object_id])

    case result do
      {:ok, record} ->
        changeset = SuiObject.erl_changeset(record)
        SuiObject.build(changeset)

      {:error, :object_not_found} ->
        {:error, :object_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Sui object #{object_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet_objects(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_objects, [wallet_id])

    case result do
      {:ok, records} -> parse_object_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def get_wallet_nfts(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_nfts, [wallet_id])

    case result do
      {:ok, records} -> parse_object_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def get_user_objects(user_id) do
    result = :erlang.apply(@walletdb, :get_user_objects, [user_id])

    case result do
      {:ok, records} -> parse_object_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def delete_object(object_id) do
    result = :erlang.apply(@walletdb, :delete_object, [object_id])

    case result do
      {:ok, :deleted} ->
        {:ok, :deleted}

      {:error, reason} ->
        Logger.error("Failed to delete Sui object #{object_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def upsert_coin_balance(wallet_id, balance_data) do
    result = :erlang.apply(@walletdb, :upsert_coin_balance, [wallet_id, balance_data])

    case result do
      {:ok, balance_id} ->
        {:ok, balance_id}

      {:error, reason} ->
        Logger.error("Failed to upsert Sui coin balance: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_coin_balance(wallet_id, coin_type) do
    result = :erlang.apply(@walletdb, :get_coin_balance, [wallet_id, coin_type])

    case result do
      {:ok, record} ->
        changeset = SuiCoinBalance.erl_changeset(record)
        SuiCoinBalance.build(changeset)

      {:error, :balance_not_found} ->
        {:error, :balance_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Sui coin balance: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet_coin_balances(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_coin_balances, [wallet_id])

    case result do
      {:ok, records} -> parse_coin_balance_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def get_user_coin_balances(user_id) do
    result = :erlang.apply(@walletdb, :get_user_coin_balances, [user_id])

    case result do
      {:ok, records} -> parse_coin_balance_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def delete_coin_balance(balance_id) do
    result = :erlang.apply(@walletdb, :delete_coin_balance, [balance_id])

    case result do
      {:ok, :deleted} ->
        {:ok, :deleted}

      {:error, reason} ->
        Logger.error("Failed to delete Sui coin balance #{balance_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp parse_transaction_list(records) do
    txs =
      records
      |> Enum.map(fn record ->
        case SuiTransaction.erl_changeset_safe(record) do
          {:ok, changeset} ->
            case SuiTransaction.build(changeset) do
              {:ok, tx} -> tx
              _ -> nil
            end

          {:error, _} ->
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, txs}
  end

  defp parse_stake_list(records) do
    stakes =
      records
      |> Enum.map(fn record ->
        case SuiStake.erl_changeset_safe(record) do
          {:ok, changeset} ->
            case SuiStake.build(changeset) do
              {:ok, stake} -> stake
              _ -> nil
            end

          {:error, _} ->
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, stakes}
  end

  defp parse_object_list(records) do
    objects =
      records
      |> Enum.map(fn record ->
        case SuiObject.erl_changeset_safe(record) do
          {:ok, changeset} ->
            case SuiObject.build(changeset) do
              {:ok, obj} -> obj
              _ -> nil
            end

          {:error, _} ->
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, objects}
  end

  defp parse_coin_balance_list(records) do
    balances =
      records
      |> Enum.map(fn record ->
        case SuiCoinBalance.erl_changeset_safe(record) do
          {:ok, changeset} ->
            case SuiCoinBalance.build(changeset) do
              {:ok, bal} -> bal
              _ -> nil
            end

          {:error, _} ->
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, balances}
  end
end
