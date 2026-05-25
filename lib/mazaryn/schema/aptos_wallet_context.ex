defmodule Context.AptosWallet do
  require Logger
  alias Schema.AptosWallet
  alias Schema.AptosTransaction
  alias Schema.AptosNftCollection
  alias Schema.AptosNftToken
  alias Schema.AptosStake

  @walletdb :aptos_walletdb

  def create_wallet(user_id, address, encrypted_data) do
    %{
      encrypted_private_key: enc_key,
      iv: iv,
      tag: tag
    } = encrypted_data

    result =
      :erlang.apply(@walletdb, :create_wallet, [
        user_id,
        address,
        enc_key,
        iv,
        tag
      ])

    case result do
      {:ok, wallet_id} ->
        case get_wallet(wallet_id) do
          {:ok, wallet} -> {:ok, wallet}
          error -> error
        end

      {:error, reason} ->
        Logger.error("Failed to create Aptos wallet: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def create_wallet(user_id, address, network, label) do
    result =
      :erlang.apply(@walletdb, :create_wallet, [
        user_id,
        address,
        <<"">>,
        <<"">>,
        <<"">>,
        %{network: network, label: label}
      ])

    case result do
      {:ok, wallet_id} ->
        case get_wallet(wallet_id) do
          {:ok, wallet} -> {:ok, wallet}
          error -> error
        end

      {:error, reason} ->
        Logger.error("Failed to create Aptos wallet: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet, [wallet_id])

    case result do
      {:ok, record} ->
        changeset = AptosWallet.erl_changeset(record)
        AptosWallet.build(changeset)

      {:error, :wallet_not_found} ->
        {:error, :wallet_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Aptos wallet #{wallet_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet_by_address(address) do
    result = :erlang.apply(@walletdb, :get_wallet_by_address, [address])

    case result do
      {:ok, record} ->
        changeset = AptosWallet.erl_changeset(record)
        AptosWallet.build(changeset)

      {:error, :wallet_not_found} ->
        {:error, :wallet_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Aptos wallet by address #{address}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_user_wallets(user_id) do
    # Change this:
    records = :erlang.apply(@walletdb, :get_user_wallets, [user_id])

    # To this:
    records =
      case :erlang.apply(@walletdb, :get_user_wallets, [user_id]) do
        {:ok, list} when is_list(list) -> list
        list when is_list(list) -> list
        _ -> []
      end

    wallets =
      records
      |> Enum.map(fn record ->
        case AptosWallet.erl_changeset_safe(record) do
          {:ok, changeset} ->
            case AptosWallet.build(changeset) do
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
        changeset = AptosWallet.erl_changeset(record)
        AptosWallet.build(changeset)

      {:error, :no_primary_wallet} ->
        {:error, :no_primary_wallet}

      {:error, reason} ->
        Logger.error("Failed to get Aptos primary wallet for #{user_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def set_primary_wallet(user_id, wallet_id) do
    result = :erlang.apply(@walletdb, :set_primary_wallet, [user_id, wallet_id])

    case result do
      {:ok, ^wallet_id} -> {:ok, wallet_id}
      {:error, reason} ->
        Logger.error("Failed to set Aptos primary wallet: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def update_wallet_label(wallet_id, new_label) do
    result = :erlang.apply(@walletdb, :update_wallet_label, [wallet_id, new_label])

    case result do
      {:ok, ^wallet_id} -> {:ok, wallet_id}
      {:error, reason} ->
        Logger.error("Failed to update Aptos wallet label: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def update_wallet_last_used(wallet_id) do
    result = :erlang.apply(@walletdb, :update_wallet_last_used, [wallet_id])

    case result do
      {:ok, ^wallet_id} -> {:ok, wallet_id}
      {:error, reason} ->
        Logger.error("Failed to update Aptos wallet last_used: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def delete_wallet(wallet_id) do
    result = :erlang.apply(@walletdb, :delete_wallet, [wallet_id])

    case result do
      {:ok, :deleted} -> {:ok, :deleted}
      {:error, reason} ->
        Logger.error("Failed to delete Aptos wallet #{wallet_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def wallet_exists(wallet_id) do
    :erlang.apply(@walletdb, :wallet_exists, [wallet_id])
  end

  def address_exists(address) do
    :erlang.apply(@walletdb, :address_exists, [address])
  end

  def record_transaction(wallet_id, tx_data) do
    result = :erlang.apply(@walletdb, :create_transaction, [wallet_id, tx_data])

    case result do
      {:ok, tx_id} ->
        case get_transaction(tx_id) do
          {:ok, tx} -> {:ok, tx}
          error -> error
        end

      {:error, reason} ->
        Logger.error("Failed to record Aptos transaction: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_transaction(tx_id) do
    result = :erlang.apply(@walletdb, :get_transaction, [tx_id])

    case result do
      {:ok, record} ->
        changeset = AptosTransaction.erl_changeset(record)
        AptosTransaction.build(changeset)

      {:error, :transaction_not_found} ->
        {:error, :transaction_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Aptos transaction #{tx_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_transaction_by_hash(hash) do
    result = :erlang.apply(@walletdb, :get_transaction_by_hash, [hash])

    case result do
      {:ok, record} ->
        changeset = AptosTransaction.erl_changeset(record)
        AptosTransaction.build(changeset)

      {:error, :transaction_not_found} ->
        {:error, :transaction_not_found}

      {:error, reason} ->
        Logger.error("Failed to get Aptos transaction by hash: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet_transactions(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_transactions, [wallet_id])

    case result do
      {:ok, records} -> parse_transaction_list(records)
      {:error, reason} ->
        Logger.error("Failed to get Aptos wallet transactions: #{inspect(reason)}")
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
        Logger.error("Failed to get Aptos wallet transactions (paged): #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_user_transactions(user_id) do
    result = :erlang.apply(@walletdb, :get_user_transactions, [user_id])

    case result do
      {:ok, records} -> parse_transaction_list(records)
      {:error, reason} ->
        Logger.error("Failed to get Aptos user transactions: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_transactions_by_type(wallet_id, tx_type) do
    result = :erlang.apply(@walletdb, :get_transactions_by_type, [wallet_id, tx_type])

    case result do
      {:ok, records} -> parse_transaction_list(records)
      {:error, reason} ->
        Logger.error("Failed to get Aptos transactions by type: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def record_nft_collection(wallet_id, collection_data) do
    result = :erlang.apply(@walletdb, :create_nft_collection, [wallet_id, collection_data])

    case result do
      {:ok, collection_id} ->
        case get_nft_collection(collection_id) do
          {:ok, coll} -> {:ok, coll}
          error -> error
        end

      {:error, reason} ->
        Logger.error("Failed to record Aptos NFT collection: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_nft_collection(collection_id) do
    result = :erlang.apply(@walletdb, :get_nft_collection, [collection_id])

    case result do
      {:ok, record} ->
        changeset = AptosNftCollection.erl_changeset(record)
        AptosNftCollection.build(changeset)

      {:error, reason} ->
        {:error, reason}
    end
  end

  def get_wallet_nft_collections(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_nft_collections, [wallet_id])

    case result do
      {:ok, records} -> parse_nft_collection_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def get_user_nft_collections(user_id) do
    result = :erlang.apply(@walletdb, :get_user_nft_collections, [user_id])

    case result do
      {:ok, records} -> parse_nft_collection_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def record_nft_token(wallet_id, token_data) do
    result = :erlang.apply(@walletdb, :create_nft_token, [wallet_id, token_data])

    case result do
      {:ok, token_id} ->
        case get_nft_token(token_id) do
          {:ok, token} -> {:ok, token}
          error -> error
        end

      {:error, reason} ->
        Logger.error("Failed to record Aptos NFT token: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_nft_token(token_id) do
    result = :erlang.apply(@walletdb, :get_nft_token, [token_id])

    case result do
      {:ok, record} ->
        changeset = AptosNftToken.erl_changeset(record)
        AptosNftToken.build(changeset)

      {:error, reason} ->
        {:error, reason}
    end
  end

  def get_wallet_nft_tokens(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_nft_tokens, [wallet_id])

    case result do
      {:ok, records} -> parse_nft_token_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def get_collection_nft_tokens(collection_id) do
    result = :erlang.apply(@walletdb, :get_collection_nft_tokens, [collection_id])

    case result do
      {:ok, records} -> parse_nft_token_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def record_fungible_asset(wallet_id, fa_data) do
    result = :erlang.apply(@walletdb, :create_fungible_asset, [wallet_id, fa_data])

    case result do
      {:ok, fa_id} -> {:ok, fa_id}
      {:error, reason} ->
        Logger.error("Failed to record Aptos fungible asset: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_wallet_fungible_assets(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_fungible_assets, [wallet_id])

    case result do
      {:ok, records} ->
        {:ok, Enum.map(records, &erlang_fa_to_map/1)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def get_user_fungible_assets(user_id) do
    result = :erlang.apply(@walletdb, :get_user_fungible_assets, [user_id])

    case result do
      {:ok, records} ->
        {:ok, Enum.map(records, &erlang_fa_to_map/1)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def create_stake(wallet_id, stake_data) do
    result = :erlang.apply(@walletdb, :create_stake, [wallet_id, stake_data])

    case result do
      {:ok, stake_id} ->
        case get_stake(stake_id) do
          {:ok, stake} -> {:ok, stake}
          error -> error
        end

      {:error, reason} ->
        Logger.error("Failed to create Aptos stake: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def get_stake(stake_id) do
    result = :erlang.apply(@walletdb, :get_stake, [stake_id])

    case result do
      {:ok, record} ->
        changeset = AptosStake.erl_changeset(record)
        AptosStake.build(changeset)

      {:error, reason} ->
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

  def get_stakes_by_pool(wallet_id, pool_address) do
    result = :erlang.apply(@walletdb, :get_stakes_by_pool, [wallet_id, pool_address])

    case result do
      {:ok, records} -> parse_stake_list(records)
      {:error, reason} -> {:error, reason}
    end
  end

  def update_stake(stake_id, updates) do
    result = :erlang.apply(@walletdb, :update_stake, [stake_id, updates])

    case result do
      {:ok, ^stake_id} -> {:ok, stake_id}
      {:error, reason} ->
        Logger.error("Failed to update Aptos stake: #{inspect(reason)}")
        {:error, reason}
    end
  end

  def delete_stake(stake_id) do
    result = :erlang.apply(@walletdb, :delete_stake, [stake_id])

    case result do
      {:ok, :deleted} -> {:ok, :deleted}
      {:error, reason} -> {:error, reason}
    end
  end

  def upsert_coin_balance(wallet_id, balance_data) do
    result = :erlang.apply(@walletdb, :upsert_coin_balance, [wallet_id, balance_data])

    case result do
      {:ok, balance_id} -> {:ok, balance_id}
      {:error, reason} -> {:error, reason}
    end
  end

  def get_coin_balance(wallet_id, coin_type) do
    result = :erlang.apply(@walletdb, :get_coin_balance, [wallet_id, coin_type])

    case result do
      {:ok, record} -> {:ok, erlang_coin_balance_to_map(record)}
      {:error, reason} -> {:error, reason}
    end
  end

  def get_wallet_coin_balances(wallet_id) do
    result = :erlang.apply(@walletdb, :get_wallet_coin_balances, [wallet_id])

    case result do
      {:ok, records} -> {:ok, Enum.map(records, &erlang_coin_balance_to_map/1)}
      {:error, reason} -> {:error, reason}
    end
  end

  def get_user_coin_balances(user_id) do
    result = :erlang.apply(@walletdb, :get_user_coin_balances, [user_id])

    case result do
      {:ok, records} -> {:ok, Enum.map(records, &erlang_coin_balance_to_map/1)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_transaction_list(records) do
    txs =
      records
      |> Enum.map(fn record ->
        case AptosTransaction.erl_changeset_safe(record) do
          {:ok, cs} ->
            case AptosTransaction.build(cs) do
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

  defp parse_nft_collection_list(records) do
    colls =
      records
      |> Enum.map(fn record ->
        case AptosNftCollection.erl_changeset_safe(record) do
          {:ok, cs} ->
            case AptosNftCollection.build(cs) do
              {:ok, c} -> c
              _ -> nil
            end

          {:error, _} ->
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, colls}
  end

  defp parse_nft_token_list(records) do
    tokens =
      records
      |> Enum.map(fn record ->
        case AptosNftToken.erl_changeset_safe(record) do
          {:ok, cs} ->
            case AptosNftToken.build(cs) do
              {:ok, t} -> t
              _ -> nil
            end

          {:error, _} ->
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, tokens}
  end

  defp parse_stake_list(records) do
    stakes =
      records
      |> Enum.map(fn record ->
        case AptosStake.erl_changeset_safe(record) do
          {:ok, cs} ->
            case AptosStake.build(cs) do
              {:ok, s} -> s
              _ -> nil
            end

          {:error, _} ->
            nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    {:ok, stakes}
  end

  defp erlang_fa_to_map(
         {:aptos_fungible_asset, fa_id, wallet_id, user_id, creator_address, asset_name,
          symbol, decimals, metadata_address, transaction_hash, created_at, metadata}
       ) do
    %{
      fa_id: fa_id,
      wallet_id: wallet_id,
      user_id: user_id,
      creator_address: to_string_or_nil(creator_address),
      asset_name: to_string_or_nil(asset_name),
      symbol: to_string_or_nil(symbol),
      decimals: if(is_integer(decimals), do: decimals, else: nil),
      metadata_address: to_string_or_nil(metadata_address),
      transaction_hash: to_string_or_nil(transaction_hash),
      created_at: if(is_integer(created_at), do: created_at, else: nil),
      metadata: if(is_map(metadata), do: metadata, else: %{})
    }
  end

  defp erlang_fa_to_map(_), do: %{}

  defp erlang_coin_balance_to_map(
         {:aptos_coin_balance, balance_id, wallet_id, user_id, account_address,
          coin_type, balance, network, last_synced, metadata}
       ) do
    %{
      balance_id: balance_id,
      wallet_id: wallet_id,
      user_id: user_id,
      account_address: to_string_or_nil(account_address),
      coin_type: to_string_or_nil(coin_type),
      balance: if(is_integer(balance), do: balance, else: 0),
      network: to_string_or_nil(network),
      last_synced: if(is_integer(last_synced), do: last_synced, else: nil),
      metadata: if(is_map(metadata), do: metadata, else: %{})
    }
  end

  defp erlang_coin_balance_to_map(_), do: %{}

  defp to_string_or_nil(:undefined), do: nil
  defp to_string_or_nil(nil), do: nil
  defp to_string_or_nil(v), do: to_string(v)
end
