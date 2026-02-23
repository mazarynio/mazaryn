defmodule Context.NearWallet do
  alias Core.NearWalletClient
  alias Schema.NearWallet
  alias Schema.NearTransaction
  alias Schema.NearAccessKey
  alias Schema.NearStake
  alias Schema.NearImplicitAccount
  alias Schema.NearSocialPost
  require Logger

  def create_wallet(user_id, account_id, encrypted_data) do
    case NearWalletClient.create_wallet(user_id, account_id, encrypted_data) do
      {:ok, wallet_id} ->
        {:ok, to_string(wallet_id)}

      {:error, :account_id_already_exists} ->
        {:error, "Account ID already exists"}

      {:error, reason} ->
        Logger.error("Failed to create NEAR wallet: #{inspect(reason)}")
        {:error, "Failed to create wallet: #{inspect(reason)}"}
    end
  end

  def create_wallet(user_id, account_id, network, label) do
    encrypted_data = %{
      encrypted_private_key: "",
      encryption_iv: "",
      encryption_tag: network
    }

    case NearWalletClient.create_wallet(user_id, account_id, encrypted_data, label) do
      {:ok, wallet_id} ->
        {:ok, to_string(wallet_id)}

      {:error, :account_id_already_exists} ->
        {:error, "Account ID already exists"}

      {:error, reason} ->
        Logger.error("Failed to create labeled NEAR wallet: #{inspect(reason)}")
        {:error, "Failed to create wallet: #{inspect(reason)}"}
    end
  end

  def get_wallet(wallet_id) do
    case NearWalletClient.get_wallet(wallet_id) do
      {:ok, record} ->
        changeset = NearWallet.erl_changeset(record)
        NearWallet.build(changeset)

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to get NEAR wallet: #{inspect(reason)}")
        {:error, "Failed to get wallet: #{inspect(reason)}"}
    end
  end

  def get_wallet_by_account_id(account_id) do
    case NearWalletClient.get_wallet_by_account_id(account_id) do
      {:ok, record} ->
        changeset = NearWallet.erl_changeset(record)
        NearWallet.build(changeset)

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to get NEAR wallet by account ID: #{inspect(reason)}")
        {:error, "Failed to get wallet: #{inspect(reason)}"}
    end
  end

  def get_user_wallets(user_id) do
    case NearWalletClient.get_user_wallets(user_id) do
      {:ok, records} ->
        wallets =
          records
          |> Enum.map(&NearWallet.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearWallet.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, wallet} -> wallet end)

        {:ok, wallets}

      {:error, reason} ->
        Logger.error("Failed to get user NEAR wallets: #{inspect(reason)}")
        {:error, "Failed to get wallets: #{inspect(reason)}"}
    end
  end

  def get_primary_wallet(user_id) do
    case NearWalletClient.get_primary_wallet(user_id) do
      {:ok, record} ->
        changeset = NearWallet.erl_changeset(record)
        NearWallet.build(changeset)

      {:error, :no_wallets_found} ->
        {:error, "No wallets found for user"}

      {:error, reason} ->
        Logger.error("Failed to get primary NEAR wallet: #{inspect(reason)}")
        {:error, "Failed to get primary wallet: #{inspect(reason)}"}
    end
  end

  def set_primary_wallet(user_id, wallet_id) do
    case NearWalletClient.set_primary_wallet(user_id, wallet_id) do
      :ok ->
        :ok

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, :access_denied} ->
        {:error, "Access denied"}

      {:error, reason} ->
        Logger.error("Failed to set primary NEAR wallet: #{inspect(reason)}")
        {:error, "Failed to set primary wallet: #{inspect(reason)}"}
    end
  end

  def update_wallet_last_used(wallet_id) do
    case NearWalletClient.update_wallet_last_used(wallet_id) do
      :ok ->
        :ok

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to update NEAR wallet last used: #{inspect(reason)}")
        {:error, "Failed to update wallet: #{inspect(reason)}"}
    end
  end

  def update_wallet_label(wallet_id, new_label) do
    case NearWalletClient.update_wallet_label(wallet_id, new_label) do
      :ok ->
        :ok

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to update NEAR wallet label: #{inspect(reason)}")
        {:error, "Failed to update wallet label: #{inspect(reason)}"}
    end
  end

  def delete_wallet(wallet_id) do
    case NearWalletClient.delete_wallet(wallet_id) do
      :ok ->
        :ok

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to delete NEAR wallet: #{inspect(reason)}")
        {:error, "Failed to delete wallet: #{inspect(reason)}"}
    end
  end

  def wallet_exists(wallet_id) do
    NearWalletClient.wallet_exists(wallet_id)
  end

  def account_id_exists(account_id) do
    NearWalletClient.account_id_exists(account_id)
  end

  def create_transaction(wallet_id, tx_data) do
    case NearWalletClient.create_transaction(wallet_id, tx_data) do
      {:ok, tx_id} ->
        {:ok, to_string(tx_id)}

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to create NEAR transaction: #{inspect(reason)}")
        {:error, "Failed to create transaction: #{inspect(reason)}"}
    end
  end

  def get_transaction(tx_id) do
    case NearWalletClient.get_transaction(tx_id) do
      {:ok, record} ->
        changeset = NearTransaction.erl_changeset(record)
        NearTransaction.build(changeset)

      {:error, :transaction_not_found} ->
        {:error, "Transaction not found"}

      {:error, reason} ->
        Logger.error("Failed to get NEAR transaction: #{inspect(reason)}")
        {:error, "Failed to get transaction: #{inspect(reason)}"}
    end
  end

  def get_wallet_transactions(wallet_id) do
    case NearWalletClient.get_wallet_transactions(wallet_id) do
      {:ok, records} ->
        transactions =
          records
          |> Enum.map(&NearTransaction.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearTransaction.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, tx} -> tx end)

        {:ok, transactions}

      {:error, reason} ->
        Logger.error("Failed to get NEAR wallet transactions: #{inspect(reason)}")
        {:error, "Failed to get transactions: #{inspect(reason)}"}
    end
  end

  def get_wallet_transactions(wallet_id, limit, offset) do
    case NearWalletClient.get_wallet_transactions(wallet_id, limit, offset) do
      {:ok, records, total} ->
        transactions =
          records
          |> Enum.map(&NearTransaction.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearTransaction.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, tx} -> tx end)

        {:ok, transactions, total}

      {:error, reason} ->
        Logger.error("Failed to get paginated NEAR wallet transactions: #{inspect(reason)}")
        {:error, "Failed to get transactions: #{inspect(reason)}"}
    end
  end

  def get_user_transactions(user_id) do
    case NearWalletClient.get_user_transactions(user_id) do
      {:ok, records} ->
        transactions =
          records
          |> Enum.map(&NearTransaction.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearTransaction.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, tx} -> tx end)

        {:ok, transactions}

      {:error, reason} ->
        Logger.error("Failed to get user NEAR transactions: #{inspect(reason)}")
        {:error, "Failed to get transactions: #{inspect(reason)}"}
    end
  end

  def get_transactions_by_type(wallet_id, tx_type) do
    case NearWalletClient.get_transactions_by_type(wallet_id, tx_type) do
      {:ok, records} ->
        transactions =
          records
          |> Enum.map(&NearTransaction.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearTransaction.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, tx} -> tx end)

        {:ok, transactions}

      {:error, reason} ->
        Logger.error("Failed to get NEAR transactions by type: #{inspect(reason)}")
        {:error, "Failed to get transactions: #{inspect(reason)}"}
    end
  end

  def create_access_key(wallet_id, key_data) do
    case NearWalletClient.create_access_key(wallet_id, key_data) do
      {:ok, key_id} ->
        {:ok, to_string(key_id)}

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to create NEAR access key: #{inspect(reason)}")
        {:error, "Failed to create access key: #{inspect(reason)}"}
    end
  end

  def get_access_key(key_id) do
    case NearWalletClient.get_access_key(key_id) do
      {:ok, record} ->
        changeset = NearAccessKey.erl_changeset(record)
        NearAccessKey.build(changeset)

      {:error, :key_not_found} ->
        {:error, "Access key not found"}

      {:error, reason} ->
        Logger.error("Failed to get NEAR access key: #{inspect(reason)}")
        {:error, "Failed to get access key: #{inspect(reason)}"}
    end
  end

  def get_wallet_access_keys(wallet_id) do
    case NearWalletClient.get_wallet_access_keys(wallet_id) do
      {:ok, records} ->
        keys =
          records
          |> Enum.map(&NearAccessKey.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearAccessKey.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, key} -> key end)

        {:ok, keys}

      {:error, reason} ->
        Logger.error("Failed to get NEAR wallet access keys: #{inspect(reason)}")
        {:error, "Failed to get access keys: #{inspect(reason)}"}
    end
  end

  def get_user_access_keys(user_id) do
    case NearWalletClient.get_user_access_keys(user_id) do
      {:ok, records} ->
        keys =
          records
          |> Enum.map(&NearAccessKey.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearAccessKey.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, key} -> key end)

        {:ok, keys}

      {:error, reason} ->
        Logger.error("Failed to get user NEAR access keys: #{inspect(reason)}")
        {:error, "Failed to get access keys: #{inspect(reason)}"}
    end
  end

  def delete_access_key(key_id) do
    case NearWalletClient.delete_access_key(key_id) do
      :ok ->
        :ok

      {:error, :key_not_found} ->
        {:error, "Access key not found"}

      {:error, reason} ->
        Logger.error("Failed to delete NEAR access key: #{inspect(reason)}")
        {:error, "Failed to delete access key: #{inspect(reason)}"}
    end
  end

  def create_stake(wallet_id, stake_data) do
    case NearWalletClient.create_stake(wallet_id, stake_data) do
      {:ok, stake_id} ->
        {:ok, to_string(stake_id)}

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to create NEAR stake: #{inspect(reason)}")
        {:error, "Failed to create stake: #{inspect(reason)}"}
    end
  end

  def get_stake(stake_id) do
    case NearWalletClient.get_stake(stake_id) do
      {:ok, record} ->
        changeset = NearStake.erl_changeset(record)
        NearStake.build(changeset)

      {:error, :stake_not_found} ->
        {:error, "Stake not found"}

      {:error, reason} ->
        Logger.error("Failed to get NEAR stake: #{inspect(reason)}")
        {:error, "Failed to get stake: #{inspect(reason)}"}
    end
  end

  def get_wallet_stakes(wallet_id) do
    case NearWalletClient.get_wallet_stakes(wallet_id) do
      {:ok, records} ->
        stakes =
          records
          |> Enum.map(&NearStake.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearStake.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, stake} -> stake end)

        {:ok, stakes}

      {:error, reason} ->
        Logger.error("Failed to get NEAR wallet stakes: #{inspect(reason)}")
        {:error, "Failed to get stakes: #{inspect(reason)}"}
    end
  end

  def get_user_stakes(user_id) do
    case NearWalletClient.get_user_stakes(user_id) do
      {:ok, records} ->
        stakes =
          records
          |> Enum.map(&NearStake.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearStake.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, stake} -> stake end)

        {:ok, stakes}

      {:error, reason} ->
        Logger.error("Failed to get user NEAR stakes: #{inspect(reason)}")
        {:error, "Failed to get stakes: #{inspect(reason)}"}
    end
  end

  def update_stake(stake_id, updates) do
    case NearWalletClient.update_stake(stake_id, updates) do
      :ok ->
        :ok

      {:error, :stake_not_found} ->
        {:error, "Stake not found"}

      {:error, reason} ->
        Logger.error("Failed to update NEAR stake: #{inspect(reason)}")
        {:error, "Failed to update stake: #{inspect(reason)}"}
    end
  end

  def create_implicit_account(wallet_id, implicit_data) do
    case NearWalletClient.create_implicit_account(wallet_id, implicit_data) do
      {:ok, implicit_id} ->
        {:ok, to_string(implicit_id)}

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to create NEAR implicit account: #{inspect(reason)}")
        {:error, "Failed to create implicit account: #{inspect(reason)}"}
    end
  end

  def get_implicit_account(implicit_id) do
    case NearWalletClient.get_implicit_account(implicit_id) do
      {:ok, record} ->
        changeset = NearImplicitAccount.erl_changeset(record)
        NearImplicitAccount.build(changeset)

      {:error, :implicit_account_not_found} ->
        {:error, "Implicit account not found"}

      {:error, reason} ->
        Logger.error("Failed to get NEAR implicit account: #{inspect(reason)}")
        {:error, "Failed to get implicit account: #{inspect(reason)}"}
    end
  end

  def get_implicit_account_by_account_id(account_id) do
    case NearWalletClient.get_implicit_account_by_account_id(account_id) do
      {:ok, record} ->
        changeset = NearImplicitAccount.erl_changeset(record)
        NearImplicitAccount.build(changeset)

      {:error, :implicit_account_not_found} ->
        {:error, "Implicit account not found"}

      {:error, reason} ->
        Logger.error("Failed to get NEAR implicit account by account ID: #{inspect(reason)}")
        {:error, "Failed to get implicit account: #{inspect(reason)}"}
    end
  end

  def get_wallet_implicit_accounts(wallet_id) do
    case NearWalletClient.get_wallet_implicit_accounts(wallet_id) do
      {:ok, records} ->
        accounts =
          records
          |> Enum.map(&NearImplicitAccount.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearImplicitAccount.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, account} -> account end)

        {:ok, accounts}

      {:error, reason} ->
        Logger.error("Failed to get NEAR wallet implicit accounts: #{inspect(reason)}")
        {:error, "Failed to get implicit accounts: #{inspect(reason)}"}
    end
  end

  def mark_implicit_funded(implicit_id, funded_at) do
    case NearWalletClient.mark_implicit_funded(implicit_id, funded_at) do
      :ok ->
        :ok

      {:error, :implicit_account_not_found} ->
        {:error, "Implicit account not found"}

      {:error, reason} ->
        Logger.error("Failed to mark NEAR implicit account as funded: #{inspect(reason)}")
        {:error, "Failed to mark implicit account as funded: #{inspect(reason)}"}
    end
  end

  def create_social_post(wallet_id, post_data) do
    case NearWalletClient.create_social_post(wallet_id, post_data) do
      {:ok, post_id} ->
        {:ok, to_string(post_id)}

      {:error, :wallet_not_found} ->
        {:error, "Wallet not found"}

      {:error, reason} ->
        Logger.error("Failed to create NEAR social post: #{inspect(reason)}")
        {:error, "Failed to create social post: #{inspect(reason)}"}
    end
  end

  def get_social_post(post_id) do
    case NearWalletClient.get_social_post(post_id) do
      {:ok, record} ->
        changeset = NearSocialPost.erl_changeset(record)
        NearSocialPost.build(changeset)

      {:error, :post_not_found} ->
        {:error, "Social post not found"}

      {:error, reason} ->
        Logger.error("Failed to get NEAR social post: #{inspect(reason)}")
        {:error, "Failed to get social post: #{inspect(reason)}"}
    end
  end

  def get_wallet_social_posts(wallet_id) do
    case NearWalletClient.get_wallet_social_posts(wallet_id) do
      {:ok, records} ->
        posts =
          records
          |> Enum.map(&NearSocialPost.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearSocialPost.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, post} -> post end)

        {:ok, posts}

      {:error, reason} ->
        Logger.error("Failed to get NEAR wallet social posts: #{inspect(reason)}")
        {:error, "Failed to get social posts: #{inspect(reason)}"}
    end
  end

  def get_user_social_posts(user_id) do
    case NearWalletClient.get_user_social_posts(user_id) do
      {:ok, records} ->
        posts =
          records
          |> Enum.map(&NearSocialPost.erl_changeset_safe/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, changeset} -> changeset end)
          |> Enum.map(&NearSocialPost.build/1)
          |> Enum.filter(fn
            {:ok, _} -> true
            {:error, _} -> false
          end)
          |> Enum.map(fn {:ok, post} -> post end)

        {:ok, posts}

      {:error, reason} ->
        Logger.error("Failed to get user NEAR social posts: #{inspect(reason)}")
        {:error, "Failed to get social posts: #{inspect(reason)}"}
    end
  end
end
