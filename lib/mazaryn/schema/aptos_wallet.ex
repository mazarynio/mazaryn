defmodule Schema.AptosWallet do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:label, :string)
    field(:account_address, :string)
    field(:encrypted_private_key, :string)
    field(:encryption_iv, :string)
    field(:encryption_tag, :string)
    field(:network, :string)
    field(:signing_scheme, :string)
    field(:is_legacy, :boolean, default: false)
    field(:created_at, :integer)
    field(:last_used, :integer)
    field(:is_primary, :boolean, default: false)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:aptos_wallet, wallet_id, user_id, label, account_address, encrypted_private_key,
         encryption_iv, encryption_tag, network, signing_scheme, is_legacy, created_at,
         last_used, is_primary, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      wallet_id: wallet_id,
      user_id: user_id,
      label:
        case label do
          :undefined -> nil
          v -> to_string(v)
        end,
      account_address: to_string(account_address),
      encrypted_private_key: to_string(encrypted_private_key),
      encryption_iv: to_string(encryption_iv),
      encryption_tag: to_string(encryption_tag),
      network:
        case network do
          :undefined -> nil
          v -> to_string(v)
        end,
      signing_scheme:
        case signing_scheme do
          :undefined -> nil
          v -> to_string(v)
        end,
      is_legacy:
        case is_legacy do
          :undefined -> false
          v -> v
        end,
      created_at:
        case created_at do
          :undefined -> nil
          v when is_integer(v) -> v
          _ -> nil
        end,
      last_used:
        case last_used do
          :undefined -> nil
          v when is_integer(v) -> v
          _ -> nil
        end,
      is_primary:
        case is_primary do
          :undefined -> false
          v -> v
        end,
      metadata:
        case metadata do
          :undefined -> %{}
          v when is_map(v) -> v
          _ -> %{}
        end
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Aptos wallet operation timed out")
    raise ArgumentError, "Aptos wallet operation timed out. Please try again."
  end

  def erl_changeset({:error, :wallet_not_found}) do
    Logger.error("Aptos wallet not found")
    raise ArgumentError, "Wallet not found"
  end

  def erl_changeset({:error, :address_already_exists}) do
    Logger.error("Aptos wallet address already exists")
    raise ArgumentError, "Wallet address already exists"
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Aptos wallet operation failed with reason: #{reason}")
    raise ArgumentError, "Aptos wallet operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Aptos wallet operation failed: #{inspect(reason)}")
    raise ArgumentError, "Aptos wallet operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("Aptos wallet operation returned nil")
    raise ArgumentError, "Wallet not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in AptosWallet.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid wallet data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:aptos_wallet, _wallet_id, _user_id, _label, _account_address,
         _encrypted_private_key, _encryption_iv, _encryption_tag, _network,
         _signing_scheme, _is_legacy, _created_at, _last_used, _is_primary,
         _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("Aptos wallet operation timed out")
    {:error, "Wallet operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, :wallet_not_found}) do
    Logger.warning("Aptos wallet not found")
    {:error, "Wallet not found"}
  end

  def erl_changeset_safe({:error, :address_already_exists}) do
    Logger.warning("Aptos wallet address already exists")
    {:error, "Wallet address already exists"}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("Aptos wallet operation failed: #{inspect(reason)}")
    {:error, "Wallet operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("Aptos wallet operation returned nil")
    {:error, "Wallet not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in AptosWallet.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid wallet data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end

defmodule Schema.AptosTransaction do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:tx_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:transaction_hash, :string)
    field(:tx_type, :string)
    field(:sender_address, :string)
    field(:receiver_address, :string)
    field(:amount_octas, :integer)
    field(:gas_used, :integer)
    field(:gas_unit_price, :integer)
    field(:sequence_number, :integer)
    field(:function_called, :string)
    field(:coin_type, :string)
    field(:metadata_address, :string)
    field(:pool_address, :string)
    field(:collection_name, :string)
    field(:token_address, :string)
    field(:status, :string)
    field(:vm_status, :string)
    field(:version, :string)
    field(:error_message, :string)
    field(:created_at, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:aptos_transaction, tx_id, wallet_id, user_id, transaction_hash, tx_type,
         sender_address, receiver_address, amount_octas, gas_used, gas_unit_price,
         sequence_number, function_called, coin_type, metadata_address, pool_address,
         collection_name, token_address, status, vm_status, version, error_message,
         created_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      tx_id: tx_id,
      wallet_id: wallet_id,
      user_id: user_id,
      transaction_hash: to_string_safe(transaction_hash),
      tx_type: to_string_safe(tx_type),
      sender_address: to_string_safe(sender_address),
      receiver_address: to_string_safe(receiver_address),
      amount_octas: to_integer_safe(amount_octas),
      gas_used: to_integer_safe(gas_used),
      gas_unit_price: to_integer_safe(gas_unit_price),
      sequence_number: to_integer_safe(sequence_number),
      function_called: to_string_safe(function_called),
      coin_type: to_string_safe(coin_type),
      metadata_address: to_string_safe(metadata_address),
      pool_address: to_string_safe(pool_address),
      collection_name: to_string_safe(collection_name),
      token_address: to_string_safe(token_address),
      status: to_string_safe(status),
      vm_status: to_string_safe(vm_status),
      version: to_string_safe(version),
      error_message: to_string_safe(error_message),
      created_at: to_integer_safe(created_at),
      metadata: to_map_safe(metadata)
    })
  end

  def erl_changeset({:error, reason}), do: raise(ArgumentError, "Aptos transaction error: #{inspect(reason)}")
  def erl_changeset(nil), do: raise(ArgumentError, "Transaction not found")
  def erl_changeset(v), do: raise(ArgumentError, "Invalid transaction format: #{inspect(v)}")

  def erl_changeset_safe(record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def build(changeset), do: apply_action(changeset, :build)

  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(nil), do: nil
  defp to_string_safe(v), do: to_string(v)

  defp to_integer_safe(:undefined), do: nil
  defp to_integer_safe(nil), do: nil
  defp to_integer_safe(v) when is_integer(v), do: v
  defp to_integer_safe(_), do: nil

  defp to_map_safe(:undefined), do: %{}
  defp to_map_safe(v) when is_map(v), do: v
  defp to_map_safe(_), do: %{}
end

defmodule Schema.AptosNftCollection do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    field(:collection_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:creator_address, :string)
    field(:collection_name, :string)
    field(:description, :string)
    field(:uri, :string)
    field(:max_supply, :integer)
    field(:current_supply, :integer)
    field(:transaction_hash, :string)
    field(:on_chain_id, :string)
    field(:created_at, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:aptos_nft_collection, collection_id, wallet_id, user_id, creator_address,
         collection_name, description, uri, max_supply, current_supply, transaction_hash,
         on_chain_id, created_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      collection_id: collection_id,
      wallet_id: wallet_id,
      user_id: user_id,
      creator_address: to_string_safe(creator_address),
      collection_name: to_string_safe(collection_name),
      description: to_string_safe(description),
      uri: to_string_safe(uri),
      max_supply: to_integer_safe(max_supply),
      current_supply: to_integer_safe(current_supply),
      transaction_hash: to_string_safe(transaction_hash),
      on_chain_id: to_string_safe(on_chain_id),
      created_at: to_integer_safe(created_at),
      metadata: to_map_safe(metadata)
    })
  end

  def erl_changeset({:error, reason}), do: raise(ArgumentError, "Aptos NFT collection error: #{inspect(reason)}")
  def erl_changeset(nil), do: raise(ArgumentError, "NFT collection not found")
  def erl_changeset(v), do: raise(ArgumentError, "Invalid NFT collection format: #{inspect(v)}")

  def erl_changeset_safe(record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def build(changeset), do: apply_action(changeset, :build)

  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(nil), do: nil
  defp to_string_safe(v), do: to_string(v)

  defp to_integer_safe(:undefined), do: nil
  defp to_integer_safe(nil), do: nil
  defp to_integer_safe(v) when is_integer(v), do: v
  defp to_integer_safe(_), do: nil

  defp to_map_safe(:undefined), do: %{}
  defp to_map_safe(v) when is_map(v), do: v
  defp to_map_safe(_), do: %{}
end

defmodule Schema.AptosNftToken do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    field(:token_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:collection_id, :string)
    field(:collection_name, :string)
    field(:token_name, :string)
    field(:description, :string)
    field(:uri, :string)
    field(:property_keys, {:array, :string}, default: [])
    field(:property_types, {:array, :string}, default: [])
    field(:property_values, {:array, :string}, default: [])
    field(:transaction_hash, :string)
    field(:on_chain_id, :string)
    field(:created_at, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:aptos_nft_token, token_id, wallet_id, user_id, collection_id, collection_name,
         token_name, description, uri, property_keys, property_types, property_values,
         transaction_hash, on_chain_id, created_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      token_id: token_id,
      wallet_id: wallet_id,
      user_id: user_id,
      collection_id: to_string_safe(collection_id),
      collection_name: to_string_safe(collection_name),
      token_name: to_string_safe(token_name),
      description: to_string_safe(description),
      uri: to_string_safe(uri),
      property_keys: to_list_safe(property_keys),
      property_types: to_list_safe(property_types),
      property_values: to_list_safe(property_values),
      transaction_hash: to_string_safe(transaction_hash),
      on_chain_id: to_string_safe(on_chain_id),
      created_at: to_integer_safe(created_at),
      metadata: to_map_safe(metadata)
    })
  end

  def erl_changeset({:error, reason}), do: raise(ArgumentError, "Aptos NFT token error: #{inspect(reason)}")
  def erl_changeset(nil), do: raise(ArgumentError, "NFT token not found")
  def erl_changeset(v), do: raise(ArgumentError, "Invalid NFT token format: #{inspect(v)}")

  def erl_changeset_safe(record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def build(changeset), do: apply_action(changeset, :build)

  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(nil), do: nil
  defp to_string_safe(v), do: to_string(v)

  defp to_integer_safe(:undefined), do: nil
  defp to_integer_safe(nil), do: nil
  defp to_integer_safe(v) when is_integer(v), do: v
  defp to_integer_safe(_), do: nil

  defp to_map_safe(:undefined), do: %{}
  defp to_map_safe(v) when is_map(v), do: v
  defp to_map_safe(_), do: %{}

  defp to_list_safe(:undefined), do: []
  defp to_list_safe(v) when is_list(v), do: Enum.map(v, &to_string/1)
  defp to_list_safe(_), do: []
end

defmodule Schema.AptosStake do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    field(:stake_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:delegator_address, :string)
    field(:pool_address, :string)
    field(:amount_octas, :integer)
    field(:active_stake, :string)
    field(:inactive_stake, :string)
    field(:pending_inactive_stake, :string)
    field(:tx_type, :string)
    field(:transaction_hash, :string)
    field(:gas_used, :integer)
    field(:gas_unit_price, :integer)
    field(:vm_status, :string)
    field(:version, :string)
    field(:network, :string)
    field(:status, :string)
    field(:created_at, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:aptos_stake, stake_id, wallet_id, user_id, delegator_address, pool_address,
         amount_octas, active_stake, inactive_stake, pending_inactive_stake, tx_type,
         transaction_hash, gas_used, gas_unit_price, vm_status, version, network,
         status, created_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      stake_id: stake_id,
      wallet_id: wallet_id,
      user_id: user_id,
      delegator_address: to_string_safe(delegator_address),
      pool_address: to_string_safe(pool_address),
      amount_octas: to_integer_safe(amount_octas),
      active_stake: to_string_safe(active_stake),
      inactive_stake: to_string_safe(inactive_stake),
      pending_inactive_stake: to_string_safe(pending_inactive_stake),
      tx_type: to_string_safe(tx_type),
      transaction_hash: to_string_safe(transaction_hash),
      gas_used: to_integer_safe(gas_used),
      gas_unit_price: to_integer_safe(gas_unit_price),
      vm_status: to_string_safe(vm_status),
      version: to_string_safe(version),
      network: to_string_safe(network),
      status: to_string_safe(status),
      created_at: to_integer_safe(created_at),
      metadata: to_map_safe(metadata)
    })
  end

  def erl_changeset({:error, reason}), do: raise(ArgumentError, "Aptos stake error: #{inspect(reason)}")
  def erl_changeset(nil), do: raise(ArgumentError, "Stake not found")
  def erl_changeset(v), do: raise(ArgumentError, "Invalid stake format: #{inspect(v)}")

  def erl_changeset_safe(record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def build(changeset), do: apply_action(changeset, :build)

  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(nil), do: nil
  defp to_string_safe(v), do: to_string(v)

  defp to_integer_safe(:undefined), do: nil
  defp to_integer_safe(nil), do: nil
  defp to_integer_safe(v) when is_integer(v), do: v
  defp to_integer_safe(_), do: nil

  defp to_map_safe(:undefined), do: %{}
  defp to_map_safe(v) when is_map(v), do: v
  defp to_map_safe(_), do: %{}
end
