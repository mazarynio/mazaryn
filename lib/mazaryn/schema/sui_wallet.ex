defmodule Schema.SuiWallet do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:label, :string)
    field(:sui_address, :string)
    field(:public_key, :string)
    field(:encrypted_private_key, :string)
    field(:encryption_iv, :string)
    field(:encryption_tag, :string)
    field(:scheme, :string)
    field(:network, :string)
    field(:is_primary, :boolean, default: false)
    field(:created_at, :integer)
    field(:last_used, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:sui_wallet, wallet_id, user_id, label, sui_address, public_key,
         encrypted_private_key, encryption_iv, encryption_tag, scheme, network,
         is_primary, created_at, last_used, metadata}
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
      sui_address: to_string(sui_address),
      public_key:
        case public_key do
          :undefined -> nil
          v -> to_string(v)
        end,
      encrypted_private_key: to_string(encrypted_private_key),
      encryption_iv: to_string(encryption_iv),
      encryption_tag: to_string(encryption_tag),
      scheme:
        case scheme do
          :undefined -> "ed25519"
          v -> to_string(v)
        end,
      network:
        case network do
          :undefined -> "mainnet"
          v -> to_string(v)
        end,
      is_primary:
        case is_primary do
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
      metadata:
        case metadata do
          :undefined -> %{}
          v when is_map(v) -> v
          _ -> %{}
        end
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Sui wallet operation timed out")
    raise ArgumentError, "Sui wallet operation timed out. Please try again."
  end

  def erl_changeset({:error, :wallet_not_found}) do
    Logger.error("Sui wallet not found")
    raise ArgumentError, "Wallet not found"
  end

  def erl_changeset({:error, :address_already_exists}) do
    Logger.error("Sui wallet address already exists")
    raise ArgumentError, "Sui address already exists"
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Sui wallet operation failed with reason: #{reason}")
    raise ArgumentError, "Sui wallet operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Sui wallet operation failed: #{inspect(reason)}")
    raise ArgumentError, "Sui wallet operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("Sui wallet operation returned nil")
    raise ArgumentError, "Wallet not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in SuiWallet.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid Sui wallet data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:sui_wallet, _wallet_id, _user_id, _label, _sui_address, _public_key,
         _encrypted_private_key, _encryption_iv, _encryption_tag, _scheme, _network,
         _is_primary, _created_at, _last_used, _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("Sui wallet operation timed out")
    {:error, "Wallet operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, :wallet_not_found}) do
    Logger.warning("Sui wallet not found")
    {:error, "Wallet not found"}
  end

  def erl_changeset_safe({:error, :address_already_exists}) do
    Logger.warning("Sui address already exists")
    {:error, "Sui address already exists"}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("Sui wallet operation failed: #{inspect(reason)}")
    {:error, "Wallet operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("Sui wallet operation returned nil")
    {:error, "Wallet not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in SuiWallet.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid Sui wallet data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end

defmodule Schema.SuiTransaction do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:tx_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:digest, :string)
    field(:tx_type, :string)
    field(:sender_address, :string)
    field(:recipient_address, :string)
    field(:amount_mist, :string)
    field(:coin_type, :string)
    field(:object_id, :string)
    field(:package_id, :string)
    field(:module_name, :string)
    field(:function_name, :string)
    field(:validator_address, :string)
    field(:staked_sui_id, :string)
    field(:gas_computation, :string, default: "0")
    field(:gas_storage, :string, default: "0")
    field(:gas_storage_rebate, :string, default: "0")
    field(:status, :string)
    field(:effects_status, :string)
    field(:checkpoint, :string)
    field(:epoch, :string)
    field(:error_message, :string)
    field(:network, :string)
    field(:created_at, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:sui_transaction, tx_id, wallet_id, user_id, digest, tx_type, sender_address,
         recipient_address, amount_mist, coin_type, object_id, package_id, module_name,
         function_name, validator_address, staked_sui_id, gas_computation, gas_storage,
         gas_storage_rebate, status, effects_status, checkpoint, epoch, error_message,
         network, created_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      tx_id: tx_id,
      wallet_id: wallet_id,
      user_id: user_id,
      digest: s(digest),
      tx_type: s(tx_type),
      sender_address: s(sender_address),
      recipient_address: s(recipient_address),
      amount_mist: s(amount_mist),
      coin_type: s(coin_type),
      object_id: s(object_id),
      package_id: s(package_id),
      module_name: s(module_name),
      function_name: s(function_name),
      validator_address: s(validator_address),
      staked_sui_id: s(staked_sui_id),
      gas_computation: s(gas_computation) || "0",
      gas_storage: s(gas_storage) || "0",
      gas_storage_rebate: s(gas_storage_rebate) || "0",
      status: s(status),
      effects_status: s(effects_status),
      checkpoint: s(checkpoint),
      epoch: s(epoch),
      error_message: s(error_message),
      network: s(network),
      created_at: i(created_at),
      metadata: m(metadata)
    })
  end

  def erl_changeset({:error, reason}),
    do: raise(ArgumentError, "Sui transaction error: #{inspect(reason)}")

  def erl_changeset(nil), do: raise(ArgumentError, "Transaction not found")
  def erl_changeset(v), do: raise(ArgumentError, "Invalid Sui transaction format: #{inspect(v)}")

  def erl_changeset_safe(record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def build(changeset), do: apply_action(changeset, :build)

  defp s(:undefined), do: nil
  defp s(nil), do: nil
  defp s(v) when is_binary(v), do: v
  defp s(v), do: to_string(v)

  defp i(:undefined), do: nil
  defp i(nil), do: nil
  defp i(v) when is_integer(v), do: v
  defp i(_), do: nil

  defp m(:undefined), do: %{}
  defp m(v) when is_map(v), do: v
  defp m(_), do: %{}
end

defmodule Schema.SuiStake do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:stake_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:owner_address, :string)
    field(:validator_address, :string)
    field(:staked_sui_id, :string)
    field(:principal_mist, :string)
    field(:estimated_reward_mist, :string, default: "0")
    field(:stake_activation_epoch, :string)
    field(:status, :string)
    field(:tx_type, :string)
    field(:digest, :string)
    field(:gas_computation, :string, default: "0")
    field(:gas_storage, :string, default: "0")
    field(:gas_storage_rebate, :string, default: "0")
    field(:network, :string)
    field(:created_at, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:sui_stake, stake_id, wallet_id, user_id, owner_address, validator_address,
         staked_sui_id, principal_mist, estimated_reward_mist, stake_activation_epoch,
         status, tx_type, digest, gas_computation, gas_storage, gas_storage_rebate,
         network, created_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      stake_id: stake_id,
      wallet_id: wallet_id,
      user_id: user_id,
      owner_address: s(owner_address),
      validator_address: s(validator_address),
      staked_sui_id: s(staked_sui_id),
      principal_mist: s(principal_mist),
      estimated_reward_mist: s(estimated_reward_mist) || "0",
      stake_activation_epoch: s(stake_activation_epoch),
      status: s(status),
      tx_type: s(tx_type),
      digest: s(digest),
      gas_computation: s(gas_computation) || "0",
      gas_storage: s(gas_storage) || "0",
      gas_storage_rebate: s(gas_storage_rebate) || "0",
      network: s(network),
      created_at: i(created_at),
      metadata: m(metadata)
    })
  end

  def erl_changeset({:error, reason}),
    do: raise(ArgumentError, "Sui stake error: #{inspect(reason)}")

  def erl_changeset(nil), do: raise(ArgumentError, "Stake not found")
  def erl_changeset(v), do: raise(ArgumentError, "Invalid Sui stake format: #{inspect(v)}")

  def erl_changeset_safe(record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def build(changeset), do: apply_action(changeset, :build)

  defp s(:undefined), do: nil
  defp s(nil), do: nil
  defp s(v) when is_binary(v), do: v
  defp s(v), do: to_string(v)

  defp i(:undefined), do: nil
  defp i(nil), do: nil
  defp i(v) when is_integer(v), do: v
  defp i(_), do: nil

  defp m(:undefined), do: %{}
  defp m(v) when is_map(v), do: v
  defp m(_), do: %{}
end

defmodule Schema.SuiObject do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    field(:object_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:owner_address, :string)
    field(:object_type, :string)
    field(:version, :string)
    field(:digest, :string)
    field(:balance, :string)
    field(:previous_transaction, :string)
    field(:storage_rebate, :string)
    field(:is_nft, :boolean, default: false)
    field(:nft_name, :string)
    field(:nft_description, :string)
    field(:nft_url, :string)
    field(:network, :string)
    field(:synced_at, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:sui_object, object_id, wallet_id, user_id, owner_address, object_type,
         version, digest, balance, previous_transaction, storage_rebate, is_nft,
         nft_name, nft_description, nft_url, network, synced_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      object_id: object_id,
      wallet_id: wallet_id,
      user_id: user_id,
      owner_address: s(owner_address),
      object_type: s(object_type),
      version: s(version),
      digest: s(digest),
      balance: s(balance),
      previous_transaction: s(previous_transaction),
      storage_rebate: s(storage_rebate),
      is_nft: if(is_boolean(is_nft), do: is_nft, else: false),
      nft_name: s(nft_name),
      nft_description: s(nft_description),
      nft_url: s(nft_url),
      network: s(network),
      synced_at: i(synced_at),
      metadata: m(metadata)
    })
  end

  def erl_changeset({:error, reason}),
    do: raise(ArgumentError, "Sui object error: #{inspect(reason)}")

  def erl_changeset(nil), do: raise(ArgumentError, "Object not found")
  def erl_changeset(v), do: raise(ArgumentError, "Invalid Sui object format: #{inspect(v)}")

  def erl_changeset_safe(record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def build(changeset), do: apply_action(changeset, :build)

  defp s(:undefined), do: nil
  defp s(nil), do: nil
  defp s(v) when is_binary(v), do: v
  defp s(v), do: to_string(v)

  defp i(:undefined), do: nil
  defp i(nil), do: nil
  defp i(v) when is_integer(v), do: v
  defp i(_), do: nil

  defp m(:undefined), do: %{}
  defp m(v) when is_map(v), do: v
  defp m(_), do: %{}
end

defmodule Schema.SuiCoinBalance do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    field(:balance_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:owner_address, :string)
    field(:coin_type, :string)
    field(:total_balance, :string, default: "0")
    field(:coin_object_count, :integer, default: 0)
    field(:network, :string)
    field(:synced_at, :integer)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:sui_coin_balance, balance_id, wallet_id, user_id, owner_address, coin_type,
         total_balance, coin_object_count, network, synced_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      balance_id: balance_id,
      wallet_id: wallet_id,
      user_id: user_id,
      owner_address: s(owner_address),
      coin_type: s(coin_type),
      total_balance: s(total_balance) || "0",
      coin_object_count: if(is_integer(coin_object_count), do: coin_object_count, else: 0),
      network: s(network),
      synced_at: i(synced_at),
      metadata: m(metadata)
    })
  end

  def erl_changeset({:error, reason}),
    do: raise(ArgumentError, "Sui coin balance error: #{inspect(reason)}")

  def erl_changeset(nil), do: raise(ArgumentError, "Coin balance not found")
  def erl_changeset(v), do: raise(ArgumentError, "Invalid Sui coin balance format: #{inspect(v)}")

  def erl_changeset_safe(record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def build(changeset), do: apply_action(changeset, :build)

  defp s(:undefined), do: nil
  defp s(nil), do: nil
  defp s(v) when is_binary(v), do: v
  defp s(v), do: to_string(v)

  defp i(:undefined), do: nil
  defp i(nil), do: nil
  defp i(v) when is_integer(v), do: v
  defp i(_), do: nil

  defp m(:undefined), do: %{}
  defp m(v) when is_map(v), do: v
  defp m(_), do: %{}
end
