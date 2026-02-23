defmodule Schema.NearWallet do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:label, :string)
    field(:account_id, :string)
    field(:encrypted_private_key, :string)
    field(:encryption_iv, :string)
    field(:encryption_tag, :string)
    field(:network, :string)
    field(:created_at, :utc_datetime)
    field(:last_used, :utc_datetime)
    field(:is_primary, :boolean, default: false)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:near_wallet, wallet_id, user_id, label, account_id, encrypted_private_key,
         encryption_iv, encryption_tag, network, created_at, last_used, is_primary, metadata}
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
      account_id: to_string(account_id),
      encrypted_private_key: to_string(encrypted_private_key),
      encryption_iv: to_string(encryption_iv),
      encryption_tag: to_string(encryption_tag),
      network:
        case network do
          :undefined -> nil
          v -> to_string(v)
        end,
      created_at: created_at,
      last_used:
        case last_used do
          :undefined -> nil
          v -> v
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
    Logger.error("Near wallet operation timed out")
    raise ArgumentError, "Near wallet operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Near wallet operation failed with reason: #{reason}")
    raise ArgumentError, "Near wallet operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Near wallet operation failed: #{inspect(reason)}")
    raise ArgumentError, "Near wallet operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("Near wallet operation returned nil")
    raise ArgumentError, "Wallet not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in NearWallet.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid wallet data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:near_wallet, _wallet_id, _user_id, _label, _account_id, _encrypted_private_key,
         _encryption_iv, _encryption_tag, _network, _created_at, _last_used, _is_primary,
         _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("Near wallet operation timed out")
    {:error, "Wallet operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("Near wallet operation failed: #{inspect(reason)}")
    {:error, "Wallet operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("Near wallet operation returned nil")
    {:error, "Wallet not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in NearWallet.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid wallet data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
