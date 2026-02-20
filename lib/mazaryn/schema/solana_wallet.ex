defmodule Schema.SolanaWallet do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:label, :string)
    field(:public_key, :string)
    field(:encrypted_private_key, :string)
    field(:encryption_iv, :string)
    field(:encryption_auth_tag, :string)
    field(:encryption_salt, :string)
    field(:derivation_path, :string)
    field(:created_at, :utc_datetime)
    field(:last_used, :utc_datetime)
    field(:is_primary, :boolean, default: false)
    field(:metadata, :map, default: %{})
  end

  def erl_changeset(
        {:solana_wallet, wallet_id, user_id, label, public_key, encrypted_private_key,
         encryption_iv, encryption_auth_tag, encryption_salt, derivation_path, created_at,
         last_used, is_primary, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      wallet_id: wallet_id,
      user_id: user_id,
      label:
        case label do
          :undefined -> nil
          v -> v
        end,
      public_key: public_key,
      encrypted_private_key: encrypted_private_key,
      encryption_iv: encryption_iv,
      encryption_auth_tag: encryption_auth_tag,
      encryption_salt: encryption_salt,
      derivation_path:
        case derivation_path do
          :undefined -> nil
          v -> v
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
    Logger.error("Solana wallet operation timed out")
    raise ArgumentError, "Solana wallet operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Solana wallet operation failed with reason: #{reason}")
    raise ArgumentError, "Solana wallet operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Solana wallet operation failed: #{inspect(reason)}")
    raise ArgumentError, "Solana wallet operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("Solana wallet operation returned nil")
    raise ArgumentError, "Wallet not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in SolanaWallet.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid wallet data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:solana_wallet, _wallet_id, _user_id, _label, _public_key, _encrypted_private_key,
         _encryption_iv, _encryption_auth_tag, _encryption_salt, _derivation_path, _created_at,
         _last_used, _is_primary, _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("Solana wallet operation timed out")
    {:error, "Wallet operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("Solana wallet operation failed: #{inspect(reason)}")
    {:error, "Wallet operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("Solana wallet operation returned nil")
    {:error, "Wallet not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in SolanaWallet.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid wallet data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
