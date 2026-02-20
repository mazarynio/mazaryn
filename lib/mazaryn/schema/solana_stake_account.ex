defmodule Schema.SolanaStakeAccount do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:stake_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:stake_account_address, :string)
    field(:validator_vote_address, :string)
    field(:amount_lamports, :integer)
    field(:status, :string)
    field(:signature, :string)
    field(:created_at, :utc_datetime)
    field(:delegated_at, :utc_datetime)
    field(:deactivated_at, :utc_datetime)
    field(:withdrawn_at, :utc_datetime)
    field(:rewards_earned_lamports, :integer, default: 0)
    field(:metadata, :map, default: %{})
  end

  defp to_opt_string(:undefined), do: nil
  defp to_opt_string(v), do: to_string(v)

  defp to_opt_int(:undefined), do: nil
  defp to_opt_int(v), do: v

  defp to_opt_map(:undefined), do: %{}
  defp to_opt_map(v) when is_map(v), do: v
  defp to_opt_map(_), do: %{}

  defp to_opt_datetime(:undefined), do: nil
  defp to_opt_datetime(v), do: v

  def erl_changeset(
        {:solana_stake_account, stake_id, wallet_id, user_id, stake_account_address,
         validator_vote_address, amount_lamports, status, signature, created_at, delegated_at,
         deactivated_at, withdrawn_at, rewards_earned_lamports, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      stake_id: stake_id,
      wallet_id: wallet_id,
      user_id: user_id,
      stake_account_address: stake_account_address,
      validator_vote_address: to_opt_string(validator_vote_address),
      amount_lamports: to_opt_int(amount_lamports),
      status: to_opt_string(status),
      signature: to_opt_string(signature),
      created_at: created_at,
      delegated_at: to_opt_datetime(delegated_at),
      deactivated_at: to_opt_datetime(deactivated_at),
      withdrawn_at: to_opt_datetime(withdrawn_at),
      rewards_earned_lamports:
        case rewards_earned_lamports do
          :undefined -> 0
          v -> v
        end,
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Solana stake account operation timed out")
    raise ArgumentError, "Solana stake account operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Solana stake account operation failed: #{reason}")
    raise ArgumentError, "Solana stake account operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Solana stake account operation failed: #{inspect(reason)}")
    raise ArgumentError, "Solana stake account operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    raise ArgumentError, "Stake account not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in SolanaStakeAccount.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid stake account data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:solana_stake_account, _stake_id, _wallet_id, _user_id, _stake_account_address,
         _validator_vote_address, _amount_lamports, _status, _signature, _created_at,
         _delegated_at, _deactivated_at, _withdrawn_at, _rewards_earned_lamports, _metadata} =
          record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    {:error, "Stake account operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    {:error, "Stake account operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    {:error, "Stake account not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in SolanaStakeAccount.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid stake account data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
