defmodule Schema.NearStake do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:stake_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:validator_account_id, :string)
    field(:validator_public_key, :string)
    field(:amount_near, :float)
    field(:status, :string)
    field(:transaction_hash, :string)
    field(:created_at, :utc_datetime)
    field(:unstaked_at, :utc_datetime)
    field(:withdrawn_at, :utc_datetime)
    field(:metadata, :map, default: %{})
  end

  defp to_opt_string(:undefined), do: nil
  defp to_opt_string(v), do: to_string(v)

  defp to_opt_float(:undefined), do: nil
  defp to_opt_float(v) when is_float(v), do: v
  defp to_opt_float(v) when is_integer(v), do: v * 1.0
  defp to_opt_float(v) when is_list(v) do
    case Float.parse(List.to_string(v)) do
      {f, _} -> f
      :error -> nil
    end
  end
  defp to_opt_float(v) when is_binary(v) do
    case Float.parse(v) do
      {f, _} -> f
      :error -> nil
    end
  end
  defp to_opt_float(_), do: nil

  defp to_opt_datetime(:undefined), do: nil
  defp to_opt_datetime(v), do: v

  defp to_opt_map(:undefined), do: %{}
  defp to_opt_map(v) when is_map(v), do: v
  defp to_opt_map(_), do: %{}

  def erl_changeset(
        {:near_stake, stake_id, wallet_id, user_id, validator_account_id, validator_public_key,
         amount_near, status, transaction_hash, created_at, unstaked_at, withdrawn_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      stake_id: to_string(stake_id),
      wallet_id: to_string(wallet_id),
      user_id: to_string(user_id),
      validator_account_id: to_opt_string(validator_account_id),
      validator_public_key: to_opt_string(validator_public_key),
      amount_near: to_opt_float(amount_near),
      status: to_opt_string(status),
      transaction_hash: to_opt_string(transaction_hash),
      created_at: created_at,
      unstaked_at: to_opt_datetime(unstaked_at),
      withdrawn_at: to_opt_datetime(withdrawn_at),
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Near stake operation timed out")
    raise ArgumentError, "Near stake operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Near stake operation failed with reason: #{reason}")
    raise ArgumentError, "Near stake operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Near stake operation failed: #{inspect(reason)}")
    raise ArgumentError, "Near stake operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("Near stake operation returned nil")
    raise ArgumentError, "Stake not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in NearStake.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid stake data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:near_stake, _stake_id, _wallet_id, _user_id, _validator_account_id,
         _validator_public_key, _amount_near, _status, _transaction_hash, _created_at,
         _unstaked_at, _withdrawn_at, _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("Near stake operation timed out")
    {:error, "Stake operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("Near stake operation failed: #{inspect(reason)}")
    {:error, "Stake operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("Near stake operation returned nil")
    {:error, "Stake not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in NearStake.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid stake data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
