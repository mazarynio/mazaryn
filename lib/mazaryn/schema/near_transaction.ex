defmodule Schema.NearTransaction do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:tx_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:transaction_hash, :string)
    field(:tx_type, :string)
    field(:from_account_id, :string)
    field(:receiver_id, :string)
    field(:amount_near, :float)
    field(:contract_id, :string)
    field(:method_name, :string)
    field(:status, :string)
    field(:actions_count, :integer)
    field(:error_message, :string)
    field(:created_at, :utc_datetime)
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

  defp to_opt_int(:undefined), do: nil
  defp to_opt_int(v), do: v

  defp to_opt_map(:undefined), do: %{}
  defp to_opt_map(v) when is_map(v), do: v
  defp to_opt_map(_), do: %{}

  def erl_changeset(
        {:near_transaction, tx_id, wallet_id, user_id, transaction_hash, tx_type,
         from_account_id, receiver_id, amount_near, contract_id, method_name, status,
         actions_count, error_message, created_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      tx_id: to_string(tx_id),
      wallet_id: to_string(wallet_id),
      user_id: to_string(user_id),
      transaction_hash: to_opt_string(transaction_hash),
      tx_type: to_opt_string(tx_type),
      from_account_id: to_opt_string(from_account_id),
      receiver_id: to_opt_string(receiver_id),
      amount_near: to_opt_float(amount_near),
      contract_id: to_opt_string(contract_id),
      method_name: to_opt_string(method_name),
      status: to_opt_string(status),
      actions_count: to_opt_int(actions_count),
      error_message: to_opt_string(error_message),
      created_at: created_at,
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Near transaction operation timed out")
    raise ArgumentError, "Near transaction operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Near transaction operation failed with reason: #{reason}")
    raise ArgumentError, "Near transaction operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Near transaction operation failed: #{inspect(reason)}")
    raise ArgumentError, "Near transaction operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("Near transaction operation returned nil")
    raise ArgumentError, "Transaction not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in NearTransaction.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid transaction data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:near_transaction, _tx_id, _wallet_id, _user_id, _transaction_hash, _tx_type,
         _from_account_id, _receiver_id, _amount_near, _contract_id, _method_name, _status,
         _actions_count, _error_message, _created_at, _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("Near transaction operation timed out")
    {:error, "Transaction operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("Near transaction operation failed: #{inspect(reason)}")
    {:error, "Transaction operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("Near transaction operation returned nil")
    {:error, "Transaction not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in NearTransaction.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid transaction data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
