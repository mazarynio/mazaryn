defmodule Schema.SolanaTransaction do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:tx_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:signature, :string)
    field(:tx_type, :string)
    field(:from_address, :string)
    field(:to_address, :string)
    field(:amount_lamports, :integer)
    field(:token_mint, :string)
    field(:nft_mint, :string)
    field(:status, :string)
    field(:fee_lamports, :integer)
    field(:slot, :integer)
    field(:block_time, :integer)
    field(:memo, :string)
    field(:error_message, :string)
    field(:created_at, :utc_datetime)
    field(:confirmed_at, :utc_datetime)
    field(:metadata, :map, default: %{})
  end

  defp to_opt_string(:undefined), do: nil
  defp to_opt_string(v), do: to_string(v)

  defp to_opt_int(:undefined), do: nil
  defp to_opt_int(v), do: v

  defp to_opt_map(:undefined), do: %{}
  defp to_opt_map(v) when is_map(v), do: v
  defp to_opt_map(_), do: %{}

  def erl_changeset({:solana_transaction, tx_id, wallet_id, user_id, signature, tx_type,
      from_address, to_address, amount_lamports, token_mint, nft_mint, status, fee_lamports,
      slot, block_time, memo, error_message, created_at, confirmed_at, metadata}) do
    %__MODULE__{}
    |> change(%{
      tx_id: tx_id,
      wallet_id: wallet_id,
      user_id: user_id,
      signature: to_opt_string(signature),
      tx_type: to_opt_string(tx_type),
      from_address: to_opt_string(from_address),
      to_address: to_opt_string(to_address),
      amount_lamports: to_opt_int(amount_lamports),
      token_mint: to_opt_string(token_mint),
      nft_mint: to_opt_string(nft_mint),
      status: to_opt_string(status),
      fee_lamports: to_opt_int(fee_lamports),
      slot: to_opt_int(slot),
      block_time: to_opt_int(block_time),
      memo: to_opt_string(memo),
      error_message: to_opt_string(error_message),
      created_at: created_at,
      confirmed_at: (case confirmed_at do :undefined -> nil; v -> v end),
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Solana transaction operation timed out")
    raise ArgumentError, "Solana transaction operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Solana transaction operation failed: #{reason}")
    raise ArgumentError, "Solana transaction operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Solana transaction operation failed: #{inspect(reason)}")
    raise ArgumentError, "Solana transaction operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    raise ArgumentError, "Transaction not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in SolanaTransaction.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid transaction data format: #{inspect(value)}"
  end

  def erl_changeset_safe({:solana_transaction, _tx_id, _wallet_id, _user_id, _signature,
      _tx_type, _from_address, _to_address, _amount_lamports, _token_mint, _nft_mint,
      _status, _fee_lamports, _slot, _block_time, _memo, _error_message, _created_at,
      _confirmed_at, _metadata} = record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    {:error, "Transaction operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    {:error, "Transaction operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    {:error, "Transaction not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in SolanaTransaction.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid transaction data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
