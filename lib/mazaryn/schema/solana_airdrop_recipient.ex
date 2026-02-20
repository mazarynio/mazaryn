defmodule Schema.SolanaAirdropRecipient do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:recipient_id, :string)
    field(:airdrop_id, :string)
    field(:recipient_address, :string)
    field(:amount_lamports, :integer)
    field(:mint_address, :string)
    field(:success, :boolean, default: false)
    field(:signature, :string)
    field(:error_message, :string)
    field(:processed_at, :utc_datetime)
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

  def erl_changeset({:solana_airdrop_recipient, id, airdrop_id, recipient_address,
      amount_lamports, mint_address, success, signature, error_message, processed_at,
      metadata}) do
    %__MODULE__{}
    |> change(%{
      recipient_id: id,
      airdrop_id: airdrop_id,
      recipient_address: recipient_address,
      amount_lamports: to_opt_int(amount_lamports),
      mint_address: to_opt_string(mint_address),
      success: (case success do :undefined -> false; v -> v end),
      signature: to_opt_string(signature),
      error_message: to_opt_string(error_message),
      processed_at: to_opt_datetime(processed_at),
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Solana airdrop recipient operation timed out")
    raise ArgumentError, "Solana airdrop recipient operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Solana airdrop recipient operation failed: #{reason}")
    raise ArgumentError, "Solana airdrop recipient operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Solana airdrop recipient operation failed: #{inspect(reason)}")
    raise ArgumentError, "Solana airdrop recipient operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    raise ArgumentError, "Airdrop recipient not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in SolanaAirdropRecipient.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid airdrop recipient data format: #{inspect(value)}"
  end

  def erl_changeset_safe({:solana_airdrop_recipient, _id, _airdrop_id, _recipient_address,
      _amount_lamports, _mint_address, _success, _signature, _error_message, _processed_at,
      _metadata} = record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    {:error, "Airdrop recipient operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    {:error, "Airdrop recipient operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    {:error, "Airdrop recipient not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in SolanaAirdropRecipient.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid airdrop recipient data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
