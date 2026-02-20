defmodule Schema.SolanaAirdrop do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:airdrop_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:type, :string)
    field(:token_mint, :string)
    field(:nft_collection, :string)
    field(:total_recipients, :integer)
    field(:successful, :integer, default: 0)
    field(:failed, :integer, default: 0)
    field(:total_amount_lamports, :integer)
    field(:status, :string)
    field(:created_at, :utc_datetime)
    field(:started_at, :utc_datetime)
    field(:completed_at, :utc_datetime)
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

  def erl_changeset({:solana_airdrop, airdrop_id, wallet_id, user_id, type, token_mint,
      nft_collection, total_recipients, successful, failed, total_amount_lamports, status,
      created_at, started_at, completed_at, metadata}) do
    %__MODULE__{}
    |> change(%{
      airdrop_id: airdrop_id,
      wallet_id: wallet_id,
      user_id: user_id,
      type: to_opt_string(type),
      token_mint: to_opt_string(token_mint),
      nft_collection: to_opt_string(nft_collection),
      total_recipients: to_opt_int(total_recipients),
      successful: (case successful do :undefined -> 0; v -> v end),
      failed: (case failed do :undefined -> 0; v -> v end),
      total_amount_lamports: to_opt_int(total_amount_lamports),
      status: to_opt_string(status),
      created_at: created_at,
      started_at: to_opt_datetime(started_at),
      completed_at: to_opt_datetime(completed_at),
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Solana airdrop operation timed out")
    raise ArgumentError, "Solana airdrop operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Solana airdrop operation failed: #{reason}")
    raise ArgumentError, "Solana airdrop operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Solana airdrop operation failed: #{inspect(reason)}")
    raise ArgumentError, "Solana airdrop operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    raise ArgumentError, "Airdrop not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in SolanaAirdrop.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid airdrop data format: #{inspect(value)}"
  end

  def erl_changeset_safe({:solana_airdrop, _airdrop_id, _wallet_id, _user_id, _type,
      _token_mint, _nft_collection, _total_recipients, _successful, _failed,
      _total_amount_lamports, _status, _created_at, _started_at, _completed_at,
      _metadata} = record) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    {:error, "Airdrop operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    {:error, "Airdrop operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    {:error, "Airdrop not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in SolanaAirdrop.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid airdrop data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
