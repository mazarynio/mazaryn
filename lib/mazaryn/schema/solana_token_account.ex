defmodule Schema.SolanaTokenAccount do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:token_account_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:token_account_address, :string)
    field(:token_mint, :string)
    field(:balance, :string)
    field(:decimals, :integer)
    field(:owner_address, :string)
    field(:created_at, :utc_datetime)
    field(:last_synced, :utc_datetime)
    field(:metadata, :map, default: %{})
  end

  defp to_opt_string(:undefined), do: nil
  defp to_opt_string(v) when is_binary(v), do: v
  defp to_opt_string(v), do: to_string(v)

  defp to_opt_int(:undefined), do: nil
  defp to_opt_int(v), do: v

  defp to_opt_map(:undefined), do: %{}
  defp to_opt_map(v) when is_map(v), do: v
  defp to_opt_map(_), do: %{}

  defp to_opt_datetime(:undefined), do: nil
  defp to_opt_datetime(v), do: v

  def erl_changeset(
        {:solana_token_account, id, wallet_id, user_id, token_account_address, token_mint,
         balance, decimals, owner_address, created_at, last_synced, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      token_account_id: id,
      wallet_id: wallet_id,
      user_id: user_id,
      token_account_address: token_account_address,
      token_mint: token_mint,
      balance: to_opt_string(balance),
      decimals: to_opt_int(decimals),
      owner_address: to_opt_string(owner_address),
      created_at: created_at,
      last_synced: to_opt_datetime(last_synced),
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Solana token account operation timed out")
    raise ArgumentError, "Solana token account operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Solana token account operation failed: #{reason}")
    raise ArgumentError, "Solana token account operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Solana token account operation failed: #{inspect(reason)}")
    raise ArgumentError, "Solana token account operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    raise ArgumentError, "Token account not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in SolanaTokenAccount.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid token account data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:solana_token_account, _id, _wallet_id, _user_id, _token_account_address, _token_mint,
         _balance, _decimals, _owner_address, _created_at, _last_synced, _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    {:error, "Token account operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    {:error, "Token account operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    {:error, "Token account not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in SolanaTokenAccount.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid token account data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
