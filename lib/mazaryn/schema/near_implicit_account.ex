defmodule Schema.NearImplicitAccount do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:implicit_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:account_id, :string)
    field(:encrypted_private_key, :string)
    field(:encryption_iv, :string)
    field(:encryption_tag, :string)
    field(:funded, :boolean, default: false)
    field(:created_at, :utc_datetime)
    field(:funded_at, :utc_datetime)
    field(:metadata, :map, default: %{})
  end

  defp to_opt_string(:undefined), do: nil
  defp to_opt_string(v), do: to_string(v)

  defp to_opt_datetime(:undefined), do: nil
  defp to_opt_datetime(v), do: v

  defp to_opt_map(:undefined), do: %{}
  defp to_opt_map(v) when is_map(v), do: v
  defp to_opt_map(_), do: %{}

  defp to_opt_bool(:undefined), do: false
  defp to_opt_bool(v) when is_boolean(v), do: v
  defp to_opt_bool(_), do: false

  def erl_changeset(
        {:near_implicit_account, implicit_id, wallet_id, user_id, account_id,
         encrypted_private_key, encryption_iv, encryption_tag, funded, created_at, funded_at,
         metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      implicit_id: to_string(implicit_id),
      wallet_id: to_string(wallet_id),
      user_id: to_string(user_id),
      account_id: to_string(account_id),
      encrypted_private_key: to_opt_string(encrypted_private_key),
      encryption_iv: to_opt_string(encryption_iv),
      encryption_tag: to_opt_string(encryption_tag),
      funded: to_opt_bool(funded),
      created_at: created_at,
      funded_at: to_opt_datetime(funded_at),
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Near implicit account operation timed out")
    raise ArgumentError, "Near implicit account operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Near implicit account operation failed with reason: #{reason}")
    raise ArgumentError, "Near implicit account operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Near implicit account operation failed: #{inspect(reason)}")
    raise ArgumentError, "Near implicit account operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("Near implicit account operation returned nil")
    raise ArgumentError, "Implicit account not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in NearImplicitAccount.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid implicit account data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:near_implicit_account, _implicit_id, _wallet_id, _user_id, _account_id,
         _encrypted_private_key, _encryption_iv, _encryption_tag, _funded, _created_at,
         _funded_at, _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("Near implicit account operation timed out")
    {:error, "Implicit account operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("Near implicit account operation failed: #{inspect(reason)}")
    {:error, "Implicit account operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("Near implicit account operation returned nil")
    {:error, "Implicit account not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in NearImplicitAccount.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid implicit account data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
