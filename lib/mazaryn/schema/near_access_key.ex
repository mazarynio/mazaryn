defmodule Schema.NearAccessKey do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:key_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:public_key, :string)
    field(:key_type, :string)
    field(:contract_id, :string)
    field(:method_names, {:array, :string}, default: [])
    field(:allowance_near, :float)
    field(:label, :string)
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

  defp to_opt_map(:undefined), do: %{}
  defp to_opt_map(v) when is_map(v), do: v
  defp to_opt_map(_), do: %{}

  defp to_string_list(:undefined), do: []
  defp to_string_list([]), do: []
  defp to_string_list(list) when is_list(list) do
    Enum.map(list, fn
      v when is_list(v) -> List.to_string(v)
      v when is_binary(v) -> v
      v when is_atom(v) -> Atom.to_string(v)
      v -> to_string(v)
    end)
  end
  defp to_string_list(_), do: []

  def erl_changeset(
        {:near_access_key, key_id, wallet_id, user_id, public_key, key_type, contract_id,
         method_names, allowance_near, label, created_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      key_id: to_string(key_id),
      wallet_id: to_string(wallet_id),
      user_id: to_string(user_id),
      public_key: to_string(public_key),
      key_type: to_opt_string(key_type),
      contract_id: to_opt_string(contract_id),
      method_names: to_string_list(method_names),
      allowance_near: to_opt_float(allowance_near),
      label: to_opt_string(label),
      created_at: created_at,
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Near access key operation timed out")
    raise ArgumentError, "Near access key operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Near access key operation failed with reason: #{reason}")
    raise ArgumentError, "Near access key operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Near access key operation failed: #{inspect(reason)}")
    raise ArgumentError, "Near access key operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("Near access key operation returned nil")
    raise ArgumentError, "Access key not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in NearAccessKey.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid access key data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:near_access_key, _key_id, _wallet_id, _user_id, _public_key, _key_type, _contract_id,
         _method_names, _allowance_near, _label, _created_at, _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("Near access key operation timed out")
    {:error, "Access key operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("Near access key operation failed: #{inspect(reason)}")
    {:error, "Access key operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("Near access key operation returned nil")
    {:error, "Access key not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in NearAccessKey.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid access key data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
