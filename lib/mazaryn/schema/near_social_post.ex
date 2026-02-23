defmodule Schema.NearSocialPost do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:post_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:account_id, :string)
    field(:contract, :string)
    field(:text, :string)
    field(:media_urls, {:array, :string}, default: [])
    field(:tags, {:array, :string}, default: [])
    field(:transaction_hash, :string)
    field(:block_height, :integer)
    field(:created_at, :utc_datetime)
    field(:metadata, :map, default: %{})
  end

  defp to_opt_string(:undefined), do: nil
  defp to_opt_string(v), do: to_string(v)

  defp to_opt_int(:undefined), do: nil
  defp to_opt_int(v) when is_integer(v), do: v
  defp to_opt_int(_), do: nil

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
        {:near_social_post, post_id, wallet_id, user_id, account_id, contract, text,
         media_urls, tags, transaction_hash, block_height, created_at, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      post_id: to_string(post_id),
      wallet_id: to_string(wallet_id),
      user_id: to_string(user_id),
      account_id: to_string(account_id),
      contract: to_opt_string(contract),
      text: to_opt_string(text),
      media_urls: to_string_list(media_urls),
      tags: to_string_list(tags),
      transaction_hash: to_opt_string(transaction_hash),
      block_height: to_opt_int(block_height),
      created_at: created_at,
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Near social post operation timed out")
    raise ArgumentError, "Near social post operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Near social post operation failed with reason: #{reason}")
    raise ArgumentError, "Near social post operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Near social post operation failed: #{inspect(reason)}")
    raise ArgumentError, "Near social post operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    Logger.error("Near social post operation returned nil")
    raise ArgumentError, "Social post not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in NearSocialPost.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid social post data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:near_social_post, _post_id, _wallet_id, _user_id, _account_id, _contract, _text,
         _media_urls, _tags, _transaction_hash, _block_height, _created_at,
         _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    Logger.warning("Near social post operation timed out")
    {:error, "Social post operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    Logger.error("Near social post operation failed: #{inspect(reason)}")
    {:error, "Social post operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    Logger.warning("Near social post operation returned nil")
    {:error, "Social post not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in NearSocialPost.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid social post data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
