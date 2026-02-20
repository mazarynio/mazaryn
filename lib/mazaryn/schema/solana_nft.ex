defmodule Schema.SolanaNFT do
  use Ecto.Schema
  import Ecto.Changeset
  require Logger

  embedded_schema do
    field(:nft_id, :string)
    field(:wallet_id, :string)
    field(:user_id, :string)
    field(:mint_address, :string)
    field(:token_account_address, :string)
    field(:name, :string)
    field(:symbol, :string)
    field(:uri, :string)
    field(:collection_address, :string)
    field(:verified, :boolean, default: false)
    field(:creators, {:array, :map}, default: [])
    field(:attributes, {:array, :map}, default: [])
    field(:image_url, :string)
    field(:created_at, :utc_datetime)
    field(:last_synced, :utc_datetime)
    field(:metadata, :map, default: %{})
  end

  defp to_opt_string(:undefined), do: nil
  defp to_opt_string(v), do: to_string(v)

  defp to_opt_map(:undefined), do: %{}
  defp to_opt_map(v) when is_map(v), do: v
  defp to_opt_map(_), do: %{}

  defp to_opt_list(:undefined), do: []
  defp to_opt_list(v) when is_list(v), do: v
  defp to_opt_list(_), do: []

  defp to_opt_datetime(:undefined), do: nil
  defp to_opt_datetime(v), do: v

  def erl_changeset(
        {:solana_nft, id, wallet_id, user_id, mint_address, token_account_address, name, symbol,
         uri, collection_address, verified, creators, attributes, image_url, created_at,
         last_synced, metadata}
      ) do
    %__MODULE__{}
    |> change(%{
      nft_id: id,
      wallet_id: wallet_id,
      user_id: user_id,
      mint_address: mint_address,
      token_account_address: token_account_address,
      name: to_opt_string(name),
      symbol: to_opt_string(symbol),
      uri: to_opt_string(uri),
      collection_address: to_opt_string(collection_address),
      verified:
        case verified do
          :undefined -> false
          v -> v
        end,
      creators: to_opt_list(creators),
      attributes: to_opt_list(attributes),
      image_url: to_opt_string(image_url),
      created_at: created_at,
      last_synced: to_opt_datetime(last_synced),
      metadata: to_opt_map(metadata)
    })
  end

  def erl_changeset({:error, :timeout}) do
    Logger.error("Solana NFT operation timed out")
    raise ArgumentError, "Solana NFT operation timed out. Please try again."
  end

  def erl_changeset({:error, reason}) when is_atom(reason) do
    Logger.error("Solana NFT operation failed: #{reason}")
    raise ArgumentError, "Solana NFT operation failed: #{reason}"
  end

  def erl_changeset({:error, reason}) do
    Logger.error("Solana NFT operation failed: #{inspect(reason)}")
    raise ArgumentError, "Solana NFT operation failed: #{inspect(reason)}"
  end

  def erl_changeset(nil) do
    raise ArgumentError, "NFT not found"
  end

  def erl_changeset(value) do
    Logger.error("Unexpected value in SolanaNFT.erl_changeset: #{inspect(value)}")
    raise ArgumentError, "Invalid NFT data format: #{inspect(value)}"
  end

  def erl_changeset_safe(
        {:solana_nft, _id, _wallet_id, _user_id, _mint_address, _token_account_address, _name,
         _symbol, _uri, _collection_address, _verified, _creators, _attributes, _image_url,
         _created_at, _last_synced, _metadata} = record
      ) do
    try do
      {:ok, erl_changeset(record)}
    rescue
      error -> {:error, Exception.message(error)}
    end
  end

  def erl_changeset_safe({:error, :timeout}) do
    {:error, "NFT operation timed out. Please try again."}
  end

  def erl_changeset_safe({:error, reason}) do
    {:error, "NFT operation failed: #{inspect(reason)}"}
  end

  def erl_changeset_safe(nil) do
    {:error, "NFT not found"}
  end

  def erl_changeset_safe(value) do
    Logger.error("Unexpected value in SolanaNFT.erl_changeset_safe: #{inspect(value)}")
    {:error, "Invalid NFT data format"}
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
