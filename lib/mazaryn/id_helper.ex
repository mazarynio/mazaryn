defmodule Mazaryn.IdHelper do
  @spec normalize_for_erlang(any()) :: charlist()
  def normalize_for_erlang(id) when is_binary(id) do
    case safe_to_charlist(id) do
      {:ok, charlist} -> charlist
      {:error, _} -> encode_id_for_erlang(id)
    end
  end

  def normalize_for_erlang(id) when is_list(id) do
    id
  end

  def normalize_for_erlang(id) do
    id
    |> to_string()
    |> normalize_for_erlang()
  end

  @spec encode_id_for_erlang(binary()) :: charlist()
  def encode_id_for_erlang(id) when is_binary(id) do
    ("id:" <> id)
    |> Base.url_encode64()
    |> String.replace("=", "")
    |> String.to_charlist()
  end

  @spec decode_from_erlang(charlist()) :: binary()
  def decode_from_erlang(id) when is_list(id) do
    id_str = List.to_string(id)

    if String.starts_with?(id_str, "id:") do
      id_str
      |> String.trim_leading("id:")
      |> Base.url_decode64!()
    else
      id_str
    end
  end

  @spec safe_to_charlist(binary()) :: {:ok, charlist()} | {:error, atom()}
  def safe_to_charlist(binary) do
    try do
      {:ok, String.to_charlist(binary)}
    rescue
      ArgumentError -> {:error, :badarg}
    end
  end

  @spec sanitize_filename(binary()) :: binary()
  def sanitize_filename(filename) do
    filename
    |> String.replace(~r/[^\w\.\-]/, "_")
    |> String.trim()
  end
end
