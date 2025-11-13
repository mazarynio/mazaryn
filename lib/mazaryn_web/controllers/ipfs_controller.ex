defmodule MazarynWeb.IpfsController do
  use MazarynWeb, :controller
  require Logger

  def show(conn, %{"hash" => hash}) do
    hash_type = determine_hash_type(hash)

    gateway_url =
      case hash_type do
        :ipns -> "http://#{hash}.ipns.localhost:8080/"
        :ipfs -> "https://ipfs.io/ipfs/#{hash}"
        :unknown -> nil
      end

    render(conn, "show.html",
      hash: hash,
      hash_type: hash_type,
      gateway_url: gateway_url
    )
  end

  defp determine_hash_type(hash) do
    cond do
      String.starts_with?(hash, "k51") or String.starts_with?(hash, "k2") -> :ipns
      String.starts_with?(hash, "Qm") or String.starts_with?(hash, "bafy") -> :ipfs
      true -> :unknown
    end
  end
end
