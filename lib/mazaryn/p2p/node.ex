defmodule Mazaryn.P2P.Node do
  @enforce_keys [
    :id,
    :host,
    :port
  ]

  defstruct id: nil,
            host: nil,
            port: nil

  def new(id, host, port) do
    %__MODULE__{
      id: id,
      host: host,
      port: port
    }
  end

  @doc """
  Generate twenty-byte node ID.
  """
  def generate_node_id do
    :crypto.strong_rand_bytes(20)
  end
end
