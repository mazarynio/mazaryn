defmodule Mazaryn do
  @moduledoc """
  Mazaryn keeps the contexts that define your domain
  and business logic.

  Contexts are also responsible for managing your data, regardless
  if it comes from the database, an external API or others.
  """

  require Logger

  @doc """
  Looks up `Application` config or raises if keyspace is not configured.
  ## Examples
      config :mazaryn, :media, [
        uploads_dir: Path.expand("../priv/uploads", __DIR__),
        host: [scheme: "http", host: "localhost", port: 4000],
      ]
      iex> Mazaryn.config([:media, :uploads_dir])
      iex> Mazaryn.config([:media, :host, :port])
  """
  def config([main_key | rest] = keyspace) when is_list(keyspace) do
    main = Application.fetch_env!(:mazaryn, main_key)

    Enum.reduce(rest, main, fn next_key, current ->
      case Keyword.fetch(current, next_key) do
        {:ok, val} -> val
        :error -> raise ArgumentError, "no config found under #{inspect(keyspace)}"
      end
    end)
  end
end
