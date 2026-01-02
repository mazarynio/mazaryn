defmodule Mazaryn.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      MazarynWeb.Telemetry,
      # Start the Ecto repository
      Mazaryn.Repo,
      # Start the PubSub system
      {Phoenix.PubSub, name: Mazaryn.PubSub},
      # Start the Endpoint (http/https)
      MazarynWeb.Endpoint,
      Mazaryn.HashtagIndexer,
      %{
        id: :db_supervisor,
        start: {:otpcode_sup, :start_link, []},
        restart: :permanent,
        shutdown: :infinity,
        type: :supervisor
      }
    ]

    :ets.new(:mazaryn_auth_table, [:set, :public, :named_table, read_concurrency: true])

    opts = [strategy: :one_for_one, name: Mazaryn.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def start_server do
    :user_server.start_link()
    :post_server.start_link()
    :chat_server.start_link()
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    MazarynWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
