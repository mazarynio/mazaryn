defmodule Mazaryn.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      MazarynWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Mazaryn.PubSub},
      # Start the Endpoint (http/https)
      MazarynWeb.Endpoint,

      # Start a worker by calling: Mazaryn.Worker.start_link(arg)
      # {Mazaryn.Worker, arg}

      # start erlang code to store user activities
      %{
        id: :db_supervisor,
        start: {:otpcode_sup, :start_link, []},
        restart: :permanent,
        shutdown: :infinity,
        type: :supervisor
      }
    ]

    :ets.new(:mazaryn_auth_table, [:set, :public, :named_table, read_concurrency: true])
    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options

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
