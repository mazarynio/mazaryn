defmodule Mazaryn.Application do
  use Application

  require Logger

  @impl true
  def start(_type, _args) do
    initialize_ets_tables()
    initialize_kernel_system()

    children = [
      MazarynWeb.Telemetry,
      Mazaryn.Repo,
      {Phoenix.PubSub, name: Mazaryn.PubSub},
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

    opts = [strategy: :one_for_one, name: Mazaryn.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def start_server do
    :user_server.start_link()
    :post_server.start_link()
    :chat_server.start_link()
  end

  @impl true
  def config_change(changed, _new, removed) do
    MazarynWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp initialize_ets_tables do
    Logger.info("Initializing ETS tables...")

    case :ets.info(:mazaryn_auth_table) do
      :undefined ->
        :ets.new(:mazaryn_auth_table, [:set, :public, :named_table, read_concurrency: true])
        Logger.info("✅ Auth table created")
      _ ->
        Logger.info("Auth table already exists")
    end
  rescue
    error ->
      Logger.error("Failed to initialize auth table: #{inspect(error)}")
      :ok
  end

  defp initialize_kernel_system do
    Logger.info("🚀 Initializing Notebook Kernel System...")

    case :ets.info(:kernel_sessions) do
      :undefined ->
        Logger.info("📊 Creating kernel_sessions ETS table...")

        :ets.new(:kernel_sessions, [
          :named_table,
          :public,
          :set,
          {:read_concurrency, true},
          {:write_concurrency, true}
        ])

        Logger.info("✅ Kernel sessions table created successfully")
        :ok

      _info ->
        Logger.info("✅ Kernel sessions table already exists")
        :ok
    end
  rescue
    error ->
      Logger.error("❌ Failed to initialize kernel system: #{inspect(error)}")
      Logger.error("Stacktrace: #{inspect(__STACKTRACE__)}")
      :ok
  end
end
