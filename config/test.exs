import Config


config :mazaryn, Mazaryn.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "mazaryn_test#{System.get_env("MIX_TEST_PARTITION")}",
  pool: Ecto.Adapters.SQL.Sandbox,
  pool_size: 10


config :mazaryn, MazarynWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "vqtsbLoeGWQW0exzthxM8fvX/4iBBOgrtSONN1UN72LfEnRQHgNBJmCLEB0f51oW",
  server: false


config :mazaryn, Mazaryn.Mailer, adapter: Swoosh.Adapters.Test


config :logger, level: :warn
config :logger, level: :warning

config :phoenix, :plug_init_mode, :runtime
