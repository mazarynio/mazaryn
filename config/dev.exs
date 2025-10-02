import Config

config :mazaryn, Mazaryn.Repo,
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  database: "postgres",
  stacktrace: true,
  show_sensitive_data_on_connection_error: true,
  pool_size: 10

config :mazaryn, :media,
  uploads_dir: Path.expand("../uploads", __DIR__),
  host: [scheme: "http", host: "localhost", port: 4000]

config :mazaryn, :email,
  send_emails: false

config :mazaryn, MazarynWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4000],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "bojtbQHzqjb95b/X29OIGdE6lWBYc4ndgpjlPzl+l7rYEb4StXvin8n2uqdThKc0",
  watchers: [
    esbuild: {Esbuild, :install_and_run, [:default, ~w(--sourcemap=inline --watch)]},
    tailwind: {Tailwind, :install_and_run, [:default, ~w(--watch)]}
  ]

config :mazaryn, MazarynWeb.Endpoint,
  live_reload: [
    patterns: [
      ~r"priv/static/[?!uploads].*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"priv/gettext/.*(po)$",
      ~r"lib/mazaryn_web/(controllers|live|components)/.*(ex|heex)$"
    ]
  ]

config :mazaryn, dev_routes: true

config :logger, :console, format: "[$level] $message\n"

config :phoenix, :stacktrace_depth, 20

config :phoenix, :plug_init_mode, :runtime

config :swoosh, :api_client, false

config :mazaryn, Mazaryn.Mailer,
  adapter: Swoosh.Adapters.Local

config :joken, default_signer: "secret"
