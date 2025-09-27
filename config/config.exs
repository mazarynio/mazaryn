import Config

if Mix.env() in [:dev, :test] do
  env_files = [
    ".env",
    "mazaryn/.env",
    Path.join([File.cwd!(), ".env"]),
    Path.join([File.cwd!(), "mazaryn", ".env"])
  ]

  Enum.each(env_files, fn env_file ->
    if File.exists?(env_file) do
      DotenvParser.load_file(env_file)
    end
  end)
end

config :logger, level: :warning

config :ex_heroicons, type: "solid"

config :mazaryn,
  ecto_repos: [Mazaryn.Repo],
  env: config_env()

config :mazaryn, :weather_api,
  api_key: System.get_env("OPENWEATHER_API_KEY"),
  base_url: "https://api.openweathermap.org/data/2.5",
  geocoding_url: "https://api.openweathermap.org/geo/1.0"

# Configures the endpoint
config :mazaryn, MazarynWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [
    formats: [html: MazarynWeb.ErrorHTML, json: MazarynWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Mazaryn.PubSub,
  live_view: [signing_salt: "un3aCFjC"]

# Configures the mailer
#
# By default it uses the "Local" adapter which stores the emails
# locally. You can see the emails in your browser, at "/dev/mailbox".
#
# For production it's recommended to configure a different adapter
# at the config/runtime.exs.

# Swoosh API client is needed for adapters other than SMTP.
config :swoosh, :api_client, false
config :swoosh, :local, true

config :mazaryn, MazarynWeb.Gettext,
  locales: ~w(en ru fa),
  default_locale: "en"

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.17.11",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

# Configure Tailwind (the version is required)
config :tailwind,
  version: "3.2.7",
  default: [
    args: ~w(
      --config=tailwind.config.js
      --input=css/app.css
      --output=../priv/static/assets/app.css
    ),
    cd: Path.expand("../assets", __DIR__)
  ]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

config :mnesia, dir: ~c"Mnesia/"

config :mazaryn, Mazaryn.Gettext,
  default_locale: "en",
  locales: [
    "en",
    "fa",
    "ru",
    "zh",
    "az",
    "ar",
    "es",
    "vi",
    "hi",
    "de",
    "fr",
    "it",
    "pt",
    "fil",
    "id"
  ]

config :mazaryn, :media,
  uploads_dir: Path.join([File.cwd!(), "priv", "static", "uploads"]),
  ipfs_gateway: "http://127.0.0.1:8080/ipfs/"

config :mazaryn, MazarynWeb.Endpoint,
  static_paths: ~w(assets fonts images favicon.ico robots.txt uploads)

config :mazaryn, Mazaryn.Mailer,
  adapter: Swoosh.Adapters.Mailjet,
  api_key: System.get_env("MAILJET_API_KEY"),
  secret: System.get_env("MAILJET_SECRET_KEY")

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
