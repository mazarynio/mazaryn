import Config

if Mix.env() in [:dev, :test] do
  env_file = Path.join([File.cwd!(), ".env"])

  if File.exists?(env_file) do
    File.read!(env_file)
    |> String.split("\n", trim: true)
    |> Enum.reject(fn line ->
      String.starts_with?(String.trim(line), "#") or String.trim(line) == ""
    end)
    |> Enum.each(fn line ->
      case String.split(line, "=", parts: 2) do
        [key, value] ->
          key = String.trim(key)
          value = String.trim(value) |> String.trim("\"") |> String.trim("'")
          System.put_env(key, value)

        _ ->
          :ok
      end
    end)
  end
end

config :mime, :types, %{
  "application/x-hdf" => ["h5", "hdf5"],
  "application/x-pytorch" => ["pt", "pth"],
  "application/x-tensorflow" => ["pb"],
  "application/x-onnx" => ["onnx"],
  "application/x-pickle" => ["pkl", "pickle"],
  "application/x-joblib" => ["joblib"],
  "application/zip" => ["zip"],
  "video/x-matroska" => ["mkv"],
  "video/x-msvideo" => ["avi"]
}

config :logger, level: :warning

config :mazaryn,
  ecto_repos: [Mazaryn.Repo],
  env: config_env()

config :mazaryn, :weather_api,
  base_url: "https://api.openweathermap.org/data/2.5",
  geocoding_url: "https://api.openweathermap.org/geo/1.0"

config :mazaryn, :email, send_emails: false

config :mazaryn, MazarynWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [
    formats: [html: MazarynWeb.ErrorHTML, json: MazarynWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Mazaryn.PubSub,
  live_view: [signing_salt: "un3aCFjC"]

config :swoosh, :api_client, false
config :swoosh, :local, true

config :mazaryn, MazarynWeb.Gettext,
  locales: ~w(en ru fa),
  default_locale: "en"

config :esbuild,
  version: "0.17.11",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

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

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

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

config :mazaryn, Mazaryn.Mailer, adapter: Swoosh.Adapters.Local

import_config "#{config_env()}.exs"
