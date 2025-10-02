import Config

config :mazaryn, MazarynWeb.Endpoint, cache_static_manifest: "priv/static/cache_manifest.json"

config :mazaryn, MazarynWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4000],
  url: [host: "mazaryn.io", port: 443],
  force_ssl: false

config :mazaryn, :email,
  send_emails: true

config :logger, level: :info
