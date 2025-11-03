import Config

if System.get_env("PHX_SERVER") do
  config :mazaryn, MazarynWeb.Endpoint, server: true
end

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise """
      environment variable DATABASE_URL is missing.
      For example: ecto://USER:PASS@HOST/DATABASE
      """

  maybe_ipv6 = if System.get_env("ECTO_IPV6") in ~w(true 1), do: [:inet6], else: []

  config :mazaryn, Mazaryn.Repo,
    url: database_url,
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "10"),
    socket_options: maybe_ipv6

  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  host = System.get_env("PHX_HOST") || "mazaryn.io"
  port = String.to_integer(System.get_env("PORT") || "4000")

  config :mazaryn, MazarynWeb.Endpoint,
    url: [host: host, port: 443, scheme: "https"],
    http: [
      ip: {127, 0, 0, 1},
      port: port
    ],
    secret_key_base: secret_key_base

  config :mazaryn, :weather_api,
    api_key: System.get_env("OPENWEATHER_API_KEY"),
    base_url: "https://api.openweathermap.org/data/2.5",
    geocoding_url: "https://api.openweathermap.org/geo/1.0"

  config :mazaryn, :email, send_emails: host == "mazaryn.io"

  config :mazaryn, :media,
    uploads_dir: "/home/zaryn/mazaryn/priv/static/uploads",
    host: [scheme: "https", host: host, port: 443]

  config :mazaryn, Mazaryn.Mailer,
    adapter: Resend.Swoosh.Adapter,
    api_key: System.get_env("RESEND_API_KEY")

  config :swoosh, :api_client, Swoosh.ApiClient.Hackney

  config :ex_openai,
    api_key: System.get_env("OPENAI_KEY"),
    organization_key: System.get_env("OPENAI_ORGANIZATION_KEY"),
    http_options: [recv_timeout: 50_000]
end
