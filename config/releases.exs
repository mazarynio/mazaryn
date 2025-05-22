import Config

if System.get_env("DOCKER_DEPLOYMENT") do
  config :mazaryn, Mazaryn.Repo,
    url: "ecto://${System.get_env("DB_USER")}:${System.get_env("DB_PASSWORD")}@${System.get_env("DB_HOST")}/mazaryn_prod",
    pool_size: String.to_integer(System.get_env("POOL_SIZE") || "15")

  config :mazaryn, MazarynWeb.Endpoint,
    http: [ip: {0, 0, 0, 0}],
    url: [host: System.get_env("PHX_HOST")]

  config :mazaryn, :media,
    uploads_dir: System.get_env("UPLOADS_DIR") || "/app/bin/uploads"
end
