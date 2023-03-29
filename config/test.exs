import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :mazaryn, MazarynWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "4Z78iGzSF1KwfZJmy3pqyyUtSK7UGFS0ryUdlPsInhXO/Rj4KO/K5k8OaA8Ih/VM",
  server: false

# In test we don't send emails.
config :mazaryn, Mazaryn.Mailer, adapter: Swoosh.Adapters.Test

# Print only warnings and errors during test
config :logger, level: :warn

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime
