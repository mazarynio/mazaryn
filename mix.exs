defmodule Mazaryn.MixProject do
  use Mix.Project

  def project do
    [
      app: :mazaryn,
      version: "0.1.0",
      elixir: "~> 1.13",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {Mazaryn.Application, []},
      extra_applications: [:logger, :runtime_tools, :absinthe_plug],
      included_applications: [:mnesia]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.6.12"},
      {:phoenix_ecto, "~> 4.4"},
      {:ecto_sql, "~> 3.8"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_html, "~> 3.2"},
      {:phoenix_live_reload, "~> 1.3", only: :dev},
      {:phoenix_live_view, "~> 0.17.11"},
      {:floki, ">= 0.33.1", only: :test},
      {:phoenix_live_dashboard, "~> 0.6"},
      {:esbuild, "~> 0.5", runtime: Mix.env() == :dev},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.20"},
      {:jason, "~> 1.4"},
      {:plug_cowboy, "~> 2.5"},
      {:joken, "~> 2.5"},
      {:tarams, "~> 1.6"},
      {:tailwind, "~> 0.1", runtime: Mix.env() == :dev},
      {:swoosh, "~> 1.8"},
      {:phoenix_swoosh, "~> 1.0"},
      {:timex, "~> 3.7"},

      # Erlang dependencies
      {:erlpass, git: "https://github.com/ferd/erlpass.git"},
      {:bcrypt, git: "https://github.com/erlangpack/bcrypt.git", override: true},
      {:jiffy, git: "https://github.com/davisp/jiffy.git"},
      {:epgsql, "~> 4.6"},
      {:erl_base58, "~> 0.0.1"},

      # Icons
      {:ex_heroicons, "~> 2.0"},

      # Graphql APIs
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"},
      {:rustler, "~> 0.26.0"},

      # exceptions monitoring
      {:honeybadger, "~> 0.16"}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      setup: ["deps.get", "ecto.setup"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      "ecto.seed": ["run priv/repo/seeds.exs"],
      test: ["ecto.create --quiet", "ecto.seed", "test"],
      "assets.deploy": ["tailwind default --minify", "esbuild default --minify", "phx.digest"]
    ]
  end
end
