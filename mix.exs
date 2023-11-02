defmodule Mazaryn.MixProject do
  use Mix.Project

  def project do
    [
      app: :mazaryn,
      version: "0.1.0",
      elixir: "~> 1.15.5",
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
      {:phoenix, "~> 1.7.7"},
      {:phoenix_ecto, "~> 4.4"},
      {:phoenix_view, "~> 2.0"},
      {:ecto_sql, "~> 3.10"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_html, "~> 3.3"},
      {:phoenix_live_reload, "~> 1.4", only: :dev},
      {:phoenix_live_view, "~> 0.19"},
      {:floki, ">= 0.33.1", only: :test},
      {:phoenix_live_dashboard, "~> 0.8"},
      {:esbuild, "~> 0.7", runtime: Mix.env() == :dev},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.22"},
      {:jason, "~> 1.4"},
      {:plug_cowboy, "~> 2.6"},
      {:joken, "~> 2.6"},
      {:tarams, "~> 1.7"},
      {:tailwind, "~> 0.2", runtime: Mix.env() == :dev},
      {:swoosh, "~> 1.11"},
      {:phoenix_swoosh, "~> 1.2"},
      {:timex, "~> 3.7"},

      # Erlang dependencies
      {:erlpass, git: "https://github.com/ferd/erlpass.git"},
      {:bcrypt, git: "https://github.com/erlangpack/bcrypt.git", override: true},
      {:jiffy, git: "https://github.com/davisp/jiffy.git"},
      {:nanoid, git: "https://github.com/zaryntech/nanoid.git"},
      {:epgsql, "~> 4.7"},
      {:erl_base58, "~> 0.0.1"},

      # Icons
      {:ex_heroicons, "~> 2.0"},

      # Graphql APIs
      {:absinthe, "~> 1.7.5"},
      {:absinthe_plug, "~> 1.5.8"},
      {:rustler, "~> 0.29"},

      # Machine Learning
      {:nx, "~> 0.5"},
      {:earmark, "~> 1.4"},

      # OpenAI
      {:ex_openai, "~> 1.2.1"}
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
      setup: ["deps.get", "ecto.setup", "assets.setup", "assets.build"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
      "assets.setup": ["tailwind.install --if-missing", "esbuild.install --if-missing"],
      "assets.build": ["tailwind default", "esbuild default"],
      "assets.deploy": ["tailwind default --minify", "esbuild default --minify", "phx.digest"]
    ]
  end
end
