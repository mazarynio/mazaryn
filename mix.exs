defmodule Mazaryn.MixProject do
  use Mix.Project

  def project do
    [
      app: :mazaryn,
      version: "0.1.0",
      elixir: "~> 1.19.1",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  def application do
    [
      mod: {Mazaryn.Application, []},
      extra_applications: [:logger, :runtime_tools, :absinthe_plug],
      included_applications: [:mnesia]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:phoenix, "~> 1.8"},
      {:phoenix_ecto, "~> 4.6"},
      {:phoenix_view, "~> 2.0"},
      {:ecto_sql, "~> 3.10"},
      {:postgrex, "~> 0.21.1"},
      {:phoenix_html, "~> 4.3"},
      {:phoenix_html_helpers, "~> 1.0"},
      {:phoenix_live_reload, "~> 1.6", only: :dev},
      {:phoenix_live_view, "~> 1.1"},
      {:floki, ">= 0.33.1", only: :test},
      {:phoenix_live_dashboard, "~> 0.8"},
      {:esbuild, "~> 0.7", runtime: Mix.env() == :dev},
      {:telemetry_metrics, "~> 0.6"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.26"},
      {:jason, "~> 1.4"},
      {:plug_cowboy, "~> 2.6"},
      {:joken, "~> 2.6"},
      {:tarams, "~> 1.7"},
      {:tailwind, "~> 0.4", runtime: Mix.env() == :dev},
      {:swoosh, "~> 1.11"},
      {:phoenix_swoosh, "~> 1.2"},
      {:timex, "~> 3.7"},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:httpoison, "~> 2.0"},
      {:hackney, "~> 1.20"},

      # Erlang dependencies
      {:erlpass, git: "https://github.com/ferd/erlpass.git"},
      {:bcrypt, git: "https://github.com/erlangpack/bcrypt.git", override: true},
      {:jiffy, git: "https://github.com/davisp/jiffy.git"},
      {:jsx, git: "https://github.com/talentdeficit/jsx.git"},
      {:nanoid, git: "https://github.com/zaryntech/nanoid.git"},
      {:key_guardian, git: "https://github.com/zaryntech/key-guardian.git"},
      {:epgsql, "~> 4.7"},
      {:erl_base58, "~> 0.0.1"},

      # Icons
      {:heroicons, "~> 0.5.6"},

      # Graphql APIs
      {:absinthe, "~> 1.7.5"},
      {:absinthe_plug, "~> 1.5.8"},
      {:rustler, "~> 0.29"},

      # Machine Learning
      {:nx, "~> 0.9", only: :prod},
      {:earmark, "~> 1.4"},
      {:axon, "~> 0.6", only: :prod},

      # OpenAI
      {:ex_openai, "~> 1.2.1"},

      # Test
      {:ex_machina, "~> 2.8"},
      ## {:wallaby, "~> 0.30.10"},
      {:mock, "~> 0.3.9"},
      {:mox, "~> 1.2"},
      {:resend, "~> 0.4.4"}
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "ecto.setup", "assets.setup", "assets.build"],
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.create --quiet", "ecto.migrate --quiet", "test"],
      "assets.setup": [
        "cmd npm install --prefix assets",
        "tailwind.install --if-missing",
        "esbuild.install --if-missing"
      ],
      "assets.build": ["tailwind default", "esbuild default"],
      "assets.deploy": ["tailwind default --minify", "esbuild default --minify", "phx.digest"]
    ]
  end
end
