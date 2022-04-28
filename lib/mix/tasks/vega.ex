defmodule Mix.Tasks.Vega do
  @moduledoc """
    mazaryn-front compilation and bundling for production.
  """
  use Mix.Task
  require Logger
  # Path for the frontend static assets that are being served
  # from our Phoenix router when accessing /app/* for the first time
  @public_path "./priv/static/vega"

  @shortdoc "Compile and bundle mazaryn-front for production"
  def run(_) do
    Logger.info("📦 - Installing NPM packages")
    System.cmd("npm", ["install", "--quiet"], cd: "./mazaryn-front")

    Logger.info("⚙️  - Compiling mazaryn-front")
    System.cmd("npm", ["run", "build"], cd: "./mazaryn-front")

    Logger.info("🚛 - Moving dist folder to Phoenix at #{@public_path}")
    # First clean up any stale files from previous builds if any
    System.cmd("rm", ["-rf", @public_path])
    System.cmd("cp", ["-R", "./mazaryn-front/dist", @public_path])

    Logger.info("⚛️  - mazaryn-front ready.")
  end
end
