defmodule Mazaryn.KernelDiagnostic do
  require Logger

  def check_system do
    Logger.info("=== Kernel System Diagnostic ===")

    check_ets_table()
    check_python()
    check_julia()
    check_elixir()
    check_rust()

    Logger.info("=== Diagnostic Complete ===")
  end

  defp check_ets_table do
    case :ets.info(:kernel_sessions) do
      :undefined ->
        Logger.error("❌ ETS table :kernel_sessions does not exist")
      info ->
        Logger.info("✅ ETS table exists: #{inspect(Keyword.take(info, [:size, :memory]))}")
    end
  end

  defp check_python do
    case System.cmd("python3", ["--version"], stderr_to_stdout: true) do
      {output, 0} ->
        Logger.info("✅ Python3 available: #{String.trim(output)}")
      {output, code} ->
        Logger.error("❌ Python3 failed (exit #{code}): #{output}")
    end
  rescue
    error ->
      Logger.error("❌ Python3 not found: #{inspect(error)}")
  end

  defp check_julia do
    case System.cmd("julia", ["--version"], stderr_to_stdout: true) do
      {output, 0} ->
        Logger.info("✅ Julia available: #{String.trim(output)}")
      {output, code} ->
        Logger.warning("⚠️  Julia not available (exit #{code}): #{output}")
    end
  rescue
    error ->
      Logger.warning("⚠️  Julia not found (optional): #{inspect(error)}")
  end

  defp check_elixir do
    Logger.info("✅ Elixir available: #{System.version()}")
  end

  defp check_rust do
    case System.cmd("rustc", ["--version"], stderr_to_stdout: true) do
      {output, 0} ->
        Logger.info("✅ Rust available: #{String.trim(output)}")
      {output, code} ->
        Logger.warning("⚠️  Rust not available (exit #{code}): #{output}")
    end
  rescue
    error ->
      Logger.warning("⚠️  Rust not found (optional): #{inspect(error)}")
  end
end
