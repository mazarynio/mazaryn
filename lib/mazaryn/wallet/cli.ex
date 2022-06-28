defmodule Wallet.Cli do

  import IO
  import IO.ANSI

  alias IO.ANSI.Docs

  def info(message), do: [:normal, message] |> format |> puts

  def debug(message), do: [:faint, message] |> format |> puts

  def success(message), do: [:green, message] |> format |> puts

  def warn(message), do: [:yellow, message] |> format |> puts

  def error(message, device \\ :stderr) do
    formatted = format([:red, message])
    IO.puts(device, formatted)
  end

  def color(messages), do: messages |> format |> puts

  def heading(message), do: Docs.print_headings(message, width: 100)

  def print(message), do: Docs.print(message, width: 100)

end
