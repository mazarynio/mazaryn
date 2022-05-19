defmodule Mailer do
  @moduledoc """
  Documentation for `Mailer`.
  """

  use Bamboo.Mailer, otp_app: :mailer

  @doc """
  Hello world.

  ## Examples

      iex> Mailer.hello()
      :world

  """
  def hello do
    :world
  end
end
