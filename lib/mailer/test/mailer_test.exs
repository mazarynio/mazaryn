defmodule MailerTest do
  use ExUnit.Case
  doctest Mailer

  test "greets the world" do
    assert Mailer.hello() == :world
  end
end
