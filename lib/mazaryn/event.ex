defmodule Event do
  @moduledoc """
  Event Struct
  """

  defstruct name: nil,
    date: nil,
    loc: nil,
    desc: nil

  def new({:event, name, date, loc, desc}) do
    struct(Event, %{name: name, date: date, loc: loc, desc: desc})
  end
end
