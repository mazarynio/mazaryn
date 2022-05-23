defmodule Core.EventClient do
  def start do
    :event_server.start_link()
    :event_server.init([])
  end

  def new_event(name, date, loc, desc) do
    :event_server.add_event(name, date, loc, desc)
  end
end
