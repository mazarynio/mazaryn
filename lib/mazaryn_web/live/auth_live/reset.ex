defmodule MazarynWeb.AuthLive.Reset do
  use MazarynWeb, :live_view

  def mount(_params, _session, socket) do
    if connected?(socket), do: socket |> IO.inspect(label: 1)

    {:ok, socket}
  end
end
