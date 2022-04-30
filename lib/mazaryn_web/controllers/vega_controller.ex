defmodule MazarynWeb.VegaController do
  use MazarynWeb, :controller

  def index(conn, _params) do
    conn
    |> send_resp(200, render_react_app())
  end

  defp render_react_app() do
    Application.app_dir(:mazaryn, "priv/static/vega/index.html")
    |> File.read!()
  end
end
