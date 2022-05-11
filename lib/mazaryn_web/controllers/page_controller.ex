defmodule MazarynWeb.PageController do
  use MazarynWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
