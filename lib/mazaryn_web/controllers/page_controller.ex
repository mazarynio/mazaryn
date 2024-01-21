defmodule MazarynWeb.PageController do
  use MazarynWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def contact(conn, _params) do
    render(conn, "contact.html")
  end

  def about(conn, _params) do
    render(conn, "about.html")
  end

  def privacy(conn, _params) do
    render(conn, "privacy.html")
  end

  def careers(conn, _params) do
    render(conn, "careers.html")
  end

  def dummy(conn, _params) do
    render(conn, "dummy.html")
  end
end
