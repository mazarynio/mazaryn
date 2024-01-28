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

  def empty_page(conn, _params) do
    send_resp(conn, 200, "This is an empty page.")
  end
end
