defmodule MazarynWeb.ArtistRequestController do
  use MazarynWeb, :controller

  def success(conn, _params) do
    locale = conn.assigns[:locale] || "en"

    conn
    |> put_layout(false)
    |> put_root_layout(false)
    |> render(:success, locale: locale)
  end
end
