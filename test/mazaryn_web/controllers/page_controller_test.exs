defmodule MazarynWeb.PageControllerTest do
  use MazarynWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, "/")

    assert html_response(conn, 200) =~
             ~s(Mazaryn · an Online community which empower users to explore their Talents)
  end
end
