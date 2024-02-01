defmodule MazarynWeb.FileController do
  use MazarynWeb, :controller

  def serve_empty_file(conn, _params) do
    # You can set any content you want.
    text_response = "This is an empty file for enamad."

    conn
    |> put_resp_content_type("text/plain")
    |> put_resp_header("content-disposition", ~s(inline; filename="735600.txt"))
    |> send_resp(200, text_response)
  end
end
