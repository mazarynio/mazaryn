defmodule MazarynWeb.PostController do
  use MazarynWeb, :controller




  # def create_post(conn, %{"content" => content}) do
  #   with {:ok, better_params} <- Tarams.cast(%{"content" => content}, @content) do
  #     case UserClient.creating_post(better_params.content) do
  #      {:ok, msg } ->
  #        IO.inspect(msg)
  #        render(conn, "post.json", post: msg)
  #      {:error, msg} ->
  #        IO.inspect(msg)
  #        render(conn, "error.json", message: msg)
  #     end
  #     else
  #       # return params error
  #       {:error, error} -> conn |> put_status(:bad_request) |> render("error.json", message: error)
  #     end
  # end
end
