defmodule MazarynWeb.Plug.CheckAllowedUser do
  import Plug.Conn
  import Phoenix.Controller

  alias MazarynWeb.Router.Helpers, as: Routes

  @allowed_users ["arvand", "mazaryn", "zaryn"]

  def init(default), do: default

  def call(conn, _opts) do
    # Define this function based on your auth logic
    username = get_current_user_username(conn)

    if username in allowed_users() do
      conn
    else
      conn
      |> put_flash(:error, "You are not authorized to access this page.")
      |> put_session(:return_to, conn.request_path)
      |> redirect(to: "/dashboard")
      |> halt()
    end
  end

  def allowed_users do
    @allowed_users
  end

  defp get_current_user_username(conn) do
    # Replace this with your actual method of retrieving the current user's username
    conn.assigns[:current_user].username
  end
end
