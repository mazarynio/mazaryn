defmodule MazarynWeb.Plug.Session do
  import Plug.Conn, only: [get_session: 2, put_session: 3, halt: 1, assign: 3]
  import Phoenix.Controller, only: [redirect: 2]
  import Phoenix.Controller

  alias MazarynWeb.Router.Helpers, as: Routes

  @allowed_users ["arvand", "mazaryn", "zaryn"]

  def redirect_unauthorized(conn, _opts) do
    user_id = Map.get(conn.assigns, :user_id)

    if user_id == nil do
      conn
      |> put_session(:return_to, conn.request_path)
      |> redirect(to: "/login")
      |> halt()
    else
      conn
    end
  end

  def check_if_admin(conn, _opts) do
    user_id = Map.get(conn.assigns, :user_id)

    {:user, _, _, _, _, _, _, _, username, _, _, _, _, _, _,_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
     _, _, _, _, _, _, _} = Core.UserClient.get_user_by_email(user_id)

    if username == nil do
      conn
      |> put_flash(:error, "You are not authorized to access this page.")
      |> put_session(:return_to, conn.request_path)
      |> redirect(to: "/home")
      |> halt()
    else
      if Enum.member?(ManageUser.get_admin_list(), String.to_charlist(username)) do
        conn
      else
        conn
        |> put_flash(:error, "You are not authorized to access this page.")
        |> put_session(:return_to, conn.request_path)
        |> redirect(to: "/home")
        |> halt()
      end
    end
  end

  def allowed_users do
    @allowed_users
  end

  def validate_session(conn, _opts) do
    case get_session(conn, :session_uuid) do
      nil ->
        conn
        |> put_session(:session_uuid, Ecto.UUID.generate())

      session_uuid ->
        conn
        |> validate_session_token(session_uuid)
    end
  end

  def validate_session_token(conn, session_uuid) do
    case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
      [{_, token}] ->
        case Phoenix.Token.verify(MazarynWeb.Endpoint, signing_salt(), token, max_age: 806_400) do
          {:ok, user_id} ->
            conn
            |> assign(:user_id, user_id)
            |> put_session("user_id", user_id)

          _ ->
            conn
        end

      _ ->
        conn
    end
  end

  def signing_salt do
    MazarynWeb.Endpoint.config(:live_view)[:signing_salt] ||
      raise MazarynWeb.AuthenticationError, message: "missing signing_salt"
  end
end
