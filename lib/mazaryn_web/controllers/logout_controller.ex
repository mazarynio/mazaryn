defmodule MazarynWeb.LogoutController do
  use MazarynWeb, :controller

  import Plug.Conn, only: [get_session: 2, clear_session: 1, configure_session: 2]

  def index(conn, _params) do
    conn
    |> delete_session_token(get_session(conn, :session_uuid))
    |> clear_session()
    |> configure_session(drop: true)
    |> redirect(
      to:
        Routes.live_path(conn, MazarynWeb.AuthLive.Login, Gettext.get_locale(MazarynWeb.Gettext))
    )
  end

  def delete_session_token(conn, nil), do: conn

  def delete_session_token(conn, session_id) do
    :ets.delete(:mazaryn_auth_table, :"#{session_id}")

    conn
  end
end
