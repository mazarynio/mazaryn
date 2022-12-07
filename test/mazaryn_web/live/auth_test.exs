defmodule MazarynWeb.AuthTest do
  use ExUnit.Case, async: false
  use MazarynWeb.ConnCase

  import Phoenix.LiveViewTest

  setup_all do
    %{
      email: "user1@example.com",
      password: "password!123!"
    }
  end

  describe "live auth" do
    test "success: logged in user", %{conn: conn, email: email, password: password} do
      assert {:ok, view, _html} = live(conn, Routes.live_path(conn, MazarynWeb.AuthLive.Login))

      form_data = %{
        email: email,
        password: password
      }

      {:error, {:live_redirect, %{kind: :push, to: path}}} =
        view
        |> form("form", form: form_data)
        |> render_submit()

      assert path === "/home"
    end

    test "success: logged out user", %{conn: conn} do
      conn = get(conn, "/logout")

      assert html_response(conn, 302) =~
               ~s(<html><body>You are being <a href=\"/login\">redirected</a>.</body></html>)

      assert redirected_to(conn) == "/login"
    end
  end
end
