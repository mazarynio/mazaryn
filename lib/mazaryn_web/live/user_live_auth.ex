defmodule MazarynWeb.UserLiveAuth do
  import Phoenix.LiveView
  import Phoenix.Component

  require Logger
  # def on_mount(:default, _params, _session, socket) do
  # user = %{}

  # socket = assign_new(socket, :current_user, fn -> user end)

  # if socket.assigns.current_user do
  #   {:cont, socket}
  # else
  #   {:halt, redirect(socket, to: "/login")}
  # end

  # end

  def on_mount(:default, _params, session, socket) do
    socket = assign_new(socket, :current_user, fn -> get_user_id(session) end)

    if socket.assigns.current_user do
      {:halt, redirect(socket, to: "/home")}
    else
      {:cont, socket}
    end
  end

  def on_mount(:user_resource, _params, session, socket) do
    socket =
      assign_new(socket, :user, fn ->
        session
        |> get_user_id()
        |> Account.Users.one_by_email()
        |> elem(1)
      end)

    if socket.assigns.user, do: {:cont, socket}, else: {:halt, redirect(socket, to: "/home")}
  end

  # def on_mount({:pubsub_subscribe, topics: topics}, _params, _session, socket) do
  # end

  def get_user_id(%{"session_uuid" => session_uuid}) do
    case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
      [{_, token}] ->
        case Phoenix.Token.verify(MazarynWeb.Endpoint, signing_salt(), token, max_age: 806_400) do
          {:ok, user_id} ->
            user_id

          _ ->
            nil
        end

      _ ->
        nil
    end
  end

  defp signing_salt() do
    MazarynWeb.Endpoint.config(:live_view)[:signing_salt]
  end
end
