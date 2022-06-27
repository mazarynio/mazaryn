defmodule MazarynWeb.HomeLive.Index do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper, only: [signing_salt: 0]
  require Logger

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    {:ok, assign(socket, user_id: user_id)}
  end

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    {:ok, assign(socket, user_id: get_user_id(session_uuid))}
  end

  @impl true
  def handle_event("messages", _param, socket) do
    random_id = "/messages/" <> "1"
    {:noreply, push_redirect(socket, to: random_id)}
  end

  defp get_user_id(session_uuid) do
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
end
