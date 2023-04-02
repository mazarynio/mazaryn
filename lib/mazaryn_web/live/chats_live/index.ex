defmodule MazarynWeb.ChatsLive.Index do
  use MazarynWeb, :live_view
  require Logger

  import MazarynWeb.Live.Helper

  alias Mazaryn.Chats
  alias Account.Users

  on_mount {MazarynWeb.UserLiveAuth, :user_resource}

  @impl true
  def mount(_params, _session, socket) do
    assigns = [search: nil, found_users: []]
    {:ok, assign(socket, assigns)}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  ## Private
  defp apply_action(%{assigns: %{user: actor_id}} = socket, :index, _params) do
    assign(socket, chats: Chats.get_user_chats(actor_id))
  end

  defp apply_action(%{assigns: %{user: actor_id}} = socket, :show, %{"id" => chat_id}) do
    chat = Chats.get_by_chat_id(chat_id, actor_id)
    # latest_messages = Chats.get_latest_messages(chat_id, actor_id)
    assign(socket, chat: chat, messages: [])
  end

  defp apply_action(%{assigns: %{user: actor_id}} = socket, :new, %{
         "recipient_id" => recipient_id
       }) do
    recipient = Users.get_by_id(recipient_id)

    actor_id
    |> Users.get_by_id()
    |> Chats.create_chat(recipient)
    |> case do
      {:ok, chat} ->
        socket |> put_flash(:info, "New chat created") |> push_navigate(to: ~p(/chats/#{chat.id}))

      {:error, _changeset} ->
        put_flash(socket, :error, "Error creating chat")
    end
  end
end
