defmodule MazarynWeb.ChatsLive.Components.MessageInput do
  use MazarynWeb, :live_component

  # alias MazarynWeb.Component.CustomComponents
  # alias MazarynWeb.Live.Helper
  alias Mazaryn.Chats
  alias Mazaryn.Chats.Chat
  alias :chat_server, as: ChatClient

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(uploaded_files: [], changeset: Chat.changeset(%Chat{}, %{}), message: %Chat{})
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  @impl true
  def handle_event("validate-message", %{"chat" => params}, socket) do
    socket.assigns.message
    |> Chats.Chat.changeset(params)
    |> struct(action: :validate)
    |> then(&{:noreply, assign(socket, :changeset, &1)})
  end

  @impl true
  def handle_event("send-message", %{"chat" => params}, socket) do
    socket.assigns.user
    |> Chats.create_chat(socket.assigns.recipient, params)
    |> case do
      {:error, :invalid_chat_participants} ->
        put_flash(socket, :error, "Can not chat with your self...")

      {:error, changeset} ->
        assign(socket, :changeset, changeset)

      {:ok, chat} ->
        send(self(), {:send_message, chat})
        assign(socket, :changeset, Chat.changeset(%Chat{}, %{}))
    end
    |> then(&{:noreply, &1})
  end

  def handle_event("delete-message", %{"chat-id" => chat_id} = _params, socket) do
    chat_id = chat_id |> to_charlist
    ChatClient.delete_msg(chat_id)
    send(self(), :reload_chats)
    {:noreply, socket}
  end

  def handle_event(
        "edit-message",
        %{"chat-id" => chat_id, "new-content" => new_content} = _params,
        socket
      ) do
    chat_id = chat_id |> to_charlist
    ChatClient.edit_msg(chat_id, new_content)
    send(self(), :reload_chats)

    {:noreply, socket}
  end
end
