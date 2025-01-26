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

  @impl true
def handle_event("save-edit", %{"chat" => chat}, socket) do
  %{"body" => body} = chat

  if String.trim(body) == "" do
    changeset =
      socket.assigns.message
      |> Chats.Chat.changeset(chat)
      |> Map.put(:action, :validate)

    {:noreply, assign(socket, :changeset, changeset)}
  else
    msg_id = socket.assigns.editting_message.id
    ChatClient.edit_msg(to_charlist(msg_id), body)

    updated_messages = Enum.map(socket.assigns.messages, fn msg ->
      if msg.id == msg_id, do: %{msg | body: body}, else: msg
    end)

    {:noreply,
     socket
     |> assign(:messages, updated_messages)
     |> assign(:editting_message, nil)
     |> assign(:changeset, Chat.changeset(%Chat{}, %{}))}
  end
end

 @impl true
  def handle_event("cancel-edit", _params, socket) do
  {:noreply, assign(socket, :editting_message, nil)}
  end
end
