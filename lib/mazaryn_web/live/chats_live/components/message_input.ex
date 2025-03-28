defmodule MazarynWeb.ChatsLive.Components.MessageInput do
  use MazarynWeb, :live_component

  # alias MazarynWeb.Component.CustomComponents
  # alias MazarynWeb.Live.Helper
  alias Mazaryn.Chats
  alias Mazaryn.Chats.Chat
  alias :chat_server, as: ChatClient

  require Logger

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(uploaded_files: [], changeset: Chat.changeset(%Chat{}, %{}), message: %Chat{})
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  @impl true
  def handle_event("validate-message", %{"chat" => params}, socket) do
    Logger.debug("Validating message with params: #{inspect(params)}")

    changeset =
      socket.assigns.message
      |> Chats.Chat.changeset(params)
      |> struct(action: :validate)

    Logger.debug("Validation changeset: #{inspect(changeset)}")

    {:noreply, assign(socket, :changeset, changeset)}
  end

  @impl true
  def handle_event("send-message", %{"chat" => params}, socket) do
    Logger.debug("Attempting to send message with params: #{inspect(params)}")
    Logger.debug("Current socket assigns: #{inspect(socket.assigns)}")

    result =
      socket.assigns.user
      |> Chats.create_chat(socket.assigns.recipient, params)

    case result do
      {:error, :invalid_chat_participants} ->
        Logger.warning("Invalid chat participants - user trying to chat with themselves")
        {:noreply, put_flash(socket, :error, "Can not chat with your self...")}

      {:error, changeset} ->
        Logger.error("Failed to create chat. Changeset errors: #{inspect(changeset.errors)}")
        {:noreply, assign(socket, :changeset, changeset)}

      {:ok, chat} ->
        Logger.debug("Successfully created chat: #{inspect(chat)}")
        send(self(), {:send_message, chat})
        {:noreply, assign(socket, :changeset, Chat.changeset(%Chat{}, %{}))}
    end
  end

  @impl true
  def handle_event("save-edit", %{"chat" => chat}, socket) do
    Logger.debug("Saving edit with chat: #{inspect(chat)}")

    %{"body" => body} = chat

    if String.trim(body) == "" do
      Logger.warning("Attempted to save empty message body")
      changeset =
        socket.assigns.message
        |> Chats.Chat.changeset(chat)
        |> Map.put(:action, :validate)

      {:noreply, assign(socket, :changeset, changeset)}
    else
      msg_id = socket.assigns.editting_message.id
      Logger.debug("Editing message with ID: #{msg_id}, new body: #{body}")

      ChatClient.edit_msg(to_charlist(msg_id), body)

      updated_messages =
        Enum.map(socket.assigns.messages, fn msg ->
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
    Logger.debug("Canceling edit operation")
    {:noreply, assign(socket, :editting_message, nil)}
  end
end
