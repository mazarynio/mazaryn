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
  def handle_event("validate-message", params, socket) do
    chat_params = case params do
      %{"chat" => chat_params} -> chat_params
      %{"self_chat_not_allowed" => chat_params} -> chat_params
      _ -> %{}
    end

    user_id = socket.assigns.user.id
    recipient_id = socket.assigns.recipient.id

    if user_id == recipient_id do
      changeset =
        socket.assigns.message
        |> Chat.changeset(chat_params)
        |> Ecto.Changeset.add_error(:recipient_id, "You cannot send messages to yourself")
        |> struct(action: :validate)

      {:noreply, assign(socket, :changeset, changeset)}
    else
      changeset =
        socket.assigns.message
        |> Chat.changeset(chat_params)
        |> struct(action: :validate)

      {:noreply, assign(socket, :changeset, changeset)}
    end
  end

  @impl true
  def handle_event("send-message", params, socket) do
    chat_params = case params do
      %{"chat" => chat_params} -> chat_params
      %{"self_chat_not_allowed" => chat_params} -> chat_params
      _ -> %{}
    end

    user_id = socket.assigns.user.id
    recipient_id = socket.assigns.recipient.id

    if user_id == recipient_id do
      changeset =
        socket.assigns.message
        |> Chat.changeset(chat_params)
        |> Ecto.Changeset.add_error(:recipient_id, "You cannot send messages to yourself")
        |> struct(action: :validate)

      {:noreply, assign(socket, :changeset, changeset)}
    else
      socket.assigns.user
      |> Chats.create_chat(socket.assigns.recipient, chat_params)
      |> case do
        {:error, :self_chat_not_allowed} ->
          changeset =
            socket.assigns.message
            |> Chat.changeset(chat_params)
            |> Ecto.Changeset.add_error(:recipient_id, "You cannot send messages to yourself")
            |> struct(action: :validate)

          assign(socket, :changeset, changeset)

        {:error, changeset} ->
          assign(socket, :changeset, changeset)

        {:ok, chat} ->
          send(self(), {:send_message, chat})
          assign(socket, :changeset, Chat.changeset(%Chat{}, %{}))
      end
      |> then(&{:noreply, &1})
    end
  end

  @impl true
  def handle_event("save-edit", params, socket) do
    chat_params = case params do
      %{"chat" => chat_params} -> chat_params
      %{"self_chat_not_allowed" => chat_params} -> chat_params
      _ -> %{}
    end

    %{"body" => body} = chat_params

    if String.trim(body) == "" do
      changeset =
        socket.assigns.message
        |> Chats.Chat.changeset(chat_params)
        |> Map.put(:action, :validate)

      {:noreply, assign(socket, :changeset, changeset)}
    else
      case Map.get(socket.assigns, :editting_message) do
        nil ->
          {:noreply, assign(socket, :changeset, Chat.changeset(%Chat{}, %{}))}

        editting_message ->
          msg_id = editting_message.id
          ChatClient.edit_msg(to_charlist(msg_id), body)

          send(self(), {:message_edited, msg_id, body})

          {:noreply,
           socket
           |> assign(:editting_message, nil)
           |> assign(:changeset, Chat.changeset(%Chat{}, %{}))}
      end
    end
  end

  @impl true
  def handle_event("cancel-edit", _params, socket) do
    {:noreply, assign(socket, :editting_message, nil)}
  end
end
