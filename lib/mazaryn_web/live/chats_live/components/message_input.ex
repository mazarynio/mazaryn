defmodule MazarynWeb.ChatsLive.Components.MessageInput do
  use MazarynWeb, :live_component

  alias Mazaryn.Chats
  alias Mazaryn.Chats.Chat
  alias :chat_server, as: ChatClient

  @impl true
  def mount(socket) do
    star_svg =
      "data:image/svg+xml;base64," <>
        Base.encode64("""
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
          <path d="M50 5L61 39H97L68 61L79 95L50 74L21 95L32 61L3 39H39L50 5Z" fill="#FFD700" stroke="#FFA500" stroke-width="3"/>
        </svg>
        """)

    heart_svg =
      "data:image/svg+xml;base64," <>
        Base.encode64("""
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
          <path d="M50 88C50 88 10 65 10 40C10 20 30 10 50 30C70 10 90 20 90 40C90 65 50 88 50 88Z" fill="#FF4444" stroke="#CC0000" stroke-width="3"/>
        </svg>
        """)

    stickers = [
      %{:golden_star => star_svg},
      %{:red_heart => heart_svg}
    ]

    {:ok,
     socket
     |> assign(
       uploaded_files: [],
       changeset: Chat.changeset(%Chat{}, %{"body" => ""}),
       message: %Chat{},
       show_emoji_panel: false,
       show_sticker_panel: false,
       stickers: stickers
     )
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  @impl true
  def handle_event("validate-message", params, socket) do
    chat_params =
      case params do
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
    chat_params =
      case params do
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
          assign(socket, :changeset, Chat.changeset(%Chat{}, %{"body" => ""}))
      end
      |> then(&{:noreply, &1})
    end
  end

  @impl true
  def handle_event("save-edit", params, socket) do
    chat_params =
      case params do
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
          {:noreply, assign(socket, :changeset, Chat.changeset(%Chat{}, %{"body" => ""}))}

        editting_message ->
          msg_id = editting_message.id
          ChatClient.edit_msg(to_charlist(msg_id), body)

          send(self(), {:message_edited, msg_id, body})

          {:noreply,
           socket
           |> assign(:editting_message, nil)
           |> assign(:changeset, Chat.changeset(%Chat{}, %{"body" => ""}))}
      end
    end
  end

  @impl true
  def handle_event("cancel-edit", _params, socket) do
    {:noreply, assign(socket, :editting_message, nil)}
  end

  @impl true
  def handle_event("toggle_emoji_panel", _params, socket) do
    {:noreply,
     assign(socket,
       show_emoji_panel: !socket.assigns.show_emoji_panel,
       show_sticker_panel: false
     )}
  end

  @impl true
  def handle_event("close_emoji_panel", _params, socket) do
    {:noreply, assign(socket, :show_emoji_panel, false)}
  end

  @impl true
  def handle_event("toggle_sticker_panel", _params, socket) do
    {:noreply,
     assign(socket,
       show_sticker_panel: !socket.assigns.show_sticker_panel,
       show_emoji_panel: false
     )}
  end

  @impl true
  def handle_event("select_emoji", %{"emoji" => emoji_character}, socket) do
    {:noreply,
     socket
     |> assign(:show_emoji_panel, false)
     |> push_event("insert_emoji", %{
       emoji: emoji_character,
       component_id: to_string(socket.assigns.myself)
     })}
  end

  @impl true
  def handle_event("select_sticker", %{"sticker" => sticker_url}, socket) do
    {:noreply,
     socket
     |> assign(:show_sticker_panel, false)
     |> push_event("insert_sticker", %{
       sticker: sticker_url,
       component_id: to_string(socket.assigns.myself)
     })}
  end

  @impl true
  def handle_event("cancel-entry", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :media, ref)}
  end
end
