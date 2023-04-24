defmodule MazarynWeb.ChatsLive.Components.MessageInput do
  use MazarynWeb, :live_component

  alias MazarynWeb.Live.Helper
  alias MazarynWeb.Component.SelectLive

  alias Mazaryn.Chats

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(uploaded_files: [])
     |> allow_upload(:media, accept: ~w(.png .jpg .jpeg), max_entries: 2)}
  end

  @impl true
  def handle_event("validate-message", %{"chat" => params}, socket) do
    socket.assigns.message
    |> Chats.Chat.changeset(params)
    |> struct(action: :validate)
    |> then(&{:noreply, assign(socket, :message_changeset, &1)})
  end

  @impl true
  def handle_event("send-message", %{"chat" => params}, socket) do
    socket.assigns.user
    |> Chats.create_chat(socket.assigns.recipient, params)
    |> case do
      {:error, :invalid_chat_participants} ->
        put_flash(socket, :error, "Can not chat with your self...")

      {:error, changeset} ->
        assign(socket, :message_changeset, changeset)

      {:ok, chat} ->
        send(self, {:new_message, chat})

        socket
        |> put_flash(:info, "Message saved!")
        |> push_navigate(to: ~p(/chats?recipient_id=#{chat.recipient_id}))
    end
    |> then(&{:noreply, &1})
  end
end
