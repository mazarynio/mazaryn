defmodule MazarynWeb.ChatsLive.Index do
  use MazarynWeb, :live_view
  use Phoenix.Component
  require Logger

  import MazarynWeb.ChatsLive.Components

  alias Account.Users
  alias Account.User
  alias Mazaryn.Chats
  alias Core.ChatClient

  on_mount {MazarynWeb.UserLiveAuth, :user_resource}

  @impl true
  def mount(_params, _session, socket) do
    assigns = [
      search: nil,
      found_users: [],
      chats: [],
      users_without_chats: [],
      show_search: false,
      search_query: nil,
      editting_message: false,
      call_id: nil,
      call_status: nil,
      call_link: nil,
      caller_username: nil
    ]

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "chats:#{socket.assigns.user.id}")
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "calls:#{socket.assigns.user.id}")
    else
      Logger.warning("Socket not connected: failed to subscribe to chats/calls:#{socket.assigns.user.id}")
    end

    {:ok, assign(socket, assigns)}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  @impl true
  def handle_event("search_following", %{"search_query" => search_query}, socket) do
    user = search_user_by_username(search_query)
    {:noreply, assign(socket, recent_chat_recepients: user || [], search_query: search_query)}
  end

  def handle_event("edit-message", %{"message-id" => msg_id}, socket) do
    [message_to_edit] = Enum.filter(socket.assigns.messages, fn msg -> msg.id == msg_id end)
    {:noreply, assign(socket, :editting_message, message_to_edit)}
  end

  def handle_event("delete-message", %{"message-id" => msg_id}, socket) do
    updated_messages = Enum.reject(socket.assigns.messages, fn msg -> msg.id == msg_id end)
    ChatClient.delete_msg(to_charlist(msg_id))
    {:noreply, assign(socket, :messages, updated_messages)}
  end

  def handle_event("start-video-call", %{"recipient_id" => recipient_id}, socket) do
    recipient = Users.one_by_id(to_charlist(recipient_id)) |> elem(1)
    socket = assign(socket, current_recipient: recipient, blank_chat?: false)
    handle_event("start-video-call", %{}, socket)
  end

  def handle_event("start-video-call", _params, socket) do
    %User{id: actor_id} = socket.assigns.user
    %User{id: recipient_id} = socket.assigns.current_recipient

    case Chats.start_video_call(socket.assigns.user, socket.assigns.current_recipient) do
      {:ok, call_id} ->
        case Chats.get_by_chat_id(call_id) do
          {:ok, chat} ->
            Phoenix.PubSub.broadcast(
              Mazaryn.PubSub,
              "calls:#{recipient_id}",
              {:incoming_call, actor_id, call_id, chat.call_link}
            )
            socket =
              socket
              |> assign(call_id: call_id, call_status: "ringing", call_link: chat.call_link)
              |> push_event("start-video-call", %{call_id: call_id, call_link: chat.call_link})
            {:noreply, socket}

          {:error, :notfound} ->
            {:noreply, put_flash(socket, :error, "Cannot start video call: Chat not found.")}

          {:error, :invalid_chat_record} ->
            {:noreply, put_flash(socket, :error, "Cannot start video call: Invalid chat record.")}

          {:error, reason} ->
            {:noreply, put_flash(socket, :error, "Cannot start video call: #{inspect(reason)}")}
        end

      {:error, {:rust_service_failed, reason}} ->
        {:noreply, put_flash(socket, :error, "Cannot start video call: WebRTC service failed (#{inspect(reason)}).")}

      {:error, :invalid_call_participants} ->
        {:noreply, put_flash(socket, :error, "Cannot start video call: Invalid participants.")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to start call: #{inspect(reason)}")}
    end
  end

  def handle_event("accept-video-call", %{"call_id" => call_id}, socket) do
    case Chats.accept_call(call_id) do
      {:ok, call_id} ->
        case Chats.get_by_chat_id(call_id) do
          {:ok, chat} ->
            socket =
              socket
              |> assign(call_id: call_id, call_status: "connected", call_link: chat.call_link)
              |> push_event("accept-video-call", %{call_id: call_id, call_link: chat.call_link})
            {:noreply, socket}

          {:error, reason} ->
            {:noreply, put_flash(socket, :error, "Failed to accept call: #{inspect(reason)}")}
        end

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to accept call: #{inspect(reason)}")}
    end
  end

  def handle_event("end-video-call", %{"call_id" => call_id}, socket) do
    case Chats.end_call(call_id) do
      {:ok, call_id} ->
        socket =
          socket
          |> assign(call_id: nil, call_status: nil, call_link: nil, caller_username: nil)
          |> push_event("end-video-call", %{call_id: call_id})
        {:noreply, socket}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to end call: #{inspect(reason)}")}
    end
  end

  def handle_event("call-error", %{"message" => message}, socket) do
    {:noreply, put_flash(socket, :error, message)}
  end

  @impl true
  def handle_info({:new_message, chat}, socket) do
    {:noreply, assign(socket, :messages, [chat | socket.assigns.messages])}
  end

  def handle_info({:send_message, chat}, socket) do
    Phoenix.PubSub.broadcast_from(
      Mazaryn.PubSub,
      self(),
      "chats:#{chat.recipient_id}",
      {:new_message, chat}
    )
    messages = List.insert_at(socket.assigns.messages, -1, chat)
    {:noreply, assign(socket, :messages, messages)}
  end

  def handle_info({:incoming_call, caller_id, call_id, call_link}, socket) do
    caller = Users.one_by_id(to_charlist(caller_id)) |> elem(1)
    socket =
      socket
      |> assign(call_id: call_id, call_status: "ringing", call_link: call_link, caller_username: caller.username)
      |> push_event("incoming-call", %{
        call_id: call_id,
        call_link: call_link,
        caller_username: caller.username
      })
    {:noreply, socket}
  end

  ## Private

  defp apply_action(%{assigns: %{user: actor}} = socket, :index, params) do
    previous_contacts = Chats.get_users_with_chats(actor)
    current_recipient = Chats.get_latest_recipient(params["recipient_id"] || actor)
    messages = Chats.get_chat_messages(actor, current_recipient)

    recent_chat_recepients = Chats.get_users_chatted_to(actor)

    assign(socket,
      current_recipient: current_recipient || struct(%User{}, chat: []),
      blank_chat?: is_nil(current_recipient),
      messages: messages,
      contacts: previous_contacts,
      recent_chat_recepients: recent_chat_recepients,
      other_users:
        Users.list()
        |> Enum.map(&(&1 |> Users.one_by_id() |> elem(1)))
        |> Kernel.--(previous_contacts)
        |> Kernel.--([actor])
    )
  end

  defp search_user_by_username(username) do
    case username |> Core.UserClient.search_user() do
      :username_not_exist -> nil
      erl_user ->
        [erl_user |> User.erl_changeset() |> User.build() |> elem(1)]
    end
  end
end
