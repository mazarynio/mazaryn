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
    Logger.info("🔵 [MOUNT] ChatsLive.Index mounting")

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
      caller_username: nil,
      show_video_call: false
    ]

    if connected?(socket) do
      user_id = socket.assigns.user.id
      Logger.info("🔵 [MOUNT] Socket connected for user: #{user_id}")

      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "chats:#{user_id}")
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "calls:#{user_id}")

      Logger.info("✅ [MOUNT] Successfully subscribed to chats:#{user_id} and calls:#{user_id}")
    else
      Logger.warning("⚠️ [MOUNT] Socket not connected during mount")
    end

    {:ok, assign(socket, assigns)}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    Logger.info("🔵 [PARAMS] handle_params called with: #{inspect(params)}")
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  @impl true
  def handle_event("search_following", %{"search_query" => search_query}, socket) do
    Logger.info("🔍 [SEARCH] Searching for: #{search_query}")
    user = search_user_by_username(search_query)
    {:noreply, assign(socket, recent_chat_recepients: user || [], search_query: search_query)}
  end

  def handle_event("edit-message", %{"message-id" => msg_id}, socket) do
    Logger.info("✏️ [EDIT] Editing message: #{msg_id}")
    [message_to_edit] = Enum.filter(socket.assigns.messages, fn msg -> msg.id == msg_id end)
    {:noreply, assign(socket, :editting_message, message_to_edit)}
  end

  def handle_event("delete-message", %{"message-id" => msg_id}, socket) do
    Logger.info("🗑️ [DELETE] Deleting message: #{msg_id}")
    updated_messages = Enum.reject(socket.assigns.messages, fn msg -> msg.id == msg_id end)
    ChatClient.delete_msg(to_charlist(msg_id))
    {:noreply, assign(socket, :messages, updated_messages)}
  end

  def handle_event("start-video-call", params, socket) do
    Logger.info("📞 [VIDEO CALL START] ========================================")
    Logger.info("📞 [VIDEO CALL START] Event received!")
    Logger.info("📞 [VIDEO CALL START] Params: #{inspect(params)}")

    Logger.info(
      "📞 [VIDEO CALL START] Current user: #{socket.assigns.user.username} (#{socket.assigns.user.id})"
    )

    Logger.info(
      "📞 [VIDEO CALL START] Current recipient: #{inspect(socket.assigns[:current_recipient])}"
    )

    Logger.info("📞 [VIDEO CALL START] Blank chat?: #{socket.assigns[:blank_chat?]}")

    recipient = socket.assigns[:current_recipient]

    cond do
      is_nil(recipient) ->
        Logger.error("❌ [VIDEO CALL START] No recipient found in socket assigns")
        {:noreply, put_flash(socket, :error, "No recipient selected")}

      is_nil(recipient.id) ->
        Logger.error("❌ [VIDEO CALL START] Recipient has no ID: #{inspect(recipient)}")
        {:noreply, put_flash(socket, :error, "Invalid recipient")}

      recipient.id == socket.assigns.user.id ->
        Logger.warning("⚠️ [VIDEO CALL START] User attempted to call themselves")
        {:noreply, put_flash(socket, :error, "Cannot call yourself")}

      true ->
        Logger.info(
          "✅ [VIDEO CALL START] Valid recipient found: #{recipient.username} (#{recipient.id})"
        )

        start_video_call(socket, recipient)
    end
  end

  def handle_event("call-status-updated", %{"status" => status}, socket) do
    Logger.info("📡 [CALL STATUS] Call status updated to: #{status}")

    case status do
      "connected" ->
        Logger.info("✅ [CALL STATUS] Call connected")
        {:noreply, assign(socket, call_status: "connected", show_video_call: true)}

      "disconnected" ->
        Logger.info("🔴 [CALL STATUS] Call disconnected")

        socket =
          socket
          |> assign(
            call_status: nil,
            call_id: nil,
            call_link: nil,
            caller_username: nil,
            show_video_call: false
          )

        {:noreply, socket}

      _ ->
        Logger.info("📡 [CALL STATUS] Status: #{status}")
        {:noreply, assign(socket, call_status: status)}
    end
  end

  def handle_event(
        "incoming-call-received",
        %{"call_id" => call_id, "call_link" => call_link, "caller_username" => caller_username},
        socket
      ) do
    Logger.info("📞 [INCOMING CALL] Received call #{call_id} from #{caller_username}")
    Logger.info("📞 [INCOMING CALL] Call link: #{call_link}")

    socket =
      socket
      |> assign(
        call_id: call_id,
        call_status: "ringing",
        call_link: call_link,
        caller_username: caller_username,
        show_video_call: true
      )
      |> push_event("incoming-call", %{
        call_id: call_id,
        call_link: call_link,
        caller_username: caller_username
      })

    Logger.info("✅ [INCOMING CALL] Socket updated with call info")
    {:noreply, socket}
  end

  def handle_event("accept-video-call", %{"call_id" => call_id}, socket) do
    Logger.info("✅ [ACCEPT CALL] Accepting video call: #{call_id}")

    case Chats.accept_call(call_id) do
      {:ok, call_id} ->
        Logger.info("✅ [ACCEPT CALL] Call accepted, retrieving chat")

        case Chats.get_by_chat_id(call_id) do
          {:ok, chat} ->
            Logger.info("✅ [ACCEPT CALL] Chat retrieved, call_link: #{chat.call_link}")

            socket =
              socket
              |> assign(
                call_id: call_id,
                call_status: "connected",
                call_link: chat.call_link,
                show_video_call: true
              )
              |> push_event("accept-video-call", %{call_id: call_id, call_link: chat.call_link})

            Logger.info("✅ [ACCEPT CALL] Socket updated and event pushed")
            {:noreply, socket}

          {:error, reason} ->
            Logger.error("❌ [ACCEPT CALL] Failed to get chat: #{inspect(reason)}")
            {:noreply, put_flash(socket, :error, "Failed to accept call: #{inspect(reason)}")}
        end

      {:error, reason} ->
        Logger.error("❌ [ACCEPT CALL] Failed to accept: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to accept call: #{inspect(reason)}")}
    end
  end

  def handle_event("end-video-call", params, socket) do
    Logger.info("🔴 [END CALL] Ending video call with params: #{inspect(params)}")
    Logger.info("🔴 [END CALL] Current call_id: #{socket.assigns.call_id}")

    if socket.assigns.call_id do
      Logger.info("🔴 [END CALL] Calling Chats.end_call(#{socket.assigns.call_id})")

      case Chats.end_call(socket.assigns.call_id) do
        {:ok, _} ->
          Logger.info("✅ [END CALL] Call ended successfully")

        {:error, reason} ->
          Logger.error("❌ [END CALL] Failed to end call: #{inspect(reason)}")
      end
    end

    socket = push_event(socket, "end-video-call", %{})
    Logger.info("📡 [END CALL] Sent end-video-call event to client")

    socket =
      assign(socket,
        call_id: nil,
        call_status: nil,
        call_link: nil,
        caller_username: nil,
        show_video_call: false
      )

    Logger.info("✅ [END CALL] Socket state cleared")
    {:noreply, socket}
  end

  def handle_event("call-error", %{"message" => message}, socket) do
    Logger.error("❌ [CALL ERROR] #{message}")
    {:noreply, put_flash(socket, :error, message)}
  end

  # ALL handle_info/2 FUNCTIONS GROUPED TOGETHER
  @impl true
  def handle_info({:new_message, chat}, socket) do
    Logger.info("💬 [MESSAGE] New message received")
    {:noreply, assign(socket, :messages, [chat | socket.assigns.messages])}
  end

  def handle_info({:send_message, chat}, socket) do
    Logger.info("💬 [MESSAGE] Sending message")

    Phoenix.PubSub.broadcast_from(
      Mazaryn.PubSub,
      self(),
      "chats:#{chat.recipient_id}",
      {:new_message, chat}
    )

    messages = List.insert_at(socket.assigns.messages, -1, chat)
    {:noreply, assign(socket, :messages, messages)}
  end

  def handle_info({:incoming_call, caller_id, call_id, call_link, caller_username}, socket) do
    Logger.info("📞 [INCOMING CALL INFO] ========================================")
    Logger.info("📞 [INCOMING CALL INFO] Call ID: #{call_id}")
    Logger.info("📞 [INCOMING CALL INFO] From: #{caller_username} (#{caller_id})")
    Logger.info("📞 [INCOMING CALL INFO] Call link: #{call_link}")

    case Users.one_by_id(to_charlist(caller_id)) do
      {:ok, caller} ->
        Logger.info("✅ [INCOMING CALL INFO] Caller found: #{caller.username}")

        socket =
          socket
          |> assign(
            call_id: call_id,
            call_status: "ringing",
            call_link: call_link,
            caller_username: caller_username,
            show_video_call: true,
            current_recipient: caller
          )
          |> push_event("incoming-call", %{
            call_id: call_id,
            call_link: call_link,
            caller_username: caller_username
          })

        Logger.info("✅ [INCOMING CALL INFO] Socket updated")
        Logger.info("📞 [INCOMING CALL INFO] show_video_call: true")
        Logger.info("📞 [INCOMING CALL INFO] ========================================")
        {:noreply, socket}

      {:error, reason} ->
        Logger.error("❌ [INCOMING CALL INFO] Failed to get caller: #{inspect(reason)}")
        {:noreply, socket}
    end
  end

  def handle_info({:message_edited, message_id, new_body}, socket) do
    Logger.info("✏️ [MESSAGE EDIT] Message #{message_id} edited")

    updated_messages =
      Enum.map(socket.assigns.messages, fn msg ->
        if msg.id == message_id do
          %{msg | body: new_body, date_updated: DateTime.utc_now()}
        else
          msg
        end
      end)

    {:noreply, assign(socket, :messages, updated_messages)}
  end

  defp start_video_call(socket, recipient) do
    %User{id: actor_id, username: actor_username} = socket.assigns.user
    %User{id: recipient_id, username: recipient_username} = recipient

    Logger.info("📞 [START CALL] ========================================")
    Logger.info("📞 [START CALL] From: #{actor_username} (#{actor_id})")
    Logger.info("📞 [START CALL] To: #{recipient_username} (#{recipient_id})")

    if actor_id == recipient_id do
      Logger.warning("⚠️ [START CALL] User attempted to call themselves")
      {:noreply, put_flash(socket, :error, "Cannot call yourself")}
    else
      Logger.info("📞 [START CALL] Calling Chats.start_video_call...")

      case Chats.start_video_call(socket.assigns.user, recipient) do
        {:ok, call_id} ->
          Logger.info("✅ [START CALL] Video call initiated with call_id: #{call_id}")

          case Chats.get_by_chat_id(call_id) do
            {:ok, chat} ->
              Logger.info("✅ [START CALL] Retrieved chat record")
              Logger.info("📞 [START CALL] Call link: #{chat.call_link}")
              Logger.info("📡 [START CALL] Broadcasting to calls:#{recipient_id}")

              Phoenix.PubSub.broadcast(
                Mazaryn.PubSub,
                "calls:#{recipient_id}",
                {:incoming_call, actor_id, call_id, chat.call_link, actor_username}
              )

              Logger.info("✅ [START CALL] Broadcast sent")

              socket =
                socket
                |> assign(
                  call_id: call_id,
                  call_status: "ringing",
                  call_link: chat.call_link,
                  caller_username: actor_username,
                  show_video_call: true,
                  current_recipient: recipient
                )
                |> push_event("start-video-call", %{call_id: call_id, call_link: chat.call_link})

              Logger.info("✅ [START CALL] Socket updated and event pushed")
              Logger.info("📞 [START CALL] show_video_call: #{socket.assigns.show_video_call}")
              Logger.info("📞 [START CALL] ========================================")

              {:noreply, socket}

            {:error, reason} ->
              Logger.error("❌ [START CALL] Failed to get chat by call_id: #{inspect(reason)}")
              {:noreply, put_flash(socket, :error, "Cannot start video call: #{inspect(reason)}")}
          end

        {:error, reason} ->
          Logger.error("❌ [START CALL] Failed to start video call: #{inspect(reason)}")
          {:noreply, put_flash(socket, :error, "Cannot start video call: #{inspect(reason)}")}
      end
    end
  end

  defp apply_action(%{assigns: %{user: actor}} = socket, :index, params) do
    Logger.info("🔵 [APPLY ACTION] Applying :index action")
    Logger.info("🔵 [APPLY ACTION] Params: #{inspect(params)}")

    current_recipient = Chats.get_latest_recipient(params["recipient_id"] || actor)

    if current_recipient do
      Logger.info(
        "✅ [APPLY ACTION] Current recipient: #{current_recipient.username} (#{current_recipient.id})"
      )
    else
      Logger.info("⚠️ [APPLY ACTION] No current recipient")
    end

    messages = Chats.get_chat_messages(actor, current_recipient)
    recent_chat_recipients = Chats.get_users_chatted_to(actor)

    Logger.info("📊 [APPLY ACTION] Messages count: #{length(messages)}")
    Logger.info("📊 [APPLY ACTION] Recent contacts: #{length(recent_chat_recipients)}")

    socket =
      assign(socket,
        current_recipient: current_recipient || struct(%User{}, chat: []),
        blank_chat?: is_nil(current_recipient),
        messages: messages,
        contacts: recent_chat_recipients,
        recent_chat_recepients: recent_chat_recipients,
        other_users: []
      )

    Logger.info("✅ [APPLY ACTION] Socket assigns updated")
    Logger.info("📊 [APPLY ACTION] blank_chat?: #{socket.assigns.blank_chat?}")

    socket
  end

  defp search_user_by_username(username) do
    Logger.info("🔍 [SEARCH USER] Searching for username: #{username}")

    case username |> Core.UserClient.search_user() do
      :username_not_exist ->
        Logger.info("⚠️ [SEARCH USER] Username not found")
        nil

      erl_user ->
        Logger.info("✅ [SEARCH USER] User found")
        [erl_user |> User.erl_changeset() |> User.build() |> elem(1)]
    end
  end
end
