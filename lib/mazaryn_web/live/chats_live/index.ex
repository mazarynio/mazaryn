defmodule MazarynWeb.ChatsLive.Index do
  use MazarynWeb, :live_view
  require Logger

  import MazarynWeb.ChatsLive.Components

  alias Account.Users
  alias Account.User
  alias Mazaryn.Chats
  alias Mazaryn.Chats.Chat

  on_mount {MazarynWeb.UserLiveAuth, :user_resource}

  @impl true
  def mount(_params, _session, socket) do
    assigns = [
      search: nil,
      found_users: [],
      chats: [],
      users_without_chats: [],
      show_search: false,
      search_query: nil
    ]

    if connected?(socket),
      do: Phoenix.PubSub.subscribe(Mazaryn.PubSub, "chats:#{socket.assigns.user.id}"),
      else:
        Logger.warning(
          "Socket not connected: failed to subscribe to chats:#{socket.assigns.user.id}"
        )

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

  ## Private

  # index page loads most recent chat, and filters messages by the chat's recipient
  # index page also looks up for the specific chat given the recipient_id
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
      :username_not_exist ->
        nil

      erl_user ->
        [
          erl_user
          |> User.erl_changeset()
          |> User.build()
          |> elem(1)
        ]
    end
  end
end
