defmodule MazarynWeb.ChatsLive.Index do
  use MazarynWeb, :live_view
  require Logger

  import MazarynWeb.Live.Helper
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
      message: %Chat{},
      message_changeset: Chat.changeset(%Chat{}, %{})
    ]

    MazarynWeb.Endpoint.subscribe("chats:#{socket.assigns.user.id}")
    {:ok, assign(socket, assigns)}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    {:noreply, apply_action(socket, socket.assigns.live_action, params)}
  end

  ## Private

  # index page loads most recent chat, and filters messages by the chat's recipient
  # index page also looks up for the specific chat given the recipient_id
  defp apply_action(%{assigns: %{user: actor}} = socket, :index, params) do
    previous_contacts = Chats.get_users_with_chats(actor)
    current_recipient = Chats.get_latest_recipient(params["recipient_id"] || actor) |> dbg
    current_chat = Chats.get_chats(actor, current_recipient)

    assign(socket,
      current_recipient: current_recipient || struct(%User{}, chat: []),
      blank_chat?: is_nil(current_recipient),
      current_chat: current_chat,
      contacts: previous_contacts,
      other_users:
        Users.list()
        |> Enum.map(&(&1 |> Users.one_by_id() |> elem(1)))
        |> Kernel.--(previous_contacts)
    )
  end
end
