defmodule MazarynWeb.ChatsLive.Index do
  use MazarynWeb, :live_view
  require Logger

  import MazarynWeb.Live.Helper
  import MazarynWeb.ChatsLive.Components

  alias Account.Users
  alias Account.User

  on_mount {MazarynWeb.UserLiveAuth, :user_resource}

  @impl true
  def mount(_params, _session, socket) do
    assigns = [search: nil, found_users: [], chats: [], users_without_chats: []]
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
    recipient =
      (to_charlist(params["recipient_id"]) ||
         actor.chat
         |> Enum.sort_by(& &1.date_updated, :desc)
         |> List.first()
         |> then(&(&1 || %{}))
         |> Map.get(:recipient_id))
      |> case do
        chat -> chat |> Users.one_by_id() |> elem(1)
        _ -> nil
      end

    # todo: chat may be just be id??? 
    chats =
      (recipient || %{})
      |> Map.get(:chat, [])
      |> Kernel.++(actor.chat)
      |> Enum.filter(&(&1.recipient_id == actor.id or &1.recipient_id == recipient.id))
      |> Enum.sort_by(& &1.date_updated, :desc)

    previous_contacts =
      actor.chat
      |> Enum.map(& &1.recipient_id)
      |> Enum.uniq()
      |> case do
        [] -> []
        ids -> Enum.reduce(ids, &[Users.one_by_id(&1.recipient_id) | &2])
      end

    assign(socket,
      current_recipient: struct(recipient || %Account.User{}, chat: []),
      blank_chat?: is_nil(recipient),
      current_chat: chats,
      contacts: previous_contacts,
      other_users:
        Users.list()
        |> Enum.map(&(&1 |> Users.one_by_id() |> elem(1)))
        |> Kernel.--(previous_contacts)
    )
  end
end
