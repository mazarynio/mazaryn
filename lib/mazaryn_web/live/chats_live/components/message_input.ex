defmodule MazarynWeb.ChatsLive.Components.MessageInput do
  use MazarynWeb, :live_component

  alias Mazaryn.Chats
  alias Mazaryn.Chats.Chat
  alias :chat_server, as: ChatClient

  @impl true
  def mount(socket) do
    emojis = [
      %{:grinning_face => "\u{1F600}"},
      %{:beaming_face => "\u{1F601}"},
      %{:joy => "\u{1F602}"},
      %{:heart_eyes => "\u{1F60D}"},
      %{:winking_face => "\u{1F609}"},
      %{:relieved_face => "\u{1F60C}"},
      %{:thinking_face => "\u{1F914}"},
      %{:smiling_face_with_sunglasses => "\u{1F60E}"},
      %{:fire => "\u{1F525}"},
      %{:red_heart => "\u{2764}\u{FE0F}"},
      %{:thumbs_up => "\u{1F44D}"},
      %{:clapping_hands => "\u{1F44F}"},
      %{:party_popper => "\u{1F389}"},
      %{:star_struck => "\u{1F929}"},
      %{:crying_with_laughter => "\u{1F923}"},
      %{:smiling_face_with_hearts => "\u{1F970}"},
      %{:hundred_points => "\u{1F4AF}"},
      %{:rocket => "\u{1F680}"},
      %{:rainbow => "\u{1F308}"},
      %{:smiling_face => "\u{1F60A}"},
      %{:raising_hands => "\u{1F64C}"},
      %{:partying_face => "\u{1F973}"},
      %{:crying_face => "\u{1F622}"},
      %{:flushed_face => "\u{1F633}"},
      %{:folded_hands => "\u{1F64F}"},
      %{:glowing_star => "\u{1F31F}"},
      %{:kissing_face => "\u{1F618}"},
      %{:winking_face_with_tongue => "\u{1F61C}"},
      %{:hugging_face => "\u{1F917}"},
      %{:sleeping_face => "\u{1F634}"},
      %{:sweating_smiling_face => "\u{1F605}"},
      %{:smiling_face_with_halo => "\u{1F607}"},
      %{:pouting_face => "\u{1F623}"},
      %{:nerd_face => "\u{1F913}"},
      %{:yum_face => "\u{1F60B}"},
      %{:tongue_out_face => "\u{1F61B}"},
      %{:smiling_face_with_three_hearts => "\u{1F970}"},
      %{:grimacing_face => "\u{1F62C}"},
      %{:squinting_face_with_tongue => "\u{1F61D}"},
      %{:face_with_steam => "\u{1F624}"},
      %{:expressionless_face => "\u{1F611}"},
      %{:grinning_squinting_face => "\u{1F606}"},
      %{:open_mouth_face => "\u{1F62E}"},
      %{:smiling_cat_with_heart_eyes => "\u{1F63B}"},
      %{:smiling_face_with_open_mouth => "\u{1F603}"},
      %{:grinning_face_with_big_eyes => "\u{1F604}"},
      %{:sad_face => "\u{1F61E}"},
      %{:angry_face => "\u{1F621}"},
      %{:confused_face => "\u{1F615}"},
      %{:surprised_face => "\u{1F62F}"},
      %{:smiling_devil => "\u{1F608}"},
      %{:crying_sad_face => "\u{1F625}"},
      %{:face_with_medical_mask => "\u{1F637}"},
      %{:pained_face => "\u{1F616}"},
      %{:worried_face => "\u{1F61F}"},
      %{:anxious_face => "\u{1F630}"},
      %{:raised_hand => "\u{270B}"},
      %{:ok_hand => "\u{1F44C}"},
      %{:point_left => "\u{1F448}"},
      %{:point_right => "\u{1F449}"},
      %{:point_up => "\u{1F446}"},
      %{:point_down => "\u{1F447}"},
      %{:sign_of_the_horns => "\u{1F918}"},
      %{:vulcan_salute => "\u{1F596}"},
      %{:raising_hand => "\u{1F64B}"},
      %{:balloon => "\u{1F388}"},
      %{:gift => "\u{1F381}"},
      %{:birthday_cake => "\u{1F382}"},
      %{:camera => "\u{1F4F8}"},
      %{:laptop => "\u{1F4BB}"},
      %{:smartphone => "\u{1F4F1}"},
      %{:money_with_wings => "\u{1F4B8}"},
      %{:lock => "\u{1F512}"},
      %{:bell => "\u{1F514}"},
      %{:cat => "\u{1F431}"},
      %{:dog => "\u{1F436}"},
      %{:bear => "\u{1F43B}"},
      %{:lion => "\u{1F981}"},
      %{:panda => "\u{1F43C}"},
      %{:frog => "\u{1F438}"},
      %{:monkey => "\u{1F435}"},
      %{:unicorn => "\u{1F984}"},
      %{:sparkling_heart => "\u{1F496}"},
      %{:collision => "\u{1F4A5}"},
      %{:rainbow_symbol => "\u{1F308}"},
      %{:sun => "\u{2600}\u{FE0F}"},
      %{:star => "\u{2B50}"},
      %{:apple => "\u{1F34E}"},
      %{:pizza => "\u{1F355}"},
      %{:hamburger => "\u{1F354}"},
      %{:coffee => "\u{2615}"},
      %{:ice_cream => "\u{1F366}"},
      %{:wine_glass => "\u{1F377}"},
      %{:rocket_ship => "\u{1F680}"},
      %{:airplane => "\u{2708}\u{FE0F}"},
      %{:globe => "\u{1F30D}"},
      %{:trophy => "\u{1F3C6}"}
    ]

    {:ok,
     socket
     |> assign(
       uploaded_files: [],
       changeset: Chat.changeset(%Chat{}, %{"body" => ""}),
       message: %Chat{},
       show_emoji_panel: false,
       emojis: emojis
     )
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
          assign(socket, :changeset, Chat.changeset(%Chat{}, %{"body" => ""}))
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
    {:noreply, assign(socket, :show_emoji_panel, !socket.assigns.show_emoji_panel)}
  end

  @impl true
  def handle_event("select_emoji", %{"emoji" => emoji_character}, socket) do
    {:noreply,
     socket
     |> assign(:show_emoji_panel, false)
     |> push_event("insert_emoji", %{emoji: emoji_character, component_id: to_string(socket.assigns.myself)})}
  end

  @impl true
  def handle_event("cancel-entry", %{"ref" => ref}, socket) do
    {:noreply, cancel_upload(socket, :media, ref)}
  end
end
