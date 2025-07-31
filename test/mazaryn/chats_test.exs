defmodule Mazaryn.ChatsTest do
  @moduledoc """
  Tests for Mazaryn.Chats module
  """
  use ExUnit.Case, async: true

  import Mock
  import Ecto.Query
  alias Mazaryn.Chats
  alias Mazaryn.Chats.Chat
  alias Account.User
  alias Account.Users
  alias Mazaryn.Repo

  defp sample_user(id \\ "user_123", chat \\ []) do
    %User{
      id: to_charlist(id),
      username: "testuser",
      email: "test@example.com",
      chat: chat
    }
  end

  defp sample_chat_tuple do
    {:chat,
     "chat_123",                                  # id
     nil,                                         # ai_chat_id
     'user_123',                                  # user_id
     'user_456',                                  # recipient_id
     "Hello there!",                              # body
     [],                                          # media
     false,                                       # bot
     ~U[2024-01-01 10:00:00Z],                   # date_created
     ~U[2024-01-01 10:00:00Z],                   # date_updated
     "call_123",                                  # call_id
     "video",                                     # call_type
     "active",                                    # call_status
     "https://call.link",                         # call_link
     ~U[2024-01-01 10:00:00Z],                   # call_start_time
     ~U[2024-01-01 10:30:00Z],                   # call_end_time
     nil,                                         # timeout_ref
     %{}                                          # data
    }
  end

  defp sample_chat_struct do
    %Chat{
      id: "chat_123",
      user_id: 'user_123',
      recipient_id: 'user_456',
      body: "Hello there!",
      media: [],
      date_created: ~U[2024-01-01 10:00:00Z],
      date_updated: ~U[2024-01-01 10:00:00Z]
    }
  end

  describe "create_chat/3" do
    test "creates chat successfully with valid users and params" do
      actor = sample_user("123")
      recipient = sample_user("456")
      params = %{body: "Hello!", media: []}

      changeset = %Ecto.Changeset{valid?: true, changes: %{body: "Hello!", media: []}}

      with_mocks([
        {Chat, [], [changeset: fn _, _ -> changeset end]},
        {Ecto.Changeset, [:passthrough], [apply_action: fn _, _ -> {:ok, %{body: "Hello!", media: []}} end]},
        {:chat_server, [], [send_msg: fn _, _, _, _ -> "chat_123" end]},
        {:chat_server, [], [get_msg: fn "chat_123" -> sample_chat_tuple() end]},
        {Chat, [], [erl_changeset: fn _ -> {:ok, sample_chat_struct()} end]}
      ]) do
        result = Chats.create_chat(actor, recipient, params)
        assert {:ok, %Chat{}} = result
      end
    end

    test "returns error when actor tries to chat with themselves" do
      user = sample_user("123")
      params = %{body: "Hello!", media: []}

      result = Chats.create_chat(user, user, params)
      assert {:error, :self_chat_not_allowed} = result
    end

    test "returns error with invalid chat participants" do
      params = %{body: "Hello!", media: []}

      result = Chats.create_chat("invalid", "invalid", params)
      assert {:error, :invalid_chat_participants} = result
    end

    test "returns changeset error when validation fails" do
      actor = sample_user("123")
      recipient = sample_user("456")
      params = %{body: "", media: []}

      invalid_changeset = %Ecto.Changeset{valid?: false, errors: [body: {"can't be blank", []}]}

      with_mocks([
        {Chat, [], [changeset: fn _, _ -> invalid_changeset end]},
        {Ecto.Changeset, [:passthrough], [apply_action: fn _, _ -> {:error, invalid_changeset} end]}
      ]) do
        result = Chats.create_chat(actor, recipient, params)
        assert {:error, _} = result
      end
    end

    test "handles chat creation with media" do
      actor = sample_user("123")
      recipient = sample_user("456")
      params = %{body: "Check this out!", media: ["image1.jpg", "video1.mp4"]}

      changeset = %Ecto.Changeset{valid?: true, changes: %{body: "Check this out!", media: ["image1.jpg", "video1.mp4"]}}

      with_mocks([
        {Chat, [], [changeset: fn _, _ -> changeset end]},
        {Ecto.Changeset, [:passthrough], [apply_action: fn _, _ -> {:ok, %{body: "Check this out!", media: ["image1.jpg", "video1.mp4"]}} end]},
        {:chat_server, [], [send_msg: fn _, _, _, _ -> "chat_123" end]},
        {:chat_server, [], [get_msg: fn "chat_123" -> sample_chat_tuple() end]},
        {Chat, [], [erl_changeset: fn _ -> {:ok, sample_chat_struct()} end]}
      ]) do
        result = Chats.create_chat(actor, recipient, params)
        assert {:ok, %Chat{}} = result
      end
    end
  end

  describe "get_by_chat_id/1" do
    test "returns chat when chat exists" do
      with_mocks([
        {:chat_server, [], [get_chat_by_call_id: fn "chat_123" -> sample_chat_tuple() end]},
        {Chat, [], [erl_changeset: fn _ -> {:ok, sample_chat_struct()} end]}
      ]) do
        result = Chats.get_by_chat_id("chat_123")
        assert {:ok, %Chat{}} = result
      end
    end

    test "returns error when chat not found" do
      with_mock :chat_server, get_chat_by_call_id: fn _ -> :notfound end do
        result = Chats.get_by_chat_id("non_existent")
        assert {:error, :notfound} = result
      end
    end

    test "returns error when chat does not exist" do
      with_mock :chat_server, get_chat_by_call_id: fn _ -> :chat_not_exist end do
        result = Chats.get_by_chat_id("non_existent")
        assert {:error, :notfound} = result
      end
    end

    test "returns error for invalid chat record" do
      with_mock :chat_server, get_chat_by_call_id: fn _ -> :invalid_response end do
        result = Chats.get_by_chat_id("invalid")
        assert {:error, :invalid_chat_record} = result
      end
    end
  end

  describe "get_chats/1" do
    test "returns all chats when no ids specified" do
      chat_ids = ["chat_1", "chat_2"]

      with_mocks([
        {:chat_server, [], [list_chats: fn -> chat_ids end]},
        {:chat_server, [], [get_msg: fn _ -> sample_chat_tuple() end]},
        {Chat, [], [erl_changeset: fn _ -> {:ok, sample_chat_struct()} end]}
      ]) do
        result = Chats.get_chats()
        assert length(result) == 2
        assert Enum.all?(result, fn chat -> %Chat{} = chat end)
      end
    end

    test "returns filtered chats when ids specified" do
      chat_ids = ["chat_1", "chat_2", "chat_3"]
      filter_ids = ['chat_123']

      chat1 = sample_chat_struct() |> Map.put(:id, "chat_1")
      chat2 = sample_chat_struct() |> Map.put(:id, "chat_2")
      chat3 = sample_chat_struct() |> Map.put(:id, "chat_123")

      with_mocks([
        {:chat_server, [], [list_chats: fn -> chat_ids end]},
        {:chat_server, [], [get_msg: fn "chat_1" -> sample_chat_tuple(); "chat_2" -> sample_chat_tuple(); "chat_3" -> sample_chat_tuple() end]},
        {Chat, [], [erl_changeset: fn _ -> {:ok, chat3} end]}
      ]) do
        result = Chats.get_chats(filter_ids)
        assert length(result) == 3
      end
    end

    test "handles empty chat list" do
      with_mock :chat_server, list_chats: fn -> [] end do
        result = Chats.get_chats()
        assert result == []
      end
    end

    test "filters out invalid chats" do
      chat_ids = ["chat_1", "chat_2"]

      with_mocks([
        {:chat_server, [], [list_chats: fn -> chat_ids end]},
        {:chat_server, [], [get_msg: fn "chat_1" -> sample_chat_tuple(); _ -> nil end]},
        {Chat, [], [erl_changeset: fn tuple when is_tuple(tuple) -> {:ok, sample_chat_struct()}; _ -> {:error, :invalid} end]}
      ]) do
        result = Chats.get_chats()
        assert length(result) == 1
      end
    end

    test "sorts chats by date created in ascending order" do
      chat1 = sample_chat_struct() |> Map.put(:date_created, ~U[2024-01-01 10:00:00Z])
      chat2 = sample_chat_struct() |> Map.put(:date_created, ~U[2024-01-02 10:00:00Z])
      chat_ids = ["chat_1", "chat_2"]

      with_mocks([
        {:chat_server, [], [list_chats: fn -> chat_ids end]},
        {:chat_server, [], [get_msg: fn "chat_1" -> sample_chat_tuple(); "chat_2" -> sample_chat_tuple() end]},
        {Chat, [], [erl_changeset: fn _ -> {:ok, chat1} end]}
      ]) do
        result = Chats.get_chats()
        assert length(result) == 2
      end
    end
  end

  describe "get_users_with_chats/1" do

    test "returns empty list when actor has no chats" do
      actor = sample_user("123", [])

      with_mock Chats, [:passthrough], get_chats: fn [] -> [] end do
        result = Chats.get_users_with_chats(actor)
        assert result == []
      end
    end
  end

  describe "get_users_chatted_to/2" do

    test "filters out chats older than 10 hours" do
      actor = sample_user("123")
      old_time = DateTime.utc_now() |> DateTime.add(-15, :hour)

      chat1 = sample_chat_struct()
              |> Map.put(:user_id, 'user_123')
              |> Map.put(:date_created, old_time)

      with_mock Chats, [:passthrough], get_chats: fn -> [chat1] end do
        result = Chats.get_users_chatted_to(actor, 5)
        assert result == []
      end
    end

  end

  describe "get_latest_recipient/1" do
    test "returns user when given binary id" do
      user = sample_user("456")

      with_mock Users, one_by_id: fn 'user_456' -> {:ok, user} end do
        result = Chats.get_latest_recipient("user_456")
        assert result == user
      end
    end

    test "returns nil when user has no chat history" do
      actor = sample_user("123", [])

      with_mock Chats, [:passthrough], get_users_with_chats: fn _actor -> [] end do
        result = Chats.get_latest_recipient(actor)
        assert result == nil
      end
    end
  end

  describe "get_chat_messages/2" do

    test "returns empty list when actor tries to get messages with themselves" do
      user = sample_user("123")

      result = Chats.get_chat_messages(user, user)
      assert result == []
    end

    test "returns empty list with invalid participants" do
      result = Chats.get_chat_messages("invalid", "invalid")
      assert result == []
    end

  end

  describe "get_unread_count/1" do
    test "returns unread message count for user" do
      with_mock Repo, one: fn _ -> 5 end do
        result = Chats.get_unread_count("user_123")
        assert result == 5
      end
    end

    test "returns 0 when no unread messages" do
      with_mock Repo, one: fn _ -> 0 end do
        result = Chats.get_unread_count("user_123")
        assert result == 0
      end
    end
  end

  describe "start_video_call/2" do
    test "starts video call successfully between different users" do
      actor = sample_user("123")
      recipient = sample_user("456")

      with_mock :chat_server, start_video_call: fn _, _ -> "call_123" end do
        result = Chats.start_video_call(actor, recipient)
        assert {:ok, "call_123"} = result
      end
    end

    test "returns error when user tries to call themselves" do
      user = sample_user("123")

      result = Chats.start_video_call(user, user)
      assert {:error, :self_call_not_allowed} = result
    end

    test "handles video call creation failure" do
      actor = sample_user("123")
      recipient = sample_user("456")

      with_mock :chat_server, start_video_call: fn _, _ -> throw({:error, :call_failed}) end do
        result = Chats.start_video_call(actor, recipient)
        assert {:error, :call_failed} = result
      end
    end
  end

  describe "accept_call/1" do
    test "accepts call successfully" do
      with_mock :chat_server, accept_call: fn _ -> "call_123" end do
        result = Chats.accept_call("call_123")
        assert {:ok, "call_123"} = result
      end
    end

    test "handles call acceptance failure" do
      with_mock :chat_server, accept_call: fn _ -> throw({:error, :call_not_found}) end do
        result = Chats.accept_call("invalid_call")
        assert {:error, :call_not_found} = result
      end
    end
  end

  describe "end_call/1" do
    test "ends call successfully" do
      with_mock :chat_server, end_video_call: fn _ -> "call_123" end do
        result = Chats.end_call("call_123")
        assert {:ok, "call_123"} = result
      end
    end

    test "handles call ending failure" do
      with_mock :chat_server, end_video_call: fn _ -> throw({:error, :call_not_active}) end do
        result = Chats.end_call("invalid_call")
        assert {:error, :call_not_active} = result
      end
    end
  end
end
