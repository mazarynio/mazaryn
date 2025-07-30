defmodule Chats.ChatTest do
  @moduledoc """
  Tests for Mazaryn.Chats.Chat embedded schema
  """
  use ExUnit.Case, async: true

  alias Mazaryn.Chats.Chat

  defp valid_chat_attrs do
    %{
      user_id: "user_123",
      recipient_id: "recipient_456",
      body: "Hello, this is a test message"
    }
  end

  defp valid_extended_attrs do
    Map.merge(valid_chat_attrs(), %{
      ai_chat_id: "ai_chat_789",
      media: ["image1.jpg", "video1.mp4"],
      bot: "chatbot_1",
      date_created: ~U[2024-01-01 10:00:00Z],
      date_updated: ~U[2024-01-02 10:00:00Z],
      call_id: "call_123",
      call_type: "video",
      call_status: "active",
      call_link: "https://call.example.com/123",
      call_start_time: ~U[2024-01-01 10:00:00Z],
      call_end_time: ~U[2024-01-01 10:30:00Z],
      timeout_ref: "ref_123",
      data: %{"metadata" => %{"source" => "mobile"}},
      status: "read"
    })
  end

  defp sample_erlang_tuple do
    {:chat,
     "chat_123",
     "ai_chat_789",
     "user_123",
     "recipient_456",
     "Hello, this is a test message",
     ["image1.jpg"],
     "chatbot_1",
     ~U[2024-01-01 10:00:00Z],
     ~U[2024-01-02 10:00:00Z],
     "call_123",
     "video",
     "active",
     "https://call.example.com/123",
     ~U[2024-01-01 10:00:00Z],
     ~U[2024-01-01 10:30:00Z],
     "ref_123",
     %{"metadata" => %{"source" => "mobile"}}}
  end

  describe "changeset/2 with valid data" do
    test "creates valid changeset with required fields only" do
      changeset = Chat.changeset(%Chat{}, valid_chat_attrs())
      assert changeset.valid?
      assert changeset.changes.user_id == "user_123"
      assert changeset.changes.recipient_id == "recipient_456"
      assert changeset.changes.body == "Hello, this is a test message"

      chat_struct = Ecto.Changeset.apply_changes(changeset)
      assert chat_struct.status == "unread"
    end

    test "creates valid changeset with all fields" do
      changeset = Chat.changeset(%Chat{}, valid_extended_attrs())
      assert changeset.valid?
      assert changeset.changes.ai_chat_id == "ai_chat_789"
      assert changeset.changes.user_id == "user_123"
      assert changeset.changes.recipient_id == "recipient_456"
      assert changeset.changes.body == "Hello, this is a test message"
      assert changeset.changes.media == ["image1.jpg", "video1.mp4"]
      assert changeset.changes.bot == "chatbot_1"
      assert changeset.changes.call_id == "call_123"
      assert changeset.changes.call_type == "video"
      assert changeset.changes.call_status == "active"
      assert changeset.changes.call_link == "https://call.example.com/123"
      assert changeset.changes.timeout_ref == "ref_123"
      assert changeset.changes.data == %{"metadata" => %{"source" => "mobile"}}

      chat_struct = Ecto.Changeset.apply_changes(changeset)
      assert chat_struct.status == "unread"
    end

    test "sets default values for array and status fields" do
      changeset = Chat.changeset(%Chat{}, valid_chat_attrs())
      chat_struct = Ecto.Changeset.apply_changes(changeset)

      assert chat_struct.media == []
      assert chat_struct.status == "unread"
    end

    test "converts id fields to strings" do
      attrs = %{
        user_id: 123,
        recipient_id: 456,
        body: "Test message",
        id: 789,
        ai_chat_id: 101112
      }
      changeset = Chat.changeset(%Chat{}, attrs)

      assert changeset.valid?
      assert changeset.changes.id == "789"
      assert changeset.changes.ai_chat_id == "101112"
      assert changeset.changes.user_id == "123"
      assert changeset.changes.recipient_id == "456"
    end
  end

  describe "changeset/2 validation errors" do
    test "requires user_id" do
      attrs = valid_chat_attrs() |> Map.delete(:user_id)
      changeset = Chat.changeset(%Chat{}, attrs)

      refute changeset.valid?
      assert %{user_id: ["can't be blank"]} = errors_on(changeset)
    end

    test "requires recipient_id" do
      attrs = valid_chat_attrs() |> Map.delete(:recipient_id)
      changeset = Chat.changeset(%Chat{}, attrs)

      refute changeset.valid?
      assert %{recipient_id: ["can't be blank"]} = errors_on(changeset)
    end

    test "requires body" do
      attrs = valid_chat_attrs() |> Map.delete(:body)
      changeset = Chat.changeset(%Chat{}, attrs)

      refute changeset.valid?
      assert %{body: ["can't be blank"]} = errors_on(changeset)
    end
  end

  describe "erl_changeset/1" do
    test "converts erlang tuple to changeset successfully" do
      {:ok, chat} = Chat.erl_changeset(sample_erlang_tuple())

      assert %Chat{} = chat
      assert chat.id == "chat_123"
      assert chat.ai_chat_id == "ai_chat_789"
      assert chat.user_id == "user_123"
      assert chat.recipient_id == "recipient_456"
      assert chat.body == "Hello, this is a test message"
      assert chat.media == ["image1.jpg"]
      assert chat.bot == "chatbot_1"
      assert chat.call_id == "call_123"
      assert chat.call_type == "video"
      assert chat.call_status == "active"
      assert chat.call_link == "https://call.example.com/123"
      assert chat.timeout_ref == "ref_123"
      assert chat.data == %{"metadata" => %{"source" => "mobile"}}
      assert chat.status == "unread"
    end

    test "handles undefined datetime fields" do
      tuple_with_undefined = put_elem(sample_erlang_tuple(), 8, :undefined)
      {:ok, chat} = Chat.erl_changeset(tuple_with_undefined)

      assert is_nil(chat.date_created)
    end

    test "returns error for :notfound" do
      assert Chat.erl_changeset(:notfound) == {:error, :notfound}
    end

    test "returns error for invalid record" do
      assert Chat.erl_changeset({:invalid}) == {:error, :invalid_chat_record}
    end
  end

  describe "complex field validations" do
    test "handles array fields correctly" do
      attrs = %{
        user_id: "user_123",
        recipient_id: "recipient_456",
        body: "Test message",
        media: ["image1.jpg", "image2.jpg"]
      }

      changeset = Chat.changeset(%Chat{}, attrs)
      assert changeset.valid?

      chat_struct = Ecto.Changeset.apply_changes(changeset)
      assert length(chat_struct.media) == 2
    end

    test "handles map fields correctly" do
      attrs = %{
        user_id: "user_123",
        recipient_id: "recipient_456",
        body: "Test message",
        data: %{"metadata" => %{"source" => "web", "version" => "1.0"}}
      }

      changeset = Chat.changeset(%Chat{}, attrs)
      assert changeset.valid?

      chat_struct = Ecto.Changeset.apply_changes(changeset)
      assert chat_struct.data["metadata"]["source"] == "web"
    end
  end

  defp errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {message, opts} ->
      Regex.replace(~r"%{(\w+)}", message, fn _, key ->
        opts |> Keyword.get(String.to_existing_atom(key), key) |> to_string()
      end)
    end)
  end
end
