defmodule VideoCallTest do
  @moduledoc """
  Test script to verify video call functionality between two users.
  Run with: elixir test_video_call.exs
  """

  require Logger

  def run do
    IO.puts("\n")
    IO.puts("═══════════════════════════════════════════════════════════")
    IO.puts("🎬 VIDEO CALL INTEGRATION TEST")
    IO.puts("═══════════════════════════════════════════════════════════")
    IO.puts("\n")

    {user1, user2} = setup_test_users()

    if user1 && user2 do
      IO.puts("✅ Test users retrieved successfully")
      IO.puts("   👤 User 1: #{user1.username} (#{String.slice(to_string(user1.id), 0..20)}...)")
      IO.puts("   👤 User 2: #{user2.username} (#{String.slice(to_string(user2.id), 0..20)}...)")
      IO.puts("\n")

      run_test_sequence(user1, user2)
    else
      IO.puts("❌ Failed to get test users")
      IO.puts("   Make sure you have at least 2 users in your database")
      {:error, :no_users}
    end
  end

  defp setup_test_users do
    IO.puts("📋 Setting up test users...")

    case Account.Users.list_users() do
      {:ok, users} when length(users) >= 2 ->
        [user1, user2 | _] = Enum.take(users, 2)
        {user1, user2}

      {:ok, users} when length(users) < 2 ->
        IO.puts("⚠️  Only #{length(users)} user(s) found. Need at least 2 users.")
        {nil, nil}

      {:error, reason} ->
        IO.puts("❌ Error listing users: #{inspect(reason)}")
        {nil, nil}
    end
  end

  defp run_test_sequence(caller, recipient) do
    IO.puts("═══════════════════════════════════════════════════════════")
    IO.puts("🧪 TEST SEQUENCE START")
    IO.puts("═══════════════════════════════════════════════════════════")
    IO.puts("\n")

    test_initiate_call(caller, recipient)

    :timer.sleep(1000)

    test_call_record_exists()

    :timer.sleep(1000)

    test_accept_call()

    :timer.sleep(1000)

    test_end_call()

    IO.puts("\n")
    IO.puts("═══════════════════════════════════════════════════════════")
    IO.puts("✅ TEST SEQUENCE COMPLETE")
    IO.puts("═══════════════════════════════════════════════════════════")
  end

  defp test_initiate_call(caller, recipient) do
    IO.puts("───────────────────────────────────────────────────────────")
    IO.puts("TEST 1: Initiating Video Call")
    IO.puts("───────────────────────────────────────────────────────────")
    IO.puts("📞 #{caller.username} calling #{recipient.username}...")
    IO.puts("\n")

    case Mazaryn.Chats.start_video_call(caller, recipient) do
      {:ok, call_id} ->
        IO.puts("✅ SUCCESS: Video call initiated")
        IO.puts("   📋 Call ID: #{call_id}")

        Process.put(:test_call_id, call_id)

        case Mazaryn.Chats.get_by_chat_id(call_id) do
          {:ok, chat} ->
            IO.puts("   ✅ Chat record created successfully")
            IO.puts("   📋 Chat ID: #{String.slice(chat.id, 0..20)}...")
            IO.puts("   📞 Call link: #{chat.call_link}")
            IO.puts("   📊 Call status: #{chat.call_status}")
            IO.puts("   📞 Call type: #{chat.call_type}")
            IO.puts("   👤 Caller: #{String.slice(chat.user_id, 0..20)}...")
            IO.puts("   👥 Recipient: #{String.slice(chat.recipient_id, 0..20)}...")

            if chat.call_link && String.contains?(chat.call_link, "ws://localhost:2020") do
              IO.puts("   ✅ WebSocket link looks correct")
            else
              IO.puts("   ⚠️  WebSocket link might be incorrect: #{chat.call_link}")
            end

            {:ok, call_id}

          {:error, reason} ->
            IO.puts("   ❌ FAILED to get chat record: #{inspect(reason)}")
            {:error, reason}
        end

      {:error, reason} ->
        IO.puts("❌ FAILED: Could not initiate call")
        IO.puts("   Error: #{inspect(reason)}")
        {:error, reason}
    end

    IO.puts("\n")
  end

  defp test_call_record_exists do
    IO.puts("───────────────────────────────────────────────────────────")
    IO.puts("TEST 2: Verifying Call Record")
    IO.puts("───────────────────────────────────────────────────────────")

    call_id = Process.get(:test_call_id)

    if call_id do
      IO.puts("🔍 Checking if call #{call_id} exists...")
      IO.puts("\n")

      case Mazaryn.Chats.get_by_chat_id(call_id) do
        {:ok, chat} ->
          IO.puts("✅ SUCCESS: Call record found")
          IO.puts("   📋 Record ID: #{String.slice(chat.id, 0..20)}...")
          IO.puts("   📊 Status: #{chat.call_status}")
          IO.puts("   🔗 Link: #{chat.call_link}")

          required_fields = [
            {:id, chat.id},
            {:call_id, chat.call_id},
            {:call_link, chat.call_link},
            {:call_status, chat.call_status},
            {:user_id, chat.user_id},
            {:recipient_id, chat.recipient_id}
          ]

          IO.puts("\n   📋 Field verification:")
          Enum.each(required_fields, fn {field, value} ->
            if value && value != "" do
              IO.puts("      ✅ #{field}: Present")
            else
              IO.puts("      ❌ #{field}: Missing or empty")
            end
          end)

          {:ok, chat}

        {:error, reason} ->
          IO.puts("❌ FAILED: Call record not found")
          IO.puts("   Error: #{inspect(reason)}")
          {:error, reason}
      end
    else
      IO.puts("⚠️  SKIPPED: No call_id from previous test")
    end

    IO.puts("\n")
  end

  defp test_accept_call do
    IO.puts("───────────────────────────────────────────────────────────")
    IO.puts("TEST 3: Accepting Video Call")
    IO.puts("───────────────────────────────────────────────────────────")

    call_id = Process.get(:test_call_id)

    if call_id do
      IO.puts("📞 Accepting call #{call_id}...")
      IO.puts("\n")

      case Mazaryn.Chats.accept_call(call_id) do
        {:ok, result} ->
          IO.puts("✅ SUCCESS: Call accepted")
          IO.puts("   📋 Result: #{inspect(result)}")

          case Mazaryn.Chats.get_by_chat_id(call_id) do
            {:ok, chat} ->
              IO.puts("   📊 Updated status: #{chat.call_status}")

              if chat.call_status == "connected" do
                IO.puts("   ✅ Status correctly updated to 'connected'")
              else
                IO.puts("   ⚠️  Status is '#{chat.call_status}', expected 'connected'")
              end

              if chat.call_start_time do
                IO.puts("   ✅ Call start time recorded: #{chat.call_start_time}")
              else
                IO.puts("   ⚠️  Call start time not set")
              end

            {:error, reason} ->
              IO.puts("   ⚠️  Could not verify updated status: #{inspect(reason)}")
          end

          {:ok, result}

        {:error, reason} ->
          IO.puts("❌ FAILED: Could not accept call")
          IO.puts("   Error: #{inspect(reason)}")
          {:error, reason}
      end
    else
      IO.puts("⚠️  SKIPPED: No call_id from previous test")
    end

    IO.puts("\n")
  end

  defp test_end_call do
    IO.puts("───────────────────────────────────────────────────────────")
    IO.puts("TEST 4: Ending Video Call")
    IO.puts("───────────────────────────────────────────────────────────")

    call_id = Process.get(:test_call_id)

    if call_id do
      IO.puts("📞 Ending call #{call_id}...")
      IO.puts("\n")

      case Mazaryn.Chats.end_call(call_id) do
        {:ok, result} ->
          IO.puts("✅ SUCCESS: Call ended")
          IO.puts("   📋 Result: #{inspect(result)}")

          case Mazaryn.Chats.get_by_chat_id(call_id) do
            {:ok, chat} ->
              IO.puts("   📊 Final status: #{chat.call_status}")

              if chat.call_status == "ended" do
                IO.puts("   ✅ Status correctly updated to 'ended'")
              else
                IO.puts("   ⚠️  Status is '#{chat.call_status}', expected 'ended'")
              end

              if chat.call_end_time do
                IO.puts("   ✅ Call end time recorded: #{chat.call_end_time}")
              else
                IO.puts("   ⚠️  Call end time not set")
              end

              if chat.call_start_time && chat.call_end_time do
                duration = DateTime.diff(chat.call_end_time, chat.call_start_time, :second)
                IO.puts("   ⏱️  Call duration: #{duration} seconds")
              end

            {:error, reason} ->
              IO.puts("   ⚠️  Could not verify final status: #{inspect(reason)}")
          end

          {:ok, result}

        {:error, reason} ->
          IO.puts("❌ FAILED: Could not end call")
          IO.puts("   Error: #{inspect(reason)}")
          {:error, reason}
      end
    else
      IO.puts("⚠️  SKIPPED: No call_id from previous test")
    end

    IO.puts("\n")
  end

  def check_erlang_database do
    IO.puts("\n")
    IO.puts("═══════════════════════════════════════════════════════════")
    IO.puts("🔍 ERLANG DATABASE INSPECTION")
    IO.puts("═══════════════════════════════════════════════════════════")
    IO.puts("\n")

    IO.puts("📋 Listing all chats...")

    case :chat_server.list_chats() do
      chats when is_list(chats) ->
        IO.puts("✅ Found #{length(chats)} chat(s)")

        Enum.each(chats, fn chat_id ->
          case :chat_server.get_msg(chat_id) do
            {:chat, id, _ai, user_id, recipient_id, _body, _media, _bot, _created, _updated, call_id, call_type, call_status, call_link, _start, _end, _ref, _data} ->
              IO.puts("\n   📝 Chat: #{id}")
              IO.puts("      👤 From: #{user_id}")
              IO.puts("      👥 To: #{recipient_id}")

              if call_id != "" do
                IO.puts("      📞 Call ID: #{call_id}")
                IO.puts("      📊 Call Status: #{call_status}")
                IO.puts("      🔗 Call Link: #{call_link}")
                IO.puts("      📞 Call Type: #{call_type}")
              end

            _ ->
              IO.puts("   ⚠️  Could not parse chat: #{inspect(chat_id)}")
          end
        end)

      error ->
        IO.puts("❌ Error listing chats: #{inspect(error)}")
    end

    IO.puts("\n")
  end

  def quick_test do

    {user1, user2} = setup_test_users()

    if user1 && user2 do
      IO.puts("📞 Testing call from #{user1.username} to #{user2.username}...")
      IO.puts("\n")

      case Mazaryn.Chats.start_video_call(user1, user2) do
        {:ok, call_id} ->
          IO.puts("✅ Call initiated successfully!")
          IO.puts("   📋 Call ID: #{call_id}")

          case Mazaryn.Chats.get_by_chat_id(call_id) do
            {:ok, chat} ->
              IO.puts("   ✅ Chat record retrieved")
              IO.puts("   🔗 Link: #{chat.call_link}")
              IO.puts("   📊 Status: #{chat.call_status}")

              IO.puts("\n🎉 SUCCESS! Video call system is working!")
              {:ok, call_id}

            error ->
              IO.puts("   ❌ Failed to retrieve chat: #{inspect(error)}")
              error
          end

        error ->
          IO.puts("❌ Failed to initiate call: #{inspect(error)}")
          error
      end
    else
      IO.puts("❌ Could not get test users")
    end

    IO.puts("\n")
  end
end

Application.ensure_all_started(:mazaryn)

IO.puts("\n🎬 Choose a test mode:")
IO.puts("   1. Full test (all steps)")
IO.puts("   2. Quick test (just initiate call)")
IO.puts("   3. Database inspection")
IO.puts("\n")

choice = IO.gets("Enter choice (1/2/3): ") |> String.trim()

case choice do
  "1" -> VideoCallTest.run()
  "2" -> VideoCallTest.quick_test()
  "3" -> VideoCallTest.check_erlang_database()
  _ ->
    IO.puts("Running full test by default...\n")
    VideoCallTest.run()
end
