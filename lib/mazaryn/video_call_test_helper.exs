defmodule VideoCallTestHelper do

  require Logger

  def test_full do
    header("FULL VIDEO CALL TEST")

    case get_two_users() do
      {nil, nil} ->
        error("Need at least 2 users in database")
        :error

      {user1, user2} ->
        info("User 1: #{user1.username}")
        info("User 2: #{user2.username}")

        step("Starting call from #{user1.username} to #{user2.username}")
        case Mazaryn.Chats.start_video_call(user1, user2) do
          {:ok, call_id} ->
            success("Call started: #{call_id}")

            step("Verifying chat record")
            case Mazaryn.Chats.get_by_chat_id(call_id) do
              {:ok, chat} ->
                success("Chat record found")
                display_chat(chat)

                :timer.sleep(500)
                step("Accepting call")
                case Mazaryn.Chats.accept_call(call_id) do
                  {:ok, _} ->
                    success("Call accepted")

                    :timer.sleep(500)
                    step("Ending call")
                    case Mazaryn.Chats.end_call(call_id) do
                      {:ok, _} ->
                        success("Call ended")

                        case Mazaryn.Chats.get_by_chat_id(call_id) do
                          {:ok, final_chat} ->
                            success("Final state:")
                            display_chat(final_chat)
                            {:ok, :test_passed}
                          _ ->
                            error("Could not get final state")
                            :error
                        end

                      error ->
                        error("Failed to end call: #{inspect(error)}")
                        :error
                    end

                  error ->
                    error("Failed to accept call: #{inspect(error)}")
                    :error
                end

              error ->
                error("Failed to get chat: #{inspect(error)}")
                :error
            end

          error ->
            error("Failed to start call: #{inspect(error)}")
            :error
        end
    end
  end

  def test_quick do
    header("QUICK VIDEO CALL TEST")

    case get_two_users() do
      {nil, nil} ->
        error("Need at least 2 users in database")
        :error

      {user1, user2} ->
        info("Testing: #{user1.username} → #{user2.username}")

        case Mazaryn.Chats.start_video_call(user1, user2) do
          {:ok, call_id} ->
            success("Call initiated!")
            info("Call ID: #{call_id}")

            case Mazaryn.Chats.get_by_chat_id(call_id) do
              {:ok, chat} ->
                success("Chat record verified")
                display_chat(chat)
                {:ok, call_id}

              error ->
                error("Could not verify chat: #{inspect(error)}")
                :error
            end

          error ->
            error("Failed: #{inspect(error)}")
            :error
        end
    end
  end

  def test_self_call do
    header("SELF-CALL TEST (Should Fail)")

    case get_two_users() do
      {nil, nil} ->
        error("Need at least 1 user in database")
        :error

      {user1, _} ->
        info("Testing: #{user1.username} → #{user1.username}")

        case Mazaryn.Chats.start_video_call(user1, user1) do
          {:ok, _} ->
            error("UNEXPECTED: Self-call succeeded (should have failed!)")
            :error

          {:error, :self_call_not_allowed} ->
            success("CORRECT: Self-call was rejected")
            {:ok, :test_passed}

          {:error, reason} ->
            warning("Call failed with unexpected reason: #{inspect(reason)}")
            :error
        end
    end
  end

  def inspect_calls do
    header("VIDEO CALLS INSPECTION")

    case :chat_server.list_chats() do
      chats when is_list(chats) ->
        info("Found #{length(chats)} total chat records")

        video_calls = Enum.filter(chats, fn chat_id ->
          case :chat_server.get_msg(chat_id) do
            {:chat, _, _, _, _, _, _, _, _, _, call_id, _, _, _, _, _, _, _} ->
              call_id != "" and call_id != :undefined
            _ ->
              false
          end
        end)

        info("Found #{length(video_calls)} video call records")
        IO.puts("")

        Enum.each(video_calls, fn chat_id ->
          case :chat_server.get_msg(chat_id) do
            {:chat, id, _, user_id, recipient_id, _, _, _, created, _, call_id, call_type, call_status, call_link, start_time, end_time, _, _} ->
              IO.puts("  ┌─ Call: #{call_id}")
              IO.puts("  │  ID: #{id}")
              IO.puts("  │  From: #{user_id}")
              IO.puts("  │  To: #{recipient_id}")
              IO.puts("  │  Status: #{call_status}")
              IO.puts("  │  Type: #{call_type}")
              IO.puts("  │  Link: #{call_link}")
              IO.puts("  │  Created: #{created}")

              if start_time != :undefined and start_time != nil do
                IO.puts("  │  Started: #{start_time}")
              end

              if end_time != :undefined and end_time != nil do
                IO.puts("  │  Ended: #{end_time}")
              end

              IO.puts("  └─")
              IO.puts("")

            _ ->
              :ok
          end
        end)

        {:ok, length(video_calls)}

      error ->
        error("Failed to list chats: #{inspect(error)}")
        :error
    end
  end

  def list_users do
    header("USERS IN SYSTEM")

    case Account.Users.list_users() do
      {:ok, users} ->
        info("Found #{length(users)} users:")

        Enum.each(users, fn user ->
          IO.puts("  • #{user.username} (#{String.slice(to_string(user.id), 0..30)}...)")
        end)

        {:ok, length(users)}

      error ->
        error("Failed to list users: #{inspect(error)}")
        :error
    end
  end

  def cleanup_calls do
    header("CLEANUP VIDEO CALLS")

    warning("This will remove all video call data from chat records")

    case :chat_server.list_chats() do
      chats when is_list(chats) ->
        count = Enum.reduce(chats, 0, fn chat_id, acc ->
          case :chat_server.get_msg(chat_id) do
            {:chat, _, _, _, _, _, _, _, _, _, call_id, _, _, _, _, _, _, _} when call_id != "" and call_id != :undefined ->
              acc + 1
            _ ->
              acc
          end
        end)

        info("Found #{count} video call records to clean")
        success("Note: In production, you'd implement actual cleanup here")
        {:ok, count}

      error ->
        error("Failed: #{inspect(error)}")
        :error
    end
  end

  defp get_two_users do
    case Account.Users.list_users() do
      {:ok, users} when length(users) >= 2 ->
        [user1, user2 | _] = Enum.take(users, 2)
        {user1, user2}

      {:ok, users} when length(users) == 1 ->
        [user1] = users
        {user1, nil}

      _ ->
        {nil, nil}
    end
  end

  defp display_chat(chat) do
    IO.puts("")
    IO.puts("  📋 Chat Details:")
    IO.puts("     ID: #{String.slice(chat.id, 0..30)}...")
    IO.puts("     Call ID: #{chat.call_id}")
    IO.puts("     Call Link: #{chat.call_link}")
    IO.puts("     Call Status: #{chat.call_status}")
    IO.puts("     Call Type: #{chat.call_type}")
    IO.puts("     From: #{String.slice(chat.user_id, 0..30)}...")
    IO.puts("     To: #{String.slice(chat.recipient_id, 0..30)}...")

    if chat.call_start_time do
      IO.puts("     Started: #{chat.call_start_time}")
    end

    if chat.call_end_time do
      IO.puts("     Ended: #{chat.call_end_time}")

      if chat.call_start_time do
        duration = DateTime.diff(chat.call_end_time, chat.call_start_time, :second)
        IO.puts("     Duration: #{duration}s")
      end
    end

    IO.puts("")
  end

  defp header(text) do
    IO.puts("")
    IO.puts("═══════════════════════════════════════════════════════════")
    IO.puts("  #{text}")
    IO.puts("═══════════════════════════════════════════════════════════")
    IO.puts("")
  end

  defp step(text) do
    IO.puts("")
    IO.puts("  ▶ #{text}")
  end

  defp success(text) do
    IO.puts("    ✅ #{text}")
  end

  defp error(text) do
    IO.puts("    ❌ #{text}")
  end

  defp warning(text) do
    IO.puts("    ⚠️  #{text}")
  end

  defp info(text) do
    IO.puts("    ℹ️  #{text}")
  end
end
