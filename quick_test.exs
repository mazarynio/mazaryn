# Quick inline test
Application.ensure_all_started(:mazaryn)

IO.puts("\n🧪 Quick Video Call Test\n")

case Account.Users.list_users() do
  {:ok, [user1, user2 | _]} ->
    IO.puts("Testing: #{user1.username} → #{user2.username}")
    
    case Mazaryn.Chats.start_video_call(user1, user2) do
      {:ok, call_id} ->
        IO.puts("✅ Call initiated: #{call_id}")
        
        case Mazaryn.Chats.get_by_chat_id(call_id) do
          {:ok, chat} ->
            IO.puts("✅ Record created")
            IO.puts("   Status: #{chat.call_status}")
            IO.puts("   Link: #{chat.call_link}")
            IO.puts("\n🎉 SUCCESS!\n")
          error ->
            IO.puts("❌ Error: #{inspect(error)}\n")
        end
      
      error ->
        IO.puts("❌ Failed: #{inspect(error)}\n")
    end
  
  _ ->
    IO.puts("❌ Need at least 2 users\n")
end
