defmodule ChatTest do
  def start() do
    sender_id = ~c"Am@JXu^n94Sm36cZ$}gE<?EB%%T@O9F7{dqE!2]VW<d?nK_a[uNB275ShlZLJ"
    receiver_id = ~c">6zbNSlQ@LNxCzYy@DkYTBzZe8IhZsTF3%JJ0z<{e3w]bXBLXlPXi]sv-}$Jv"

    actor = %Account.User{id: sender_id, username: "mina"}
    recipient = %Account.User{id: receiver_id, username: "mona"}

    chat_params = %{
      body: "Hello!",
      media: nil,
      user_id: sender_id,
      recipient_id: receiver_id
    }

    result = Mazaryn.Chats.create_chat(actor, recipient, chat_params)
    result
  end
end
