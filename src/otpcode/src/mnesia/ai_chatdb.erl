-module(ai_chatdb).
-author("Zaryn Technologies").
-include("../records.hrl"). 
-export([insert/1, get_ai_chat_by_ai_id/1, get_ai_chat_by_chat_id/1]).

insert(ChatID) ->
    Fun = fun() ->
        ID = key_guardian:gen_address(80),
        AI_Chat = #ai_chat{
            id = ID,
            chat_id = ChatID
        },
        mnesia:write(AI_Chat),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_ai_chat_by_ai_id(ID) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#ai_chat{id = ID, _= '_'})
        end),
    case Res of
      {atomic, []} -> ai_chat_not_exist;
      {atomic, [Chat]} -> Chat;
      _ -> error
end.

get_ai_chat_by_chat_id(ID) ->
    Chat = chatdb:get_message(ID),
    AI_Chat_ID = Chat#chat.ai_chat_id,
    AI_Chat = get_ai_chat_by_ai_id(AI_Chat_ID),
    AI_Chat.