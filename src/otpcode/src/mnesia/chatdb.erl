-module(chatdb).
-export([send_msg/3, get_msg/1, get_all_msg/1]). 

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

send_msg(UserID, RecipientID, Body) ->
    Fun = fun() ->
            Id = nanoid:gen(),
            mnesia:write(#chat{id = Id,
                               user_id = UserID,
                               recipient_id = RecipientID,
                               body = Body,
                               date_created = calendar:universal_time()}),
            [RecipientUser] = mnesia:read(user, RecipientID),
            Chats = RecipientUser#user.chat,
            mnesia:write(RecipientUser#user{chat = [Id|Chats]}),
            io:fwrite("~p~n", [Id]),
            io:fwrite("~p~n", [Body])
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_msg(ChatID) ->
    Fun = fun() ->
            [Chat] = mnesia:read({chat, ChatID}),
            Chat
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_all_msg(RecipientID) -> 
    Fun = fun() ->
            mnesia:match_object(#chat{recipient_id = RecipientID,
                                                _ = '_'}),
            [User] = mnesia:read({user, RecipientID}),
            lists:foldl(fun(ID, Acc) ->
                            [Chat] = mnesia:read({chat, ID}),
                            [Chat|Acc]
                        end,
                        [], User#user.chat)
      
            end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.