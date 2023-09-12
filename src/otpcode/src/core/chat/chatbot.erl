-module(chatbot).
-export([send_msg/2, send_msg2/2, get_msg/1, get_all_msg/1, call_python/1]).
-include("../../records.hrl").
-include_lib("stdlib/include/qlc.hrl").


send_msg2(UserID, Body) ->
    Fun = fun() ->
        ID = nanoid:gen(),
        mnesia:write(#chat{
                        id = ID,
                        user_id = UserID,
                        body = Body,
                        date_created = calendar:universal_time()}),
        [RecipientUser] = mnesia:read(user, UserID),
        Chats = RecipientUser#user.chat,
        mnesia:write(RecipientUser#user{chat = [ID|Chats]}),
        io:format("~p~n", [ID]),
        io:format("~p~n", [Body])
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res. 

send_msg(UserID, Body) ->
    Fun = fun() ->
            ID = nanoid:gen(),
            Bot = chatbot:call_python(Body),
            mnesia:write(#chat{
                            id = ID,
                            user_id = UserID,
                            body = Body,
                            bot = Bot,
                            date_created = calendar:universal_time()}),
            [RecipientUser] = mnesia:read(user, UserID),
            Chats = RecipientUser#user.chat,
            mnesia:write(RecipientUser#user{chat = [ID|Chats]}),
            io:format("~p~n", [ID]),
            io:format("~p~n", [Bot])
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
        mnesia:match_object(#chat{recipient_id = RecipientID, _ = '_'}),
        [User] = mnesia:read({user, RecipientID}),
        lists:foldl(fun(ID, Acc) ->
                        [Chat] = mnesia:read({chat, ID}),
                        [Chat|Acc]
                    end,
                    [], User#user.chat)
        end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.


call_python(Prompt) ->
    Command = "python3 /home/zaryn/mazaryn/src/otpcode/pycode/chat/chatgpt.py '" ++ Prompt ++ "'",
    Response = os:cmd(Command),
    Response.  
