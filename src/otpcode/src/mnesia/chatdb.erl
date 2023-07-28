-module(chatdb).
-author("Zaryn Technologies").
-export([send_msg/3, get_msg/1, get_all_msg/1, edit_msg/2, delete_msg/1, list_chats/0]). 

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

send_msg(UserID, RecipientID, Body) ->
        Fun = fun() ->
                Id = nanoid:gen(),
                mnesia:write(#chat{id = Id,
                                   user_id = UserID,
                                   recipient_id = RecipientID,
                                   body = erl_deen:main(Body),
                                   date_created = calendar:universal_time()}),
                [RecipientUser] = mnesia:read(user, RecipientID),
                Chats = RecipientUser#user.chat,
                mnesia:write(RecipientUser#user{chat = [Id|Chats]}),
                Id
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

edit_msg(ChatID, NewContent) ->
    Fun = fun() ->
            [Chat] = mnesia:read({chat, ChatID}),
            mnesia:write(Chat#chat{body = NewContent}),
            ChatID
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

delete_msg(ChatID) ->
    Fun = fun() ->
        mnesia:delete({chat, ChatID})
    end,
    mnesia:activity(transaction, Fun).

list_chats() ->
    Fun = fun() ->
        mnesia:all_keys(chat)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
