-module(chatdb).
-author("Zaryn Technologies").
-export([send_msg/4, get_msg/1, get_all_msg/1, edit_msg/2, delete_msg/1, list_chats/0]). 

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

% Send Message (UserID, RecipientID, Body, Media)
send_msg(UserID, RecipientID, Body, Media) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        Date = calendar:universal_time(),
        [SenderUser] = mnesia:read(user, UserID),
        SenderChats = SenderUser#user.chat,
        mnesia:write(#chat{
            id = Id,
            user_id = UserID,
            recipient_id = RecipientID,
            body = erl_deen:main(Body),
            media = Media,
            date_created = Date
        }),
        [RecipientUser] = mnesia:read(user, RecipientID),
        RecipientChats = RecipientUser#user.chat,
        mnesia:write(RecipientUser#user{chat = [Id | RecipientChats]}),
        mnesia:write(SenderUser#user{chat = [Id | SenderChats]}),
        Id
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

% Get Message using ChatID
get_msg(ChatID) ->
    Fun = fun() ->
            [Chat] = mnesia:read({chat, ChatID}),
            Chat
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

%% Get all the Msssages Sent to User using UserID 
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

%% Edit Message adding new Content 
edit_msg(ChatID, NewContent) ->
    Fun = fun() ->
        Date = calendar:universal_time(),
        [Chat] = mnesia:read({chat, ChatID}),
        NewChat = Chat#chat{body = NewContent, date_updated = Date},
        mnesia:dirty_write(NewChat),
        ChatID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

% Delete MEssage using ChatID
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


