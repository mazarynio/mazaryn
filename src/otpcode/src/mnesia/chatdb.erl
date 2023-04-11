-module(chatdb).
-author("Zaryn Technologies").
-export([
    create_chat/2,
    get_chat_by_peer_ids/2,
    send_msg/3,
    get_msg/1,
    get_all_msg/1,
    edit_msg/2,
    delete_msg/1
]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

create_chat(Peer_Ids, Title) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        mnesia:write(#chat{
            id = Id,
            peer_ids = Peer_Ids,
            title = Title,
            messages = [],
            inserted_at = calendar:universal_time(),
            updated_at = calendar:universal_time()
        }),
        [UserId, RecipientID] = Peer_Ids,
        [User] = mnesia:read(user, UserId),
        [RecipientUser] = mnesia:read(user, RecipientID),
        _ = mnesia:write(User#user{chat = [Id | User#user.chat]}),
        _ = mnesia:write(RecipientUser#user{chat = [Id | RecipientUser#user.chat]}),
        [Chat] = mnesia:read({chat, Id}),
        Chat
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_chat_by_peer_ids(UserID1, UserID2) ->
    Fun = fun() ->
        Query = #chat{peer_ids = [UserID1, UserID2]},
        mnesia:read(chat, Query)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

send_msg(UserID, RecipientID, Body) ->
    Fun = fun() ->
        ChatObject =
            case get_chat_by_peer_ids(UserID, RecipientID) of
                [] ->
                    Res = create_chat([UserID, RecipientID], "Chat"),
                    Res;
                [Chat] ->
                    Chat
            end,
        Id = nanoid:gen(),
        mnesia:write(#message{
            id = Id,
            chat_id = ChatObject#chat.id,
            user_id = UserID,
            recipient_id = RecipientID,
            body = Body,
            date_created = calendar:universal_time()
        }),
        [RecipientUser] = mnesia:read(user, RecipientID),
        Chats = RecipientUser#user.chat,
        mnesia:write(RecipientUser#user{chat = [Id | Chats]}),
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
        mnesia:match_object(#message{
            recipient_id = RecipientID,
            _ = '_'
        }),
        [User] = mnesia:read({user, RecipientID}),
        lists:foldl(
            fun(ID, Acc) ->
                [Chat] = mnesia:read({chat, ID}),
                [Chat | Acc]
            end,
            [],
            User#user.chat
        )
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

edit_msg(MessageID, NewContent) ->
    Fun = fun() ->
        [Message] = mnesia:read({message, MessageID}),
        mnesia:write(Message#message{body = NewContent}),
        MessageID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

delete_msg(MessageID) ->
    Fun = fun() ->
        mnesia:delete({message, MessageID})
    end,
    mnesia:activity(transaction, Fun).
