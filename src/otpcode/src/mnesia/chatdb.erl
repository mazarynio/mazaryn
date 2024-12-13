-module(chatdb).
-author("Zaryn Technologies").
-export([send_msg/4, get_msg/1, get_all_msg/1, edit_msg/2, delete_msg/1, list_chats/0]). 

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

% Send Message (UserID, RecipientID, Body, Media)
send_msg(UserID, RecipientID, Body, Media) -> 
    Fun = fun() ->
        case mnesia:read({user, UserID}) of
            [SenderUser] ->
                case mnesia:read({user, RecipientID}) of
                    [RecipientUser] ->
                        Id = nanoid:gen(),
                        AI_Chat_ID = ai_chatdb:insert(Id),
                        Date = calendar:universal_time(),
                        SenderChats = SenderUser#user.chat,
                        RecipientChats = RecipientUser#user.chat,
                        
                        % Write the new chat
                        mnesia:write(#chat{
                            id = Id,
                            ai_chat_id = AI_Chat_ID,
                            user_id = UserID,
                            recipient_id = RecipientID,
                            body = Body,
                            media = Media,
                            date_created = Date
                        }),
                        
                        % Update sender and recipient chats
                        mnesia:write(SenderUser#user{chat = [Id | SenderChats]}),
                        mnesia:write(RecipientUser#user{chat = [Id | RecipientChats]}),
                        
                        Id; 
                    [] ->
                        throw({error, recipient_not_found})
                end;
            [] ->
                throw({error, sender_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Id} ->
            Id; % Return only the ID
        {aborted, {error, Error}} ->
            throw(Error);
        {aborted, Reason} ->
            throw({transaction_failed, Reason})
    end.


% Get Message using ChatID
get_msg(ChatID) ->
    Fun = fun() ->
        case mnesia:read({chat, ChatID}) of
            [Chat] -> 
                Chat; 
            [] -> 
                throw(chat_not_found) 
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Chat} ->
            Chat; 
        {aborted, chat_not_found} ->
            throw(chat_not_found); 
        {aborted, Reason} ->
            throw({transaction_failed, Reason}) 
    end.

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
        case mnesia:read({chat, ChatID}) of
            [Chat] ->
                NewChat = Chat#chat{body = NewContent, date_updated = Date},
                mnesia:write(NewChat),
                ChatID; 
            [] ->
                throw(chat_not_found) 
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ChatID} ->
            ChatID; 
        {aborted, chat_not_found} ->
            throw(chat_not_found); 
        {aborted, Reason} ->
            throw({transaction_failed, Reason}) 
    end.



% Delete MEssage using ChatID
delete_msg(ChatID) ->
    Fun = fun() ->
        case mnesia:read({chat, ChatID}) of
            [_Chat] ->
                mnesia:delete({chat, ChatID}),
                {ok, ChatID};
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:activity(transaction, Fun) of
        {ok, Res} ->
            {ok, Res};
        {error, not_found} ->
            {error, chat_not_found};
        {aborted, Reason} ->
            {error, {transaction_failed, Reason}}
    end.


list_chats() ->
    Fun = fun() ->
        mnesia:all_keys(chat)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res. 


