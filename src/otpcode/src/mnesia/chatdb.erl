-module(chatdb).
-author("Zaryn Technologies").
-export([send_msg/4, get_msg/1, get_all_msg/1, edit_msg/2, delete_msg/1, list_chats/0, update_presence/2, accept_call/1,
 start_video_call/2, end_video_call/1, handle_call_timeout/1]). 

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
                        
                        mnesia:write(#chat{
                            id = Id,
                            ai_chat_id = AI_Chat_ID,
                            user_id = UserID,
                            recipient_id = RecipientID,
                            body = Body,
                            media = Media,
                            date_created = Date
                        }),
                        
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

generate_call_id() ->
    CallID = integer_to_list(rand:uniform(99999999999)),
    case mnesia:match_object(#chat{call_id = CallID, _ = '_'}) of
        [] -> CallID;
        [_] -> generate_call_id()
    end.

update_presence(UserID, Status) when Status == online; Status == offline ->
    mnesia:transaction(fun() ->
        mnesia:write(#presence{
            user_id = UserID,
            status = Status,
            last_updated = calendar:universal_time()
        })
    end).

is_user_online(UserID) ->
    case mnesia:dirty_read({presence, UserID}) of
        [#presence{status = online}] -> true;
        _ -> false
    end.

start_video_call(UserID, TargetID) ->
    Fun = fun() ->
        case mnesia:read({user, UserID}) of
            [SenderUser] ->
                case mnesia:read({user, TargetID}) of
                    [RecipientUser] ->
                        CallID = generate_call_id(),
                        Date = calendar:universal_time(),
                        CallLink = list_to_binary("ws://localhost:2020/ws/signaling/" ++ CallID),
                        SenderChats = SenderUser#user.chat,
                        RecipientChats = RecipientUser#user.chat,

                        Url = "http://localhost:2020/call/initiate",
                        Body = #{user_id => iolist_to_binary(UserID),
                                 target_id => iolist_to_binary(TargetID),
                                 call_id => list_to_binary(CallID)},
                        Headers = [{"Content-Type", "application/json"}],
                        Request = {Url, Headers, "application/json", jiffy:encode(Body)},

                        case httpc:request(post, Request, [], []) of
                            {ok, {{_, 200, _}, _, Response}} ->
                                Decoded = jiffy:decode(Response, [return_maps]),
                                #{<<"status">> := <<"initiated">>, <<"call_id">> := CallIDBin} = Decoded,
                                CallID = binary_to_list(CallIDBin),

                                TimeoutRef = erlang:send_after(30000, self(), {call_timeout, CallID}),

                                IsRecipientOnline = is_user_online(TargetID),
                                InitialStatus = case IsRecipientOnline of
                                    true -> ringing;
                                    false -> missed
                                end,

                                Chat = #chat{
                                    id = nanoid:gen(),
                                    ai_chat_id = undefined,
                                    user_id = UserID,
                                    recipient_id = TargetID,
                                    body = undefined,
                                    media = [],
                                    bot = undefined,
                                    date_created = Date,
                                    date_updated = Date,
                                    call_id = CallID,
                                    call_type = video,
                                    call_status = InitialStatus,
                                    call_link = CallLink,
                                    call_start_time = Date,
                                    call_end_time = undefined,
                                    timeout_ref = TimeoutRef,
                                    data = #{}
                                },
                                mnesia:write(Chat),

                                mnesia:write(SenderUser#user{chat = [Chat#chat.id | SenderChats]}),
                                mnesia:write(RecipientUser#user{chat = [Chat#chat.id | RecipientChats]}),

                                case IsRecipientOnline of
                                    false ->
                                        log_missed_call(TargetID, Chat);
                                    true ->
                                        ok
                                end,

                                CallID;
                            {error, Reason} ->
                                throw({error, {rust_service_failed, Reason}})
                        end;
                    [] ->
                        throw({error, recipient_not_found})
                end;
            [] ->
                throw({error, sender_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, CallID} ->
            CallID;
        {aborted, {error, Error}} ->
            throw({error, Error});
        {aborted, Reason} ->
            throw({error, {transaction_failed, Reason}})
    end.

accept_call(CallID) ->
    Fun = fun() ->
        case mnesia:match_object(#chat{call_id = CallID, _ = '_'}) of
            [Chat = #chat{call_status = Status, timeout_ref = TimeoutRef}] when Status == ringing; Status == missed ->
                erlang:cancel_timer(TimeoutRef),
                Url = "http://localhost:2020/call/accept",
                Body = #{call_id => list_to_binary(CallID)},
                Headers = [{"Content-Type", "application/json"}],
                Request = {Url, Headers, "application/json", jiffy:encode(Body)},
                case httpc:request(post, Request, [], []) of
                    {ok, {{_, 200, _}, _, Response}} ->
                        Decoded = jiffy:decode(Response, [return_maps]),
                        #{<<"status">> := <<"connected">>, <<"call_id">> := CallIDBin} = Decoded,
                        CallID = binary_to_list(CallIDBin),
                        mnesia:write(Chat#chat{
                            call_status = connected,
                            timeout_ref = undefined,
                            date_updated = calendar:universal_time()
                        }),
                        CallID;
                    {error, Reason} ->
                        throw({error, {rust_service_failed, Reason}})
                end;
            [_] ->
                throw({error, invalid_call_state});
            [] ->
                throw({error, chat_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, CallID} ->
            CallID;
        {aborted, {error, Error}} ->
            throw({error, Error});
        {aborted, Reason} ->
            throw({error, {transaction_failed, Reason}})
    end.

end_video_call(CallID) ->
    Fun = fun() ->
        case mnesia:match_object(#chat{call_id = CallID, _ = '_'}) of
            [Chat = #chat{call_status = Status, timeout_ref = TimeoutRef}] when Status == ringing; Status == connected; Status == missed ->
                case TimeoutRef of
                    undefined -> ok;
                    Ref -> erlang:cancel_timer(Ref)
                end,
                Url = "http://localhost:2020/call/end",
                Body = #{call_id => list_to_binary(CallID)},
                Headers = [{"Content-Type", "application/json"}],
                Request = {Url, Headers, "application/json", jiffy:encode(Body)},
                case httpc:request(post, Request, [], []) of
                    {ok, {{_, 200, _}, _, Response}} ->
                        Decoded = jiffy:decode(Response, [return_maps]),
                        #{<<"status">> := <<"ended">>, <<"call_id">> := CallIDBin} = Decoded,
                        CallID = binary_to_list(CallIDBin),
                        mnesia:write(Chat#chat{
                            call_status = ended,
                            call_end_time = calendar:universal_time(),
                            timeout_ref = undefined,
                            date_updated = calendar:universal_time()
                        }),
                        CallID;
                    {error, Reason} ->
                        throw({error, {rust_service_failed, Reason}})
                end;
            [_] ->
                throw({error, invalid_call_state});
            [] ->
                throw({error, chat_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, CallID} ->
            CallID;
        {aborted, {error, Error}} ->
            throw({error, Error});
        {aborted, Reason} ->
            throw({error, {transaction_failed, Reason}})
    end.

% Handle call timeout
handle_call_timeout(CallID) ->
    mnesia:transaction(fun() ->
        case mnesia:match_object(#chat{call_id = CallID, _ = '_'}) of
            [Chat = #chat{call_status = Status, recipient_id = RecipientID}] when Status == ringing; Status == missed ->
                mnesia:write(Chat#chat{
                    call_status = failed,
                    call_end_time = calendar:universal_time(),
                    timeout_ref = undefined,
                    date_updated = calendar:universal_time()
                }),
                log_missed_call(RecipientID, Chat);
            _ ->
                ok
        end
    end).

% Log a missed call notification
log_missed_call(RecipientID, _Chat) ->
    Notif = #notif{
        id = nanoid:gen(),
        user_id = RecipientID,
        message = "Missed Call",
        date_created = calendar:universal_time()
    },
    mnesia:write(Notif).

