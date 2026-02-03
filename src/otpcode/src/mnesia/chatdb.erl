-module(chatdb).
-author("Zaryn Technologies").
-export([send_msg/4, get_msg/1, get_chat_by_id/1, get_all_msg/1, edit_msg/2, delete_msg/1, list_chats/0, update_presence/2, accept_call/1,
 start_video_call/2, end_video_call/1, get_chat_by_call_id/1, handle_call_timeout/1]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

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
            Id;
        {aborted, {error, Error}} ->
            throw(Error);
        {aborted, Reason} ->
            throw({transaction_failed, Reason})
    end.

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

get_chat_by_id(ChatId) when is_list(ChatId) ->
    case mnesia:dirty_read(chat, ChatId) of
        [Chat] -> Chat;
        [] -> notfound
    end.

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
    io:format("~n========================================~n"),
    io:format("[START VIDEO CALL] Initiating call~n"),
    io:format("[START VIDEO CALL] UserID: ~p~n", [UserID]),
    io:format("[START VIDEO CALL] TargetID: ~p~n", [TargetID]),
    io:format("========================================~n"),

    Fun = fun() ->
        case mnesia:read({user, UserID}) of
            [SenderUser] ->
                io:format("[START VIDEO CALL] Sender user found: ~p~n", [binary_to_list(SenderUser#user.username)]),
                case mnesia:read({user, TargetID}) of
                    [RecipientUser] ->
                        io:format("[START VIDEO CALL] Recipient user found: ~p~n", [binary_to_list(RecipientUser#user.username)]),

                        CallID = generate_call_id(),
                        io:format("[START VIDEO CALL] Generated CallID: ~p~n", [CallID]),

                        Date = calendar:universal_time(),
                        CallLink = list_to_binary("ws://localhost:2020/ws/signaling/" ++ CallID),
                        io:format("[START VIDEO CALL] Call link: ~p~n", [CallLink]),

                        SenderChats = SenderUser#user.chat,
                        RecipientChats = RecipientUser#user.chat,

                        Url = "http://localhost:2020/call/initiate",
                        Body = #{user_id => iolist_to_binary(UserID),
                                 target_id => iolist_to_binary(TargetID),
                                 call_id => list_to_binary(CallID)},
                        Headers = [{"Content-Type", "application/json"}],
                        Request = {Url, Headers, "application/json", jiffy:encode(Body)},

                        io:format("[START VIDEO CALL] Calling Rust service at: ~p~n", [Url]),
                        io:format("[START VIDEO CALL] Request body: ~p~n", [Body]),

                        case httpc:request(post, Request, [], []) of
                            {ok, {{_, 200, _}, _, Response}} ->
                                io:format("[START VIDEO CALL] Rust service response: ~p~n", [Response]),

                                Decoded = jiffy:decode(Response, [return_maps]),
                                #{<<"status">> := <<"initiated">>, <<"call_id">> := CallIDBin} = Decoded,
                                CallID = binary_to_list(CallIDBin),

                                io:format("[START VIDEO CALL] Call initiated successfully~n"),

                                TimeoutRef = erlang:send_after(30000, self(), {call_timeout, CallID}),
                                io:format("[START VIDEO CALL] Timeout timer set: ~p~n", [TimeoutRef]),

                                IsRecipientOnline = is_user_online(TargetID),
                                io:format("[START VIDEO CALL] Is recipient online: ~p~n", [IsRecipientOnline]),

                                InitialStatus = case IsRecipientOnline of
                                    true -> ringing;
                                    false -> missed
                                end,
                                io:format("[START VIDEO CALL] Initial call status: ~p~n", [InitialStatus]),

                                ChatId = nanoid:gen(),
                                Chat = #chat{
                                    id = ChatId,
                                    ai_chat_id = "",
                                    user_id = UserID,
                                    recipient_id = TargetID,
                                    body = "Video call initiated",
                                    media = [],
                                    bot = "",
                                    date_created = Date,
                                    date_updated = Date,
                                    call_id = CallID,
                                    call_type = "video",
                                    call_status = InitialStatus,
                                    call_link = CallLink,
                                    call_start_time = Date,
                                    call_end_time = undefined,
                                    timeout_ref = term_to_binary(TimeoutRef),
                                    data = #{}
                                },

                                io:format("[START VIDEO CALL] Writing chat record~n"),
                                mnesia:write(Chat),

                                io:format("[START VIDEO CALL] Updating user chat lists~n"),
                                mnesia:write(SenderUser#user{chat = [ChatId | SenderChats]}),
                                mnesia:write(RecipientUser#user{chat = [ChatId | RecipientChats]}),

                                case IsRecipientOnline of
                                    false ->
                                        io:format("[START VIDEO CALL] Logging missed call~n"),
                                        log_missed_call(TargetID, Chat);
                                    true ->
                                        io:format("[START VIDEO CALL] Recipient is online, no missed call log~n"),
                                        ok
                                end,

                                io:format("[START VIDEO CALL] Call setup complete, returning CallID: ~p~n", [CallID]),
                                io:format("========================================~n"),
                                CallID;
                            {error, Reason} ->
                                io:format("[START VIDEO CALL] ERROR: Rust service failed: ~p~n", [Reason]),
                                io:format("========================================~n"),
                                throw({error, {rust_service_failed, Reason}})
                        end;
                    [] ->
                        io:format("[START VIDEO CALL] ERROR: Recipient not found~n"),
                        io:format("========================================~n"),
                        throw({error, recipient_not_found})
                end;
            [] ->
                io:format("[START VIDEO CALL] ERROR: Sender not found~n"),
                io:format("========================================~n"),
                throw({error, sender_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, CallID} ->
            io:format("[START VIDEO CALL] Transaction successful, CallID: ~p~n", [CallID]),
            CallID;
        {aborted, {error, Error}} ->
            io:format("[START VIDEO CALL] ERROR: Transaction aborted: ~p~n", [Error]),
            throw({error, Error});
        {aborted, Reason} ->
            io:format("[START VIDEO CALL] ERROR: Transaction failed: ~p~n", [Reason]),
            throw({error, {transaction_failed, Reason}})
    end.

accept_call(CallID) ->
    io:format("~n========================================~n"),
    io:format("[ACCEPT CALL] Accepting call~n"),
    io:format("[ACCEPT CALL] CallID: ~p~n", [CallID]),
    io:format("========================================~n"),

    Fun = fun() ->
        case mnesia:match_object(#chat{call_id = CallID, _ = '_'}) of
            [Chat = #chat{call_status = Status, timeout_ref = TimeoutRef}] when Status == ringing; Status == missed ->
                io:format("[ACCEPT CALL] Found chat with status: ~p~n", [Status]),

                case TimeoutRef of
                    undefined ->
                        io:format("[ACCEPT CALL] No timeout reference~n"),
                        ok;
                    Ref when is_binary(Ref) ->
                        io:format("[ACCEPT CALL] Canceling timeout timer~n"),
                        erlang:cancel_timer(binary_to_term(Ref));
                    _ -> ok
                end,

                Url = "http://localhost:2020/call/accept",
                Body = #{call_id => list_to_binary(CallID)},
                Headers = [{"Content-Type", "application/json"}],
                Request = {Url, Headers, "application/json", jiffy:encode(Body)},

                io:format("[ACCEPT CALL] Calling Rust service at: ~p~n", [Url]),
                io:format("[ACCEPT CALL] Request body: ~p~n", [Body]),

                case httpc:request(post, Request, [], []) of
                    {ok, {{_, 200, _}, _, Response}} ->
                        io:format("[ACCEPT CALL] Rust service response: ~p~n", [Response]),

                        Decoded = jiffy:decode(Response, [return_maps]),
                        #{<<"status">> := <<"connected">>, <<"call_id">> := CallIDBin} = Decoded,
                        CallID = binary_to_list(CallIDBin),

                        io:format("[ACCEPT CALL] Updating chat status to connected~n"),
                        mnesia:write(Chat#chat{
                            call_status = connected,
                            timeout_ref = undefined,
                            date_updated = calendar:universal_time()
                        }),

                        io:format("[ACCEPT CALL] Call accepted successfully~n"),
                        io:format("========================================~n"),
                        CallID;
                    {error, Reason} ->
                        io:format("[ACCEPT CALL] ERROR: Rust service failed: ~p~n", [Reason]),
                        io:format("========================================~n"),
                        throw({error, {rust_service_failed, Reason}})
                end;
            [Chat] ->
                io:format("[ACCEPT CALL] ERROR: Invalid call state: ~p~n", [Chat#chat.call_status]),
                io:format("========================================~n"),
                throw({error, invalid_call_state});
            [] ->
                io:format("[ACCEPT CALL] ERROR: Chat not found~n"),
                io:format("========================================~n"),
                throw({error, chat_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, CallID} ->
            io:format("[ACCEPT CALL] Transaction successful~n"),
            CallID;
        {aborted, {error, Error}} ->
            io:format("[ACCEPT CALL] ERROR: Transaction aborted: ~p~n", [Error]),
            throw({error, Error});
        {aborted, Reason} ->
            io:format("[ACCEPT CALL] ERROR: Transaction failed: ~p~n", [Reason]),
            throw({error, {transaction_failed, Reason}})
    end.

end_video_call(CallID) ->
    io:format("~n========================================~n"),
    io:format("[END CALL] Ending call~n"),
    io:format("[END CALL] CallID: ~p~n", [CallID]),
    io:format("========================================~n"),

    Fun = fun() ->
        case mnesia:match_object(#chat{call_id = CallID, _ = '_'}) of
            [Chat = #chat{call_status = Status, timeout_ref = TimeoutRef}] when Status == ringing; Status == connected; Status == missed ->
                io:format("[END CALL] Found chat with status: ~p~n", [Status]),

                case TimeoutRef of
                    undefined ->
                        io:format("[END CALL] No timeout reference~n"),
                        ok;
                    Ref when is_binary(Ref) ->
                        io:format("[END CALL] Canceling timeout timer~n"),
                        erlang:cancel_timer(binary_to_term(Ref));
                    _ -> ok
                end,

                Url = "http://localhost:2020/call/end",
                Body = #{call_id => list_to_binary(CallID)},
                Headers = [{"Content-Type", "application/json"}],
                Request = {Url, Headers, "application/json", jiffy:encode(Body)},

                io:format("[END CALL] Calling Rust service at: ~p~n", [Url]),
                io:format("[END CALL] Request body: ~p~n", [Body]),

                case httpc:request(post, Request, [], []) of
                    {ok, {{_, 200, _}, _, Response}} ->
                        io:format("[END CALL] Rust service response: ~p~n", [Response]),

                        Decoded = jiffy:decode(Response, [return_maps]),
                        #{<<"status">> := <<"ended">>, <<"call_id">> := CallIDBin} = Decoded,
                        CallID = binary_to_list(CallIDBin),

                        io:format("[END CALL] Updating chat status to ended~n"),
                        mnesia:write(Chat#chat{
                            call_status = ended,
                            call_end_time = calendar:universal_time(),
                            timeout_ref = undefined,
                            date_updated = calendar:universal_time()
                        }),

                        io:format("[END CALL] Call ended successfully~n"),
                        io:format("========================================~n"),
                        CallID;
                    {error, Reason} ->
                        io:format("[END CALL] ERROR: Rust service failed: ~p~n", [Reason]),
                        io:format("========================================~n"),
                        throw({error, {rust_service_failed, Reason}})
                end;
            [Chat] ->
                io:format("[END CALL] ERROR: Invalid call state: ~p~n", [Chat#chat.call_status]),
                io:format("========================================~n"),
                throw({error, invalid_call_state});
            [] ->
                io:format("[END CALL] ERROR: Chat not found~n"),
                io:format("========================================~n"),
                throw({error, chat_not_found})
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, CallID} ->
            io:format("[END CALL] Transaction successful~n"),
            CallID;
        {aborted, {error, Error}} ->
            io:format("[END CALL] ERROR: Transaction aborted: ~p~n", [Error]),
            throw({error, Error});
        {aborted, Reason} ->
            io:format("[END CALL] ERROR: Transaction failed: ~p~n", [Reason]),
            throw({error, {transaction_failed, Reason}})
    end.

get_chat_by_call_id(CallID) ->
    io:format("[GET CHAT BY CALL ID] Looking up CallID: ~p~n", [CallID]),

    Res = mnesia:transaction(
        fun() ->
            mnesia:match_object(#chat{call_id = CallID, _= '_'})
        end),
    case Res of
      {atomic, []} ->
        io:format("[GET CHAT BY CALL ID] Chat not found~n"),
        chat_not_exist;
      {atomic, [Chat]} ->
        io:format("[GET CHAT BY CALL ID] Chat found: ~p~n", [Chat#chat.id]),
        Chat;
      _ ->
        io:format("[GET CHAT BY CALL ID] Error occurred~n"),
        error
    end.

handle_call_timeout(CallID) ->
    io:format("~n========================================~n"),
    io:format("[CALL TIMEOUT] Handling timeout for CallID: ~p~n", [CallID]),
    io:format("========================================~n"),

    mnesia:transaction(fun() ->
        case mnesia:match_object(#chat{call_id = CallID, _ = '_'}) of
            [Chat = #chat{call_status = Status, recipient_id = RecipientID}] when Status == ringing; Status == missed ->
                io:format("[CALL TIMEOUT] Updating call status to failed~n"),
                mnesia:write(Chat#chat{
                    call_status = failed,
                    call_end_time = calendar:universal_time(),
                    timeout_ref = undefined,
                    date_updated = calendar:universal_time()
                }),
                io:format("[CALL TIMEOUT] Logging missed call for recipient: ~p~n", [RecipientID]),
                log_missed_call(RecipientID, Chat),
                io:format("========================================~n");
            _ ->
                io:format("[CALL TIMEOUT] Call not in timeout-eligible state~n"),
                io:format("========================================~n"),
                ok
        end
    end).

log_missed_call(RecipientID, Chat) ->
    io:format("[LOG MISSED CALL] Creating notification for recipient: ~p~n", [RecipientID]),

    Notif = #notif{
        id = nanoid:gen(),
        user_id = RecipientID,
        message = "Missed Call",
        date_created = calendar:universal_time()
    },
    mnesia:write(Notif),

    io:format("[LOG MISSED CALL] Notification created: ~p~n", [Notif#notif.id]).
