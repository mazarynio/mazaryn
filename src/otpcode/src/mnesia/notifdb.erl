-module(notifdb).
-author("Zaryn Technologies").
-export([insert/3, welcome/2, follow/3, mention/3, chat/3, get_single_notif/1, get_notif_message/1, get_all_notifs/1,
         get_all_notif_ids/1, get_username_by_id/1, delete_notif/1, get_notif_time/1, get_five_latest_notif_ids/1,
         get_five_latest_notif_messages/1, mark_notif_as_read/1,
    mark_as_read/2, count_unread/1, mark_all_as_read/1, get_all_notif_ids/1,
 get_username_by_id/1, delete_notif/1, get_notif_time/1, get_five_latest_notif_ids/1,
 get_five_latest_notif_messages/1]). 

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert(UserID, Message, Type) ->
    Fun = fun() ->
        Id = nanoid:gen(),
        mnesia:write(#notif{
            id = Id,
            user_id = UserID,
            message = Message,
            type = Type,
            date_created = calendar:universal_time(),
            read = false
        }),
        [User] = mnesia:read({user, UserID}),
        Notifs = User#user.notif,
        mnesia:write(User#user{notif = [Id | Notifs]}),
        Id
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

welcome(UserID, Message) ->
    {atomic, {Id, _}} = mnesia:transaction(fun() ->
        Id = nanoid:gen(),
        mnesia:write(#notif{
            id = Id,
            user_id = UserID,
            message = Message,
            type = <<"welcome">>,
            date_created = calendar:universal_time(),
            read = false
        }),
        [User] = mnesia:read({user, UserID}),
        Notifs = User#user.notif,
        mnesia:write(User#user{notif = [Id | Notifs]}),
        {Id, ok}
    end),
    spawn_monitor(fun() ->
        Time = 2 * 24 * 60 * 60 * 1000, %% 2 days in milliseconds
        receive
        after Time -> delete_notif(Id)
        end
    end),
    Id.

follow(FollowerID, UserID, Message) ->
    {atomic, {Id, _}} = mnesia:transaction(fun() ->
        Id = nanoid:gen(),
        mnesia:write(#notif{
            id = Id,
            follower = FollowerID,
            user_id = UserID,
            message = Message,
            type = <<"follow">>,
            date_created = calendar:universal_time(),
            read = false
        }),
        [User] = mnesia:read({user, UserID}),
        Notifs = User#user.notif,
        mnesia:write(User#user{notif = [Id | Notifs]}),
        {Id, ok}
    end),
    spawn_monitor(fun() ->
        receive
        after 1200000 -> delete_notif(Id)
        end
    end),
    Id.

%% Mention notification (type is "mention")
mention(MentionerID, UserID, Message) ->
    {atomic, {Id, _}} = mnesia:transaction(fun() ->
        Id = nanoid:gen(),
        mnesia:write(#notif{
            id = Id,
            follower = MentionerID,
            user_id = UserID,
            message = Message,
            type = <<"mention">>,
            date_created = calendar:universal_time(),
            read = false
        }),
        [User] = mnesia:read({user, UserID}),
        Notifs = User#user.notif,
        mnesia:write(User#user{notif = [Id | Notifs]}),
        {Id, ok}
    end),
    spawn_monitor(fun() ->
        receive
        after 1200000 -> delete_notif(Id)
        end
    end),
    Id.

chat(SenderID, ReceiverID, Message) ->
    {atomic, {Id, _}} = mnesia:transaction(fun() ->
        Id = nanoid:gen(),
        mnesia:write(#notif{
            id = Id,
            follower = SenderID,
            user_id = ReceiverID,
            message = Message,
            type = <<"chat">>,
            date_created = calendar:universal_time(),
            read = false
        }),
        [User] = mnesia:read({user, ReceiverID}),
        Notifs = User#user.notif,
        mnesia:write(User#user{notif = [Id | Notifs]}),
        {Id, ok}
    end),
    spawn_monitor(fun() ->
        receive
        after 1200000 -> delete_notif(Id)
        end
    end),
    Id.

delete_notif(Id) ->
    Fun = fun() ->
        case mnesia:read({notif, Id}) of
            [N] ->
                UserID = N#notif.user_id,
                mnesia:delete({notif, Id}),
                [User] = mnesia:read({user, UserID}),
                Filtered = lists:filter(fun(E) -> E =/= Id end, User#user.notif),
                mnesia:write(User#user{notif = Filtered}),
                ok;
            [] -> ok
        end
    end,
    mnesia:transaction(Fun).

get_single_notif(NotifID) ->
    Fun = fun() ->
        case mnesia:read({notif, NotifID}) of
            [Notif] ->
                Notif;
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Res} ->
            Res;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

get_notif_message(NotifID) -> 
    Notification = get_single_notif(NotifID),
    NotifMessage = Notification#notif.message,
    NotifMessage.

get_all_notifs(UserID) ->
    Fun = fun() ->
        case mnesia:match_object(#notif{user_id = UserID, _ = '_'}) of
            Objects when is_list(Objects) ->
                Objects;
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Res} ->
            Res;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

get_all_notif_ids(UserID) ->
    Fun = fun() ->
        case mnesia:match_object(#notif{user_id = UserID, _ = '_'}) of
            Records when is_list(Records) ->
                [ID || #notif{id = ID} <- Records];
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Res} ->
            Res;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

get_username_by_id(UserID) ->
    User = userdb:get_user_by_id(UserID),
    Username = User#user.username,
    Username.

get_notif_time(NotifID) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#notif{id = NotifID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> block_not_exist;
        {atomic, [Notif]} -> Notif#notif.date_created;
        _ -> error
    end. 

get_five_latest_notif_ids(UserID) ->
    NotifIDs = notifdb:get_all_notif_ids(UserID),
    % Sort NotifIDs by their date_created in descending order
    SortedNotifIDs = lists:sort(fun(NotifID1, NotifID2) ->
        Time1 = get_notif_time(NotifID1),
        Time2 = get_notif_time(NotifID2),
        Time1 > Time2
    end, NotifIDs),
    % Take the first 5 elements from the sorted list
    lists:sublist(SortedNotifIDs, 1, 5).

get_five_latest_notif_messages(UserID) -> 
    LatestNotifIDs = get_five_latest_notif_ids(UserID),
    Messages = [get_notif_message(NotifID) || NotifID <- LatestNotifIDs],
    Messages.

mark_notif_as_read(NotifID) ->
    Fun = fun() ->
        case mnesia:read({notif, NotifID}) of
            [Notif] ->
                mnesia:write(Notif#notif{read = true}),
                ok;
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Res} ->
            Res;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

  count_unread(UserID) ->
    Fun = fun() ->
        Unread = mnesia:match_object(#notif{user_id = UserID, read = false, _ = '_'}),
        length(Unread)
    end,
    case mnesia:transaction(Fun) of
        {atomic, Count} ->
            Count;
        {aborted, Reason} ->
            {error, {aborted, Reason}}
    end.

mark_as_read(UserID, NotificationID) ->
    Fun = fun() ->
        case mnesia:read({notif, NotificationID}) of
            [#notif{user_id = UserID} = Notif] ->
                %% Update the 'read' field
                mnesia:write(Notif#notif{read = true});
            _ ->
                ok
        end
    end,
    mnesia:transaction(Fun).

mark_all_as_read(UserID) ->
    Fun = fun() ->
        Notifications = mnesia:match_object(#notif{user_id = UserID, read = false, _ = '_'}),
        lists:foreach(fun(N) ->
            mnesia:write(N#notif{read = true})
        end, Notifications)
    end,
    mnesia:transaction(Fun).
