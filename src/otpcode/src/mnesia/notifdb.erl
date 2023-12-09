-module(notifdb).
-author("Zaryn Technologies").
-export([insert/2, welcome/2, follow/2, get_single_notif/1, get_notif_message/1, get_all_notifs/1,
    get_username_by_id/1, delete_notif/1]). 

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert(UserID, Message) ->  
    Fun = fun() ->
            Id = nanoid:gen(),
            mnesia:write(#notif{id = Id,
                                user_id = UserID,
                                message = Message,
                                date_created = calendar:universal_time()}),
            [User] = mnesia:read({user, UserID}),
            Notifs = User#user.notif,
            mnesia:write(User#user{notif = [Id|Notifs]}),
            Id 
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

welcome(UserID, Message) ->  
    Fun = fun() ->
            Id = nanoid:gen(),
            mnesia:write(#notif{id = Id,
                                user_id = UserID,
                                message = Message,
                                date_created = calendar:universal_time()}),
            [User] = mnesia:read({user, UserID}),
            Notifs = User#user.notif,
            mnesia:write(User#user{notif = [Id|Notifs]}),
            Id
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

follow(UserID, Message) ->  
    Fun = fun() ->
            Id = nanoid:gen(),
            mnesia:write(#notif{id = Id,
                                user_id = UserID,
                                message = Message,
                                date_created = calendar:universal_time()}),
            [User] = mnesia:read({user, UserID}),
            Notifs = User#user.notif,
            mnesia:write(User#user{notif = [Id|Notifs]}),
            Id
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
%
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
%
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

get_username_by_id(UserID) ->
    User = userdb:get_user_by_id(UserID),
    Username = User#user.username,
    Username.

%% Delete notification bt Notif ID
delete_notif(NotifID) ->
  F = fun() ->
          mnesia:delete({notification, NotifID})
      end,
  mnesia:activity(transaction, F).

    