-module(notifdb).
-author("Zaryn Technologies").
-export([insert/2, get_single_notif/1, get_all_notifs/1]). 

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

%
get_single_notif(NotifID) ->
    Fun = fun() ->
        case mnesia:read({notif, NotifID}) of
            [Notif] ->
                {ok, Notif};
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


    