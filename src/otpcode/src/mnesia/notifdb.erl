-module(notifdb).
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

get_single_notif(NotifID) ->
    Fun = fun() ->
            [Notif] = mnesia:read({notif, NotifID}),
            Notif
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_all_notifs(UserID) ->
  Fun = fun() ->
          mnesia:match_object(#notif{user_id = UserID,
                                              _ = '_'}),
          [User] = mnesia:read({user, UserID}),
          lists:foldl(fun(ID, Acc) ->
                        [Notif] = mnesia:read({notif, ID}),
                        [Notif|Acc]
                      end,
                      [], User#user.notif)

        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.
    