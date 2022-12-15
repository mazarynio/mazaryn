-module(notifdb).
-export([insert/3, get_notif_by_id/1]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert(From, To, Message) ->
    Fun = fun() ->
        Id = id_gen:generate(),
        Notification = #notification{id = Id,
                                     from = From,
                                     to = To,
                                     message = Message},
        mnesia:write(Notification),
        Id
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_notif_by_id(Id) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#notification{id = Id, _= '_'})
            end),
    case Res of
      {atomic, []} -> notif_not_exist;
      {atomic, [Notification]} -> Notification;
      _ -> error
    end.
