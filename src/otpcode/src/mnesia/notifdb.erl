-module(notifdb).
-export([insert/3]).

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
