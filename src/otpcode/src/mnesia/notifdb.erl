-module(notifdb).
-export([insert/2]).

-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

insert(To, Message) ->
    F = fun() ->
        Id = nanoid:gen(),
        mnesia:write(#notification{id = Id,
                                   message = Message,
                                   to = To,
                                   date_created = calendar:universal_time() }),
          [User] = mnesia:index_read(user, To, username),
          Notifications = User#user.notification,
          mnesia:write(User#user{notification = [Id | Notifications]}),
          Id
    end,
    {atomic, Res} = mnesia:transaction(F),
    Res.