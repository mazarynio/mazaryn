-module(notifdb).
-export([insert/2]). 

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
    