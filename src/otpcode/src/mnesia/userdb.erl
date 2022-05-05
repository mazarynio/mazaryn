-module(userdb).
-compile([export_all, nowarn_export_all]).
-include("../include/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(user, [{attributes, record_info(fields, user)},
            {disc_copies, [node()]}]).

reset_db() ->
    mnesia:clear_table(user). 

insert(Username, Password) ->
    Id = id_gen:generate(),
    User = #user{id = Id, username = Username, password = Password},
    F = fun() ->
        mnesia:write(User)
    end,
    mnesia:transaction(F).

get_user(Username) ->
    F = fun() ->
        Q = qlc:q([{U#user.username, U#user.password}
                || U <- mnesia:table(user),
                   U#user.username == Username]),
        qlc:e(Q)
        end,
    mnesia:transaction(F).

get_password(Username) ->
    Fun = fun() ->
        case mnesia:read({user, Username}) of 
            [#user{password = Password}] ->
                Password;
            [] ->
                undefined
        end
    end,
    case mnesia:trasnaction(Fun) of 
        {atomic, undefined} -> {error, unknown_user};
        {atomic, Password} -> Password
    end.

get_user_by_email(Email) ->
    F = fun() ->
        Q = qlc:q([{U#user.email}
                || U <- mnesia:table(user),
                U#user.email == Email]),
        qlc:e(Q)
        end,
        mnesia:transaction(F).

get_users() ->
    F = fun() ->
        Q = qlc:q([{U#user.username, U#user.password}
                || U <- mnesia:table(user)]),
        qlc:e(Q)
        end,
    mnesia:transaction(F).

search_user1(Username) ->
    F = fun() ->
        Q = qlc:q([U#user.username || U <- mnesia:table(user),
                U#user.username == Username]),
        qlc:e(Q)
    end,
    mnesia:transaction(F).

return_user(Username) ->
    mnesia:dirty_read({user, Username}).


delete_user(Username) ->
    mnesia:delete_table(Username),
    F = fun() ->
        mnesia:delete({user, Username})
    end,
    mnesia:activity(transaction, F).
    
search_user(Username) ->
    mnesia:dirty_read(user, Username).