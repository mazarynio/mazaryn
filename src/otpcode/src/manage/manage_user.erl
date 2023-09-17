-module(manage_user).
-export([get_user/1, ban_user/1, unban_user/1]).
-include("../records.hrl").

get_user(Username) ->
    F = fun() ->
        mnesia:index_read(user, Username, username)
        end,
    Res = mnesia:transaction(F),
    case Res of
      {atomic, [User]} -> User;
      {atomic, []} -> not_exist;
      _ -> error
    end.

ban_user(UsernameOrID) ->
    {ok, User} = get_user(UsernameOrID),
    UpdatedUser = User#user{blocked = true},
    {atomic, _} = mnesia:transaction(fun() ->
        mnesia:write(UpdatedUser)
    end).
        
unban_user(UsernameOrID) ->
    {ok, User} = get_user(UsernameOrID),
    UpdatedUser = User#user{blocked = false},
    {atomic, _} = mnesia:transaction(fun() ->
        mnesia:write(UpdatedUser)
    end).