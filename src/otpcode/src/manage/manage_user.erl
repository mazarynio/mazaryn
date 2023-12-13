-module(manage_user).
-export([get_users/0, get_users_info/0, get_user_info/1, get_user/1, delete_account/1,
 ban_user/1, unban_user/1, verify_user/1, unverify_user/1, remove_inactive_users/0, suspend_user/2,
 unsuspend_user/1]).
-include("../records.hrl").

get_users() ->
    Fun = fun() ->
            mnesia:all_keys(user)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_users_info() ->
    Users = userdb:get_users(),
    UsersInfo = lists:map(fun(UserID) ->
                                get_user_info(UserID)
                                end, Users),
    UsersInfo.
    
get_user_info(UserID) ->
    userdb:get_user_by_id(UserID).

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

delete_account(Username) ->
    F = fun() ->
        [User] = mnesia:match_object(#user{username = Username, _= '_'}),
        mnesia:delete_object(User)
    end,
    mnesia:activity(transaction, F).

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

verify_user(UsernameOrID) ->
    Fun = fun() ->
        User = get_user(UsernameOrID),
        UpdatedUser = User#user{verified = true},
        mnesia:write(UpdatedUser),
        io:fwrite("~p~n", [UpdatedUser])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

unverify_user(UsernameOrID) ->
    Fun = fun() ->
        User = get_user(UsernameOrID),
        UpdatedUser = User#user{verified = false},
        mnesia:write(UpdatedUser),
        io:fwrite("~p~n", [UpdatedUser])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

remove_inactive_users() ->
    'not implemented'.

suspend_user(UserID, Duration) ->
    Fun = fun() ->
        ID = nanoid:gen(),
        Date = calendar:universal_time(),
        Suspend = #suspend{
            id = ID,
            user = UserID,
            status = true,
            date_created = Date,
            duration = Duration
        },
        [User] = mnesia:read({user, UserID}),
        mnesia:write(User#user{suspend = Suspend}),
        ID 
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
    
unsuspend_user(UserID) ->
    Fun = fun() ->
        Suspend = #suspend{
            status = false 
        },
        [User] = mnesia:read({user, UserID}),
        mnesia:write(User#user{suspend = Suspend})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.




