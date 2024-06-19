-module(manage_user).
-export([get_users/0, get_users_info/0, get_user_info/1, get_user/1, delete_account/2,
 ban_user/1, unban_user/1, verify_user/2, unverify_user/2, remove_inactive_users/0, suspend_user/2,
 unsuspend_user/1]).
-include("../records.hrl").
-include("admins.hrl").

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

delete_account(UsernameOrID, AdminUsername) ->
    case userdb:get_user_id(AdminUsername) of
        {error, _} = Error ->
            Error;
        {ok, _} ->
            case userdb:get_user_id(UsernameOrID) of
                {error, _} = Error ->
                    Error;
                {ok, _} ->
                    case lists:member(AdminUsername, ?ADMIN_USERNAMES) of
                        true ->
                            Fun = fun() ->
                                [User] = mnesia:match_object(#user{username = UsernameOrID, _ = '_'}),
                                mnesia:delete_object(User),
                                io:fwrite("User ~s has been deleted~n", [UsernameOrID]),
                                ok
                            end,
                            {atomic, Res} = mnesia:transaction(Fun),
                            Res;
                        false ->
                            {error, not_admin}
                    end
            end
    end.

ban_user(UsernameOrID) ->
    Fun = fun() ->
            case get_user(UsernameOrID) of
                {error, Reason} ->
                    {error, Reason};
                User ->
                    UpdatedUser = User#user{blocked = true},
                    mnesia:write(UpdatedUser),
                    io:fwrite("~p~n", [UpdatedUser]),
                    ok
            end
        end,
        {atomic, Res} = mnesia:transaction(Fun),
        Res.
        
unban_user(UsernameOrID) ->
    Fun = fun() ->
            case get_user(UsernameOrID) of
                {error, Reason} ->
                    {error, Reason};
                User ->
                    UpdatedUser = User#user{blocked = false},
                    mnesia:write(UpdatedUser),
                    io:fwrite("~p~n", [UpdatedUser]),
                    ok
            end
        end,
        {atomic, Res} = mnesia:transaction(Fun),
        Res.

verify_user(UsernameOrID, AdminUsername) ->
    FormatAdminUsername = binary_to_list(AdminUsername),
    case userdb:get_user_id(UsernameOrID) of
        {error, _} = Error ->            
            Error;
        {ok, _ID} ->
            
            case lists:member(FormatAdminUsername, ?ADMIN_USERNAMES) of
                
                true ->
                    Fun = fun() ->
                        case get_user(UsernameOrID) of
                            {error, Reason} ->
                                {error, Reason};
                            User ->
                                UpdatedUser = User#user{verified = true},
                                mnesia:write(UpdatedUser),
                                io:fwrite("~p~n", [UpdatedUser]),
                                ok
                        end
                    end,
                    {atomic, Res} = mnesia:transaction(Fun),
                    Res;
                false ->
                    {error, not_admin}
            end
    end.

unverify_user(UsernameOrID, AdminUsername) ->
    FormatAdminUsername = binary_to_list(AdminUsername),
    case userdb:get_user_id(AdminUsername) of
        {error, _} = Error ->
            Error;
        {ok, _} ->
            case lists:member(FormatAdminUsername, ?ADMIN_USERNAMES) of
                true ->
                    Fun = fun() ->
                        case get_user(UsernameOrID) of
                            {error, Reason} ->
                                {error, Reason};
                            User ->
                                UpdatedUser = User#user{verified = false},
                                mnesia:write(UpdatedUser),
                                io:fwrite("~p~n", [UpdatedUser]),
                                ok
                        end
                    end,
                    {atomic, Res} = mnesia:transaction(Fun),
                    Res;
                false ->
                    {error, not_admin}
            end
    end.

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
            case get_user(UserID) of
                {error, Reason} ->
                    {error, Reason};
                User ->
                    UpdatedUser = User#user{suspend = false},
                    mnesia:write(UpdatedUser),
                    io:fwrite("~p~n", [UpdatedUser]),
                    ok
            end
        end,
        {atomic, Res} = mnesia:transaction(Fun),
        Res.




