-module(admindb).
-export([
    is_admin/1,
    add_admin/1,
    remove_admin/1,
    list_admins/0,
    init_default_admins/0
]).

-define(DEFAULT_ADMINS, ["zaryn", "mazaryn", "arvand"]).

init_default_admins() ->
    case application:get_env(otpcode, admin_users) of
        undefined ->
            application:set_env(otpcode, admin_users, ?DEFAULT_ADMINS),
            ok;
        {ok, _Admins} ->
            ok
    end.

is_admin(Username) when is_binary(Username) ->
    is_admin(binary_to_list(Username));
is_admin(Username) when is_list(Username) ->
    case application:get_env(otpcode, admin_users) of
        {ok, Admins} -> lists:member(Username, Admins);
        undefined -> lists:member(Username, ?DEFAULT_ADMINS)
    end;
is_admin(_) -> false.

add_admin(Username) when is_binary(Username) ->
    add_admin(binary_to_list(Username));
add_admin(Username) when is_list(Username) ->
    Admins = case application:get_env(otpcode, admin_users) of
        {ok, List} -> List;
        undefined -> ?DEFAULT_ADMINS
    end,
    case lists:member(Username, Admins) of
        true -> {error, already_admin};
        false ->
            NewAdmins = [Username | Admins],
            application:set_env(otpcode, admin_users, NewAdmins),
            ok
    end;
add_admin(_) -> {error, invalid_username}.

remove_admin(Username) when is_binary(Username) ->
    remove_admin(binary_to_list(Username));
remove_admin(Username) when is_list(Username) ->
    Admins = case application:get_env(otpcode, admin_users) of
        {ok, List} -> List;
        undefined -> ?DEFAULT_ADMINS
    end,
    NewAdmins = lists:delete(Username, Admins),
    application:set_env(otpcode, admin_users, NewAdmins),
    ok;
remove_admin(_) -> {error, invalid_username}.

list_admins() ->
    case application:get_env(otpcode, admin_users) of
        {ok, Admins} -> Admins;
        undefined -> ?DEFAULT_ADMINS
    end.
