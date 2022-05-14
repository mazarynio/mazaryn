-module(followerdb).
-compile([export_all, nowarn_export_all]).
-include("../include/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(follower, [{attributes, record_info(fields, follower)},
            {disc_copies, [node()]}]).

reset_db() ->
    mnesia:clear_table(follower).

insert(Username) ->
    Id = id_gen:generate(),
    Follower = #follower{id = Id, username = Username},
    F = fun() ->
        mnesia:write(Follower)
    end,
    mnesia:transaction(F).

get_follower(Username) ->
    Fun = fun() ->
        Q = qlc:q([{F#follower.username}
                || F <- mnesia:table(follower),
                   F#follower.username == Username]),
        qlc:e(Q)
        end,
    mnesia:transaction(Fun).

save_follow_user(Username) ->
    Follower = #follower{username = Username},
    case is_following(Username) of
        true ->
            {error, already_following};
        false ->
            mnesia:activity(transaction, fun() -> mnesia:write(Follower) end)
    end.

delete_follow_user(Username) ->
    case is_following(Username) of
        true ->
            Follower = #follower{username = Username},
            mnesia:activity(transaction, 
                        fun() ->
                            [Follow] = mnesia:match_object(Follower),
                            mnesia:delete_object(Follow)
                        end);
        false -> {error, not_following}
    end.

is_following(Username) ->
    Follower = #follower{username = Username},
    case mnesia:activity(async_dirty,
                        fun() -> mnesia:match_object(Follower) end) of
        [_Follow] -> true;
        [] -> false
    end.

