-module(user_level_db).
-author("Zaryn Technologies").
-include("../../records.hrl").
-export([remove_level_one_users/0, downgrade_level_two_users/0, downgrade_level_three_users/0, downgrade_level_four_users/0]).

remove_level_one_users() ->
    Fun = fun() ->
        CurrentTime = erlang:system_time(seconds),
        SixMonthsAgo = CurrentTime - (6 * 30 * 24 * 60 * 60),
        MatchSpec = [{#user{level = 1, date_created = '$1', _ = '_'},
                      [{'=<', '$1', SixMonthsAgo}],
                      ['$_']}],
        Users = mnesia:select(user, MatchSpec),
        lists:foreach(fun(User) -> mnesia:delete_object(User) end, Users)
    end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.

downgrade_level_two_users() ->
    Fun = fun() ->
        CurrentTime = erlang:system_time(seconds),
        SixMonthsAgo = CurrentTime - (6 * 30 * 24 * 60 * 60),
        MatchSpec = [{#user{level = 2, date_updated = '$1', _ = '_'},
                      [{'=<', '$1', SixMonthsAgo}],
                      ['$_']}],
        Users = mnesia:select(user, MatchSpec),
        lists:foreach(fun(User) ->
                          UpdatedUser = User#user{level = 1},
                          mnesia:write(UpdatedUser)
                      end, Users)
    end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.

downgrade_level_three_users() ->
    Fun = fun() ->
        CurrentTime = erlang:system_time(seconds),
        SixMonthsAgo = CurrentTime - (6 * 30 * 24 * 60 * 60),
        MatchSpec = [{#user{level = 3, date_updated = '$1', _ = '_'},
                      [{'=<', '$1', SixMonthsAgo}],
                      ['$_']}],
        Users = mnesia:select(user, MatchSpec),
        lists:foreach(fun(User) ->
                          UpdatedUser = User#user{level = 2},
                          mnesia:write(UpdatedUser)
                      end, Users)
    end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.

downgrade_level_four_users() ->
    Fun = fun() ->
        CurrentTime = erlang:system_time(seconds),
        SixMonthsAgo = CurrentTime - (6 * 30 * 24 * 60 * 60),
        MatchSpec = [{#user{level = 4, date_updated = '$1', _ = '_'},
                      [{'=<', '$1', SixMonthsAgo}],
                      ['$_']}],
        Users = mnesia:select(user, MatchSpec),
        lists:foreach(fun(User) ->
                          UpdatedUser = User#user{level = 3},
                          mnesia:write(UpdatedUser)
                      end, Users)
    end,
    {atomic, _} = mnesia:transaction(Fun),
    ok.