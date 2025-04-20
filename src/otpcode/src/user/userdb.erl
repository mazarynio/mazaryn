-module(userdb).
-export([get_user_by_id/1, get_user/1]).

-include("../records.hrl").

get_user_by_id(UserID) ->
    %% Try to read from Mnesia
    case mnesia:dirty_read({user, UserID}) of
        [User] -> User;
        [] -> not_found
    end.

get_user(Username) ->
    %% Simulated way to get by username — you can replace this with proper QLC later
    Users = mnesia:dirty_match_object(#user{username = Username, _ = '_'}),
    case Users of
        [User] -> User;
        [] -> not_found
    end.
