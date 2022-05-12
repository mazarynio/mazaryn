-module(userdb).
-compile([export_all, nowarn_export_all]).
-include("../records.hrl").
-include_lib("stdlib/include/qlc.hrl").

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

following(Username, Following) ->
    Fun = fun() ->
            [User] = mnesia:read(user, Username),
            NewFollowList = [Following|User#user.following],
            mnesia:write(user, User#user{following = NewFollowList}),

            %% update Following that have a new follower
            [FollowingUser] = mnesia:read(user, Following),
            mnesia:write(user, FollowingUser#user{follower = [Username| FollowingUser#user.follower]})
        end,
    mnesia:transaction(Fun).

unfollowing(Username, Following) ->
    Fun = fun() ->
            [User] = mnesia:read(user, Username),
            NewFollowList = lists:delete(Following, User#user.following),
            mnesia:write(user, User#user{following = NewFollowList}),

            [FollowingUser] = mnesia:read(user, Following),
            NewFollower = lists:delete(Username, FollowingUser#user.follower),
            mnesia:write(user, FollowingUser#user{follower = NewFollower})
        end,
    mnesia:transaction(Fun).

follow_multiple(Username, Others) ->
    lists:foreach(fun(Other) ->
                    following(Username, Other)
                  end, Others).

unfollow_multiple(Username, Others) ->
    lists:foreach(fun(Other) ->
                    unfollowing(Username, Other)
                  end, Others).
                
%% follow/unfollow posts
follow_post(Username, PostId) ->
    Fun = fun() ->
                [User] = mnesia:read(user, Username),
                NewFollowList = [PostId|User#user.following_posts],
                mnesia:write(user, User#user{following_posts = NewFollowList})
          end,
    mnesia:transaction(Fun).

unfollow_post(Username, PostId) ->
    Fun = fun() ->
                [User] = mnesia:read(user, Username),
                NewFollowList = lists:delete(PostId, User#user.following_posts),
                mnesia:write(user, User#user{following_posts = NewFollowList})
          end,
    mnesia:transaction(Fun).

follow_posts(Username, PostIds) ->
    lists:foreach(fun(PostId) ->
                    follow_post(Username, PostId)
                  end, PostIds).

unfollow_posts(Username, PostIds) ->
    lists:foreach(fun(PostId) ->
                    unfollow_post(Username, PostId)
                  end, PostIds).


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

find_user(Username) ->
    F = fun() ->
        mnesia:match_object({user, Username, '_'})
    end,
    {atomic, Results} = mnesia:transaction(F),
    Results.

find_all() ->
    F = fun() ->
        mnesia:match_object({user, '_', '_'})
    end,
    {atomic, Results} = mnesia:transaction(F),
    Results.

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