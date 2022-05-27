-module(userdb).
-include("../records.hrl").

-export([set_user_info/3, get_user_info/1,
         insert/3, login/2,
         get_user/1, get_user_by_email/1,
         get_users/0, delete_user/1, get_password/1,
         change_username/3, change_password/3, change_email/3,
         follow/2, unfollow/2, follow_multiple/2, unfollow_multiple/2,
         save_post/2, save_posts/2, unsave_post/2, unsave_posts/2,
         get_save_posts/1, get_follower/1, get_following/1,
         block/2, unblock/2, get_blocked/1]).

%%% check user credentials
-spec login(Username :: term(), Password :: term()) -> wrong_username_or_password | logged_in.
login(Username, Password) ->
  case check_user_credential(Username, Password) of
    {true, _} -> logged_in;
    false -> wrong_username_or_password
  end.

%%% Fields: [field_a, field_b, field_c]
%%% Values: [a,b,c]
set_user_info(Username, Fields, Values) ->
  List = lists:zip(Fields, Values),
  case get_user(Username) of
    not_exist -> not_exist;
    error -> error;
    User ->
      Attrs = proplists:get_keys(User#user.other_info),
      NotUpdatedAttrs =[proplists:delete(X, Attrs) || X <- proplists:get_keys(List)],
      mnesia:transaction(
        fun() ->
          mnesia:write(#user{username = Username,
                             other_info = lists:append([List|NotUpdatedAttrs]),
                             date_updated = calendar:universal_time()})
        end)
  end.

insert(Username, Password, Email) ->
    %% check username exist or not
    case {check_username(Username), check_email(Email)} of
      {undefined, undefined} ->
        Now = calendar:universal_time(),
        User = #user{username = Username,
                     password = erlpass:hash(Password),
                     email = Email,
                     date_created = Now},
        F = fun() ->
              mnesia:write(User)
            end,
        {atomic, Res} = mnesia:transaction(F),
        Res;
      _ -> username_or_email_existed
    end.

get_user(Username) ->
    F = fun() ->
        mnesia:read(user, Username)
        end,
    Res = mnesia:transaction(F),
    case Res of
      {atomic, [User]} -> User;
      {atomic, []} -> not_exist;
      _ -> error
    end.

get_users() ->
    Fun = fun() ->
            mnesia:foldl(fun(User, Acc) ->
                            [User#user.username|Acc]
                         end, [], user)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_password(Username) ->
    F = fun() ->
        mnesia:read(user, Username)
        end,
    Res = mnesia:transaction(F),
    case Res of
      {atomic, [User]} -> User#user.password;
      {atomic, []} -> not_exist;
      _ -> error
    end.

get_user_by_email(Email) ->
  Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#user{email = Email, _= '_'})
          end),
  case Res of
    {atomic, []} -> not_exist;
    {atomic, User} -> User;
    _ -> error
  end.

% TODO: change password, email, username
change_password(Username, CurrentPass, NewPass) ->
  case check_user_credential(Username, CurrentPass) of
    {true, User} ->
      Fun = fun() ->
              mnesia:write(User#user{password = erlpass:hash(NewPass),
                                     date_updated = calendar:universal_time()})
            end,
      mnesia:transaction(Fun),
      ok;
    false -> wrong_username_or_password
  end.

change_email(Username, CurrentPass, NewEmail) ->
  case check_user_credential(Username, CurrentPass) of
    {true, User} ->
      case check_email(NewEmail) of
        undefined ->
          mnesia:transaction(fun() ->
                               mnesia:write(User#user{email = NewEmail,
                                                      date_updated = calendar:universal_time()})
                             end);
        existed ->
          email_existed
      end;
    false ->
      wrong_username_or_password
  end.

change_username(Username, CurrentPass, NewUsername) ->
  case check_user_credential(Username, CurrentPass) of
    {true, User} ->
      case check_username(NewUsername) of
        undefined ->
            mnesia:transaction(fun() ->
                                mnesia:write(User#user{username = NewUsername,
                                                       date_updated = calendar:universal_time()}),
                                mnesia:delete({user, Username})
                               end),
            ok;
        existed ->
          username_existed
      end;
    false ->
      wrong_username_or_password
  end.

delete_user(Username) ->
    F = fun() ->
        [User] = mnesia:match_object(#user{username = Username, _= '_'}),
        mnesia:delete_object(User)
    end,
    mnesia:activity(transaction, F).

follow(Username, Following) ->
    Fun = fun() ->
            [User] = mnesia:read(user, Username),
            NewFollowList = [Following|User#user.following],
            mnesia:write(User#user{following = NewFollowList}),

            %% update Following that have a new follower
            [FollowingUser] = mnesia:read(user, Following),
            mnesia:write(FollowingUser#user{follower = [Username| FollowingUser#user.follower]})
        end,
    mnesia:transaction(Fun).

unfollow(Username, Following) ->
    Fun = fun() ->
            [User] = mnesia:read(user, Username),
            NewFollowList = lists:delete(Following, User#user.following),
            mnesia:write(User#user{following = NewFollowList}),

            [FollowingUser] = mnesia:read(user, Following),
            NewFollower = lists:delete(Username, FollowingUser#user.follower),
            mnesia:write(FollowingUser#user{follower = NewFollower})
        end,
    mnesia:transaction(Fun).

follow_multiple(Username, Others) ->
    lists:foreach(fun(Other) ->
                    follow(Username, Other)
                  end, Others).

unfollow_multiple(Username, Others) ->
    lists:foreach(fun(Other) ->
                    unfollow(Username, Other)
                  end, Others).

%% follow/unfollow posts
save_post(Username, PostId) ->
    Fun = fun() ->
                [User] = mnesia:read(user, Username),
                NewFollowList = [PostId|User#user.saved_posts],
                mnesia:write(User#user{saved_posts = NewFollowList})
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

unsave_post(Username, PostId) ->
    Fun = fun() ->
                [User] = mnesia:read(user, Username),
                NewFollowList = lists:delete(PostId, User#user.saved_posts),
                mnesia:write(User#user{saved_posts = NewFollowList})
          end,
    mnesia:transaction(Fun).

save_posts(Username, PostIds) ->
    lists:foreach(fun(PostId) ->
                    save_post(Username, PostId)
                  end, PostIds).

unsave_posts(Username, PostIds) ->
    lists:foreach(fun(PostId) ->
                    unsave_post(Username, PostId)
                  end, PostIds).

%% get save post, follower, following
get_save_posts(Username) ->
  {atomic, Res} = mnesia:transaction(fun() ->
                                      [User] = mnesia:read(user, Username),
                                      User#user.saved_posts
                                     end),
  Res.

get_following(Username) ->
  {atomic, Res} = mnesia:transaction(fun() ->
                                      [User] = mnesia:read(user, Username),
                                      User#user.following
                                     end),
  Res.

get_follower(Username) ->
  {atomic, Res} = mnesia:transaction(fun() ->
                                      [User] = mnesia:read(user, Username),
                                      User#user.follower
                                     end),
  Res.

get_user_info(Username) ->
  {atomic, Res} = mnesia:transaction(fun() ->
                                      [User] = mnesia:read(user, Username),
                                      User#user.other_info
                                     end),
  Res.

%%% username and email should be unique for each user
%%% check if username/email existed
-spec check_username(Username :: term()) -> undefined | existed.
check_username(Username) ->
    Fun = fun() ->
          Object =  mnesia:match_object(#user{username = Username,
                                              _ = '_'}),
          case Object of
            [] -> undefined;
            _ -> existed
          end
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

-spec check_email(Username :: term()) -> undefined | existed.
check_email(Email) ->
    Fun = fun() ->
          Object =  mnesia:match_object(#user{email = Email, _ = '_'}),
          case Object of
            [] -> undefined;
            _ -> existed
          end
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

check_user_credential(Username, Password) ->
  case get_user(Username) of
    not_exist -> false;
    error -> false;
    User ->
      case erlpass:match(Password, User#user.password) of
        true -> {true, User};
        false -> false
      end
  end.

block(Username, Blocked) ->
  case get_user(Username) of
    not_exist -> not_exist;
    error -> error;
    User ->
      List = User#user.blocking,
      UpdatedUser = User#user{blocking = [Blocked|List]},
      {atomic, Res} = mnesia:transaction(fun() -> mnesia:write(UpdatedUser) end),
      Res
  end.

unblock(Username, Unblocked) ->
  case get_user(Username) of
    not_exist -> not_exist;
    error -> error;
    User ->
      List = User#user.blocking,
      UpdatedUser = User#user{blocking = lists:delete(Unblocked, List)},
      {atomic, Res} = mnesia:transaction(fun() -> mnesia:write(UpdatedUser) end),
      Res
  end.

get_blocked(Username) ->
  case get_user(Username) of
    not_exist -> not_exist;
    error -> error;
    User ->
      User#user.blocking
  end.