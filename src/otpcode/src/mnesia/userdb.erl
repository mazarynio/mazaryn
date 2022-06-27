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
  {atomic, Res} = mnesia:transaction(fun() ->
                                        check_user_credential(Username, Password)
                                     end),
  case Res of
    {true, _} -> logged_in;
    false -> wrong_username_or_password
  end.

%%% Fields: [field_a, field_b, field_c]
%%% Values: [a,b,c]
set_user_info(Username, Fields, Values) ->
  List = lists:zip(Fields, Values),
  Fun = fun() ->
          case get_user_in_transaction(Username) of
            [] -> not_exist;
            User ->
              CurrentFields = User#user.other_info,
              NewFields =
                lists:foldl(fun({Key, Value}, Acc) ->
                                case lists:keymember(Key, 1, Acc) of
                                  true ->
                                    lists:keyreplace(Key, 1, Acc, {Key, Value});
                                  false ->
                                    [{Key, Value}|Acc]
                                end
                            end, CurrentFields, List),

              mnesia:write(User#user{other_info = NewFields,
                                     date_updated = calendar:universal_time()})
          end
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

insert(Username, Password, Email) ->
    %% check username exist or not
    Fun = fun() ->
            case {check_username(Username), check_email(Email)} of
              {undefined, undefined} ->
                Now = calendar:universal_time(),
                User = #user{username = Username,
                             password = erlpass:hash(Password),
                             email = Email,
                             date_created = Now},
                mnesia:write(User);
              {username_existed, _} -> username_existed;
              {_, email_existed} -> email_existed;
              {username_existed, email_existed} -> username_and_email_existed
            end
      end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

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

get_user_in_transaction(Username) ->
    Res = mnesia:read(user, Username),
    case Res of
      [] -> not_exist;
      [User] -> User
    end.

get_users() ->
    Fun = fun() ->
            mnesia:all_keys(user)
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
      {atomic, []} -> user_not_existed;
      _ -> error
    end.

get_user_by_email(Email) ->
  Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#user{email = Email, _= '_'})
          end),
  case Res of
    {atomic, []} -> user_not_exist;
    {atomic, User} -> User;
    _ -> error
  end.

% TODO: change password, email, username
change_password(Username, CurrentPass, NewPass) ->
  Fun = fun() ->
          case check_user_credential(Username, CurrentPass) of
            {true, User} ->
                      mnesia:write(User#user{password = erlpass:hash(NewPass),
                                             date_updated = calendar:universal_time()});
            false -> wrong_username_or_password
          end
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

change_email(Username, Password, NewEmail) ->
  Fun = fun() ->
            case check_email(NewEmail) of
                  undefined ->
                     case check_user_credential(Username, Password) of
                       {true, User} ->
                          mnesia:write(User#user{email = NewEmail,
                                                 date_updated = calendar:universal_time()});
                       false ->
                          wrong_username_or_password
                     end;
                  email_existed ->
                    email_existed
            end
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

change_username(Username, CurrentPass, NewUsername) ->
  Fun = fun() ->
          case check_username(NewUsername) of
            undefined ->
              case check_user_credential(Username, CurrentPass) of
                {true, User} ->
                  mnesia:write(User#user{username = NewUsername,
                                         date_updated = calendar:universal_time()}),
                  mnesia:delete({user, Username});
                false ->
                  wrong_username_or_password
              end;
            username_existed -> username_existed
          end
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

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
            mnesia:write(User#user{following = NewFollowList,
                                   date_updated = calendar:universal_time()}),

            %% update Following that have a new follower
            [FollowingUser] = mnesia:read(user, Following),
            mnesia:write(FollowingUser#user{follower = [Username| FollowingUser#user.follower],
                                            date_updated = calendar:universal_time()})
        end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

unfollow(Username, Following) ->
    Fun = fun() ->
            [User] = mnesia:read(user, Username),
            NewFollowList = lists:delete(Following, User#user.following),
            mnesia:write(User#user{following = NewFollowList,
                                   date_updated = calendar:universal_time()}),

            [FollowingUser] = mnesia:read(user, Following),
            NewFollower = lists:delete(Username, FollowingUser#user.follower),
            mnesia:write(FollowingUser#user{follower = NewFollower,
                                            date_updated = calendar:universal_time()})
        end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

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
                mnesia:write(User#user{saved_posts = NewFollowList,
                                       date_updated = calendar:universal_time()})
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

unsave_post(Username, PostId) ->
    Fun = fun() ->
                [User] = mnesia:read(user, Username),
                NewFollowList = lists:delete(PostId, User#user.saved_posts),
                mnesia:write(User#user{saved_posts = NewFollowList,
                                       date_updated = calendar:universal_time()})
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
          Object =  mnesia:match_object(#user{username = Username,
                                              _ = '_'}),
          case Object of
            [] -> undefined;
            _ -> username_existed
          end.

-spec check_email(Username :: term()) -> undefined | existed.
check_email(Email) ->
  Object =  mnesia:match_object(#user{email = Email, _ = '_'}),
  case Object of
    [] -> undefined;
    _ -> email_existed
  end.

check_user_credential(Username, Password) ->
  Object =  mnesia:match_object(#user{username = Username,
                                      _ = '_'}),
  case Object of
    [] -> false;
    [User] ->
      case erlpass:match(Password, User#user.password) of
        true -> {true, User};
        false -> false
      end
  end.

block(Username, Blocked) ->
  Fun = fun() ->
          [#user{blocked = BlockedList} = User] = mnesia:read(user, Username),
          mnesia:write(User#user{blocked = [Blocked|BlockedList],
                                 date_updated = calendar:universal_time()})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

unblock(Username, Unblocked) ->
  Fun = fun() ->
          [#user{blocked = BlockedList} = User] = mnesia:read(user, Username),
          mnesia:write(User#user{blocked = lists:delete(Unblocked, BlockedList),
                                 date_updated = calendar:universal_time()})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

get_blocked(Username) ->
  Fun = fun() ->
          [#user{blocked = BlockedList}] = mnesia:read(user, Username),
          BlockedList
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.
