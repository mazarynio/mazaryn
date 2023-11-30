-module(userdb).
-author("Zaryn Technologies").
-include("../records.hrl"). 

-export([set_user_info/3, get_user_info/2,
         insert/3, insert_media/3, get_media/2,
         login/2,
         get_user/1, get_user_by_email/1, get_user_by_id/1, get_token_by_id/1,
         get_users/0, delete_user/1, get_password/1,
         change_username/3, change_password/3, change_email/3,
         follow/2, unfollow/2, follow_multiple/2, unfollow_multiple/2,
         save_post/2, save_posts/2, unsave_post/2, unsave_posts/2,
         get_save_posts/1, get_follower/1, get_following/1,
         block/2, unblock/2, get_blocked/1, search_user/1, search_user_pattern/1,
         insert_avatar/2, insert_banner/2, report_user/4, update_last_activity/2,
         last_activity_status/1, make_private/1, make_public/1]).

-define(LIMIT_SEARCH, 50).

%%% check user credentials
-spec login(Email :: term(), Password :: term()) -> wrong_email_or_password | logged_in.
login(Email, Password) ->
  {atomic, Res} = mnesia:transaction(fun() ->
                                        check_user_credential(Email, Password)
                                     end),
  case Res of
    {true, _} -> logged_in;
    false -> wrong_email_or_password
  end.

%%% Fields: [field_a, field_b, field_c]
%%% Values: [a,b,c]
set_user_info(Username, Fields, Values) ->
  List = lists:zip(Fields, Values),
  Fun = fun() ->
          case get_user_in_transaction(Username) of
            not_existed -> not_existed;
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

%% Register User account
insert(Username, Password, Email) ->
    %% check username exist or not
    Fun = fun() ->
            case {check_username(Username), check_email(Email)} of
              {undefined, undefined} ->
                Now = calendar:universal_time(),
                Id = nanoid:gen(),
                TokenID = nanoid:gen(),
                Address = key_guardian:gen_address(80),
                KNode = kademlia:insert_node(Id),
                User = #user{id = Id,
                             username = Username,
                             password = erlpass:hash(Password),
                             email = Email,
                             address = Address,
                             knode = KNode,
                             date_created = Now,
                             token_id = TokenID,
                             level = 1,
                             last_activity = Now},
                mnesia:write(User),
                Id;
              {username_existed, _} -> username_and_email_existed;
              {_, email_existed} -> username_and_email_existed;
              {username_existed, email_existed} -> username_and_email_existed
            end
      end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

insert_media(Id, Type, Url) ->
  Fun = fun() ->
          [User] =  mnesia:read(user, Id),
          Media = User#user.media,
          NewMedia = case lists:keymember(Type, 1, Media) of
                        true ->
                          lists:keyreplace(Type, 1, Media, {Type, Url});
                        false ->
                          [{Type, Url}|Media]
                     end,
          mnesia:write(User#user{media = NewMedia})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

insert_avatar(Id, AvatarUrl) ->
  Fun = fun() ->
            [User] = mnesia:read(user, Id),
            mnesia:write(User#user{avatar_url = AvatarUrl,
                                   date_updated = calendar:universal_time()}),
            User
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  case Res of
    {atomic, [User]} -> User;
    Res -> Res
  end.

insert_banner(Id, BannerUrl) ->
  Fun = fun() ->
            [User] = mnesia:read(user, Id),
            mnesia:write(User#user{banner_url = BannerUrl,
                                   date_updated = calendar:universal_time()}),
            User
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  case Res of
    {atomic, [User]} -> User;
    Res -> Res
  end.

get_media(Id, Type) ->
  Fun = fun() ->
          [User] = mnesia:read({user, Id}),
          Media = User#user.media,
          proplists:get_value(Type, Media, undefined)
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

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

get_user_in_transaction(Username) ->
    Res = mnesia:read(user, Username),
    case Res of
      [] -> not_existed;
      [User] -> User
    end.

get_users() ->
    Fun = fun() ->
            mnesia:all_keys(user)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_password(Id) ->
    F = fun() ->
        mnesia:read(user, Id)
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
    {atomic, [User]} -> User;
    _ -> error
  end.

get_user_by_id(Id) ->
  Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#user{id = Id, _= '_'})
          end),
  case Res of
    {atomic, []} -> user_not_exist;
    {atomic, [User]} -> User;
    _ -> error
  end.

% TODO: add verified or confirmed field
get_token_by_id(TokenID) ->
  Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#user{token_id = TokenID, _= '_'})
          end),
  case Res of
    {atomic, []} -> token_not_exist;
    {atomic, [User]} -> User;
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

%% follow(myID, UserID)
follow(Id, Following) ->
    Fun = fun() ->
            [User] = mnesia:match_object(#user{id = Id, _ = '_'}),
            NewFollowList = [Following|User#user.following],
            mnesia:write(User#user{following = NewFollowList,
                                   date_updated = calendar:universal_time()}),

            %% update Following that have a new follower
            [FollowingUser] = mnesia:read(user, Following),
            mnesia:write(FollowingUser#user{follower = [Id| FollowingUser#user.follower],
                                            date_updated = calendar:universal_time()})
        end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

%% unfollow(MyID, UserID)
unfollow(Id, Following) ->
    Fun = fun() ->
            [User] = mnesia:match_object(#user{id = Id, _ = '_'}),
            NewFollowList = lists:delete(Following, User#user.following),
            mnesia:write(User#user{following = NewFollowList,
                                   date_updated = calendar:universal_time()}),

            [FollowingUser] = mnesia:read(user, Following),
            NewFollower = lists:delete(Id, FollowingUser#user.follower),
            mnesia:write(FollowingUser#user{follower = NewFollower,
                                            date_updated = calendar:universal_time()})
        end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

follow_multiple(Id, Others) ->
    lists:foreach(fun(Other) ->
                    follow(Id, Other)
                  end, Others).

unfollow_multiple(Id, Others) ->
    lists:foreach(fun(Other) ->
                    unfollow(Id, Other)
                  end, Others).

%% follow/unfollow posts
save_post(Id, PostId) ->
    Fun = fun() ->
                [User] = mnesia:read(user, Id),
                NewFollowList = [PostId|User#user.saved_posts],
                mnesia:write(User#user{saved_posts = NewFollowList,
                                       date_updated = calendar:universal_time()})
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

unsave_post(Id, PostId) ->
    Fun = fun() ->
                [User] = mnesia:read(user, Id),
                NewFollowList = lists:delete(PostId, User#user.saved_posts),
                mnesia:write(User#user{saved_posts = NewFollowList,
                                       date_updated = calendar:universal_time()})
          end,
    mnesia:transaction(Fun).

save_posts(Id, PostIds) ->
    lists:foreach(fun(PostId) ->
                    save_post(Id, PostId)
                  end, PostIds).

unsave_posts(Id, PostIds) ->
    lists:foreach(fun(PostId) ->
                    unsave_post(Id, PostId)
                  end, PostIds).

%% get save post, follower, following
get_save_posts(Id) ->
  {atomic, Res} = mnesia:transaction(fun() ->
                                      [User] = mnesia:read(user, Id),
                                      User#user.saved_posts
                                     end),
  Res.

get_following(Id) ->
  Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#user{id = Id, _= '_'})
          end),
  case Res of
    {atomic, []} -> user_not_exist;
    {atomic, [User]} -> User#user.following;
    _ -> error
  end.

get_follower(Id) ->
  Res = mnesia:transaction(fun() ->
                               mnesia:match_object(#user{id = Id, _= '_'})
                           end),
  case Res of
    {atomic, []} -> user_not_exist;
    {atomic, [User]} -> User#user.follower;
    _ -> error
  end.

get_user_info(Id, Fields) ->
  Fun = fun() ->
          [User] = mnesia:read(user, Id),
          Dict = User#user.other_info,
          lists:map(fun(Field) ->
                      proplists:get_value(Field, Dict, undefined)
                    end,
                    Fields)
        end,

  {atomic, Res} = mnesia:transaction(Fun),
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

check_user_credential(Email, Password) ->
  Object =  mnesia:match_object(#user{email = Email,
                                      _ = '_'}),
  case Object of
    [] -> false;
    [User] ->
      case erlpass:match(Password, User#user.password) of
        true -> {true, User};
        false -> false
      end
  end.

%% Id = MyID, Blocked = UserID
block(Id, Blocked) ->
  Fun = fun() ->
          [#user{blocked = BlockedList} = User] = mnesia:read(user, Id),
          mnesia:write(User#user{blocked = [Blocked|BlockedList],
                                 date_updated = calendar:universal_time()})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%% Id = MyID, Unblocked = UserID 
unblock(Id, Unblocked) ->
  Fun = fun() ->
          [#user{blocked = BlockedList} = User] = mnesia:read(user, Id),
          mnesia:write(User#user{blocked = lists:delete(Unblocked, BlockedList),
                                 date_updated = calendar:universal_time()})
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

% Get Blocked USers (MyID)
get_blocked(Id) ->
  Fun = fun() ->
          [#user{blocked = BlockedList}] = mnesia:read(user, Id),
          BlockedList
        end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

% Search user by USername
search_user(Username) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#user{username = Username, _= '_'})
            end),
    case Res of
        {atomic, []} -> username_not_exist;
        {atomic, [User]} -> User;
        _ -> error
    end.

search_user_pattern(Pattern) ->
    Fun = fun() ->
          mnesia:all_keys(user)
        end,

  {atomic, Names} = mnesia:transaction(Fun),
  search_user_pattern(Pattern, Names, ?LIMIT_SEARCH, []).


search_user_pattern(_Pattern, [], ?LIMIT_SEARCH, Acc) -> Acc;
search_user_pattern(_Pattern, _Names, ?LIMIT_SEARCH, Acc) when ?LIMIT_SEARCH == length(Acc) -> Acc;
search_user_pattern(Pattern, [H|T]= _Names, ?LIMIT_SEARCH, Acc) ->
  case re:run(H, Pattern, [caseless]) of
    nomatch ->
      search_user_pattern(Pattern, T, ?LIMIT_SEARCH, Acc);
    {match, _} ->
      search_user_pattern(Pattern, T, ?LIMIT_SEARCH, [H|Acc])
  end.

% Report User 
report_user(MyID, UserID, Type, Description) ->
  Fun = fun() ->
    ID = nanoid:gen(),
    mnesia:read(user, MyID),
        Report = #report{
          id = ID,
          type = Type,
          description = Description,
          reporter = MyID,
          user = UserID,
          date_created = calendar:universal_time()},
        mnesia:write(Report),
        ID
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

update_last_activity(UserID, Date) ->
  Fun = fun() ->
    [User] = mnesia:read(user, UserID),
    mnesia:write(User#user{last_activity = Date})
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

last_activity_status(UserID) ->
  User = get_user_by_id(UserID),
  LastActivity = User#user.last_activity,
  LastActivity.

make_private(UserID) ->
  Fun = fun() ->
    Message = "The Profile is Private now",
    [User] = mnesia:read(user, UserID),
    mnesia:write(User#user{private = true}),
    Message
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

make_public(UserID) ->
  Fun = fun() ->
    Message = "The profile is public now",
    [User] = mnesia:read(user, UserID),
    mnesia:write(User#user{private = false}),
    Message
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.
    