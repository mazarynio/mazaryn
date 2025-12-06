-module(userdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-export([
    set_user_info/3,
    get_user_info/2,
    insert/3,
    insert_media/3,
    get_media/2,
    login/2,
    get_user/1,
    get_user_id/1,
    get_user_by_email/1,
    get_user_by_id/1,
    get_token_by_id/1,
    get_users/0,
    delete_user/1,
    get_password/1,
    change_username/3,
    change_password/3,
    change_email/3,
    follow/2,
    unfollow/2,
    follow_multiple/2,
    unfollow_multiple/2,
    save_post/2,
    save_posts/2,
    unsave_post/2,
    unsave_posts/2,
    get_save_posts/1,
    get_follower/1,
    get_following/1,
    block/2,
    unblock/2,
    get_blocked/1,
    search_user/1,
    search_user_pattern/1,
    insert_avatar/2,
    insert_banner/2,
    get_avatar/1,
    get_banner/1,
    report_user/4,
    update_last_activity/2,
    last_activity_status/1,
    make_private/1,
    make_public/1,
    get_token/1,
    validate_user/1,
    insert_concurrent/3,
    get_following_usernames/1,
    search_followings/2,
    get_follower_usernames/1,
    search_followers/2,
    get_user_level/1,
    get_user_token/1,
    set_verification_token/2,
    verify_email_token/1,
    get_user_by_verification_token/1,
    mark_user_as_verified/1
]).
-define(LIMIT_SEARCH, 50).
-define(MAX_RETRIES, 10).
-define(BACKOFF_TIME, 20).

login(Email, Password) ->
    {atomic, Res} = mnesia:transaction(fun() ->
        check_user_credential(Email, Password)
    end),
    case Res of
        {true, _} -> logged_in;
        false -> wrong_email_or_password
    end.

set_user_info(Username, Fields, Values) ->
    List = lists:zip(Fields, Values),
    Fun = fun() ->
        case get_user_in_transaction(Username) of
            not_existed -> not_existed;
            User ->
                CurrentFields = User#user.other_info,
                NewFields = lists:foldl(fun({Key, Value}, Acc) ->
                    case lists:keymember(Key, 1, Acc) of
                        true ->
                            lists:keyreplace(Key, 1, Acc, {Key, Value});
                        false ->
                            [{Key, Value} | Acc]
                    end
                end, CurrentFields, List),
                mnesia:write(User#user{other_info = NewFields,
                                       date_updated = calendar:universal_time()})
        end
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

insert(Username, Password, Email) ->
    Fun = fun() ->
        case {check_username(Username), check_email(Email)} of
            {undefined, undefined} ->
                Now = calendar:universal_time(),
                Id = nanoid:gen(),
                AI_User_ID = ai_userdb:insert(Id),
                TokenID = nanoid:gen(),
                Address = key_guardian:gen_address(80),
                QuantumID = key_guardian:gen_address(80),
                KNode = kademlia:insert_node(Id),
                P2P_Node = p2pdb:insert(Id),
                {ok, #{id := IPFSKeyBinary}} = ipfs_client_4:key_gen(Id, [{type, "ed25519"}, {'ipns-base', "base36"}]),
                IPFS_KEY = binary_to_list(IPFSKeyBinary),
                User = #user{id = Id,
                             p2p_node_address = P2P_Node,
                             ipfs_key = IPFS_KEY,
                             ai_user_id = AI_User_ID,
                             quantum_id = QuantumID,
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

insert_concurrent(Username, Password, Email) ->
    case check_username_email_concurrent(Username, Email) of
        {ok, both_available} ->
            Id = nanoid:gen(),
            Now = calendar:universal_time(),
            AIUserIdFuture = spawn_monitor(fun() ->
                exit({result, ai_userdb:insert(Id)})
            end),
            TokenIdFuture = spawn_monitor(fun() -> exit({result, nanoid:gen()}) end),
            AddressFuture = spawn_monitor(fun() -> exit({result, key_guardian:gen_address(80)}) end),
            QuantumIdFuture = spawn_monitor(fun() -> exit({result, key_guardian:gen_address(80)}) end),
            KNodeFuture = spawn_monitor(fun() -> exit({result, kademlia:insert_node(Id)}) end),
            P2PNodeFuture = spawn_monitor(fun() -> exit({result, p2pdb:insert(Id)}) end),
            IPFSKeyFuture = spawn_monitor(fun() ->
                {ok, #{id := IPFSKeyBinary}} = ipfs_client_4:key_gen(Id, [{type, "ed25519"}, {'ipns-base', "base36"}]),
                exit({result, binary_to_list(IPFSKeyBinary)})
            end),
            AIUserId = receive_result(AIUserIdFuture),
            TokenID = receive_result(TokenIdFuture),
            Address = receive_result(AddressFuture),
            QuantumID = receive_result(QuantumIdFuture),
            KNode = receive_result(KNodeFuture),
            P2P_Node = receive_result(P2PNodeFuture),
            IPFS_KEY = receive_result(IPFSKeyFuture),
            User = #user{id = Id,
                         p2p_node_address = P2P_Node,
                         ipfs_key = IPFS_KEY,
                         ai_user_id = AIUserId,
                         quantum_id = QuantumID,
                         username = Username,
                         password = erlpass:hash(Password),
                         email = Email,
                         address = Address,
                         knode = KNode,
                         date_created = Now,
                         token_id = TokenID,
                         level = 1,
                         last_activity = Now},
            case write_user_with_retry(User, ?MAX_RETRIES) of
                ok -> Id;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

check_username_email_concurrent(Username, Email) ->
    ParentPid = self(),
    UsernamePid = spawn_link(fun() ->
        Result = case mnesia:dirty_match_object(#user{username = Username, _ = '_'}) of
            [] -> available;
            _ -> username_existed
        end,
        ParentPid ! {username_check, Result}
    end),
    EmailPid = spawn_link(fun() ->
        Result = case mnesia:dirty_match_object(#user{email = Email, _ = '_'}) of
            [] -> available;
            _ -> email_existed
        end,
        ParentPid ! {email_check, Result}
    end),
    UsernameResult = receive
        {username_check, Result1} -> Result1
    after 5000 ->
        exit(UsernamePid, kill),
        {error, username_check_timeout}
    end,
    EmailResult = receive
        {email_check, Result2} -> Result2
    after 5000 ->
        exit(EmailPid, kill),
        {error, email_check_timeout}
    end,
    case {UsernameResult, EmailResult} of
        {available, available} -> {ok, both_available};
        {username_existed, _} -> {error, username_existed};
        {_, email_existed} -> {error, email_existed};
        _ -> {error, check_failed}
    end.

    receive_result({Pid, Ref}) ->
        receive
            {'DOWN', Ref, process, Pid, {result, Result}} ->
                Result;
            {'DOWN', Ref, process, Pid, Reason} ->
                error_logger:error_msg("~n~n===== CONCURRENT OPERATION FAILED =====~nProcess ~p failed with reason: ~p~n~n", [Pid, Reason]),
                exit({concurrent_operation_failed, Reason})
        after 10000 ->
            error_logger:error_msg("~n~n===== CONCURRENT OPERATION TIMEOUT =====~nProcess ~p timed out after 10 seconds~n~n", [Pid]),
            exit(Pid, kill),
            exit(timeout)
        end.

    write_user_with_retry(User, RetriesLeft) when RetriesLeft > 0 ->
        try
            mnesia:dirty_write(User),
            ok
        catch
            error:Reason:Stacktrace ->
                error_logger:error_msg("User write FAILED (error:Reason) - retries left: ~p~nReason: ~p~nStacktrace: ~p~n",
                                       [RetriesLeft, Reason, Stacktrace]),
                timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
                write_user_with_retry(User, RetriesLeft - 1);
            throw:Reason:Stacktrace ->
                error_logger:error_msg("User write FAILED (throw) - retries left: ~p~nReason: ~p~nStacktrace: ~p~n",
                                       [RetriesLeft, Reason, Stacktrace]),
                timer:sleep(?BACKOFF_TIME * ((?MAX_RETRIES - RetriesLeft) + 1)),
                write_user_with_retry(User, RetriesLeft - 1);
            exit:Reason:Stacktrace ->
                error_logger:error_msg("User write FAILED (exit) - retries left: ~p~nReason: ~p~nStacktrace: ~p~n",
                                       [RetriesLeft, Reason, Stacktrace]),
                {error, {process_died, Reason}}
        end;
    write_user_with_retry(_User, 0) ->
        error_logger:error_msg("User write FAILED - MAX RETRIES EXCEEDED~n", []),
        {error, max_retries_exceeded}.


insert_media(Id, Type, Url) ->
    Fun = fun() ->
        [User] = mnesia:read(user, Id),
        Media = User#user.media,
        NewMedia = case lists:keymember(Type, 1, Media) of
            true ->
                lists:keyreplace(Type, 1, Media, {Type, Url});
            false ->
                [{Type, Url} | Media]
        end,
        mnesia:write(User#user{media = NewMedia})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

insert_avatar(Id, AvatarUrl) ->
    Fun = fun() ->
        case mnesia:read(user, Id) of
            [] ->
                {error, user_not_found};
            [User] ->
                UpdatedUser = User#user{avatar_url = AvatarUrl,
                                        date_updated = calendar:universal_time()},
                mnesia:write(UpdatedUser),
                {ok, UpdatedUser}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, UpdatedUser}} ->
            UpdatedUser;
        {aborted, Reason} ->
            {error, Reason}
    end.

insert_banner(Id, BannerUrl) ->
    Fun = fun() ->
        case mnesia:read(user, Id) of
            [] ->
                {error, user_not_found};
            [User] ->
                UpdatedUser = User#user{banner_url = BannerUrl,
                                        date_updated = calendar:universal_time()},
                mnesia:write(UpdatedUser),
                {ok, UpdatedUser}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, UpdatedUser}} ->
            UpdatedUser;
        {aborted, Reason} ->
            {error, Reason}
    end.

get_avatar(UserID) ->
    F = fun() ->
        mnesia:read(user, UserID)
    end,
    Res = mnesia:transaction(F),
    case Res of
        {atomic, [User]} -> User#user.avatar_url;
        {atomic, []} -> user_not_existed;
        _ -> error
    end.

get_banner(UserID) ->
    F = fun() ->
        mnesia:read(user, UserID)
    end,
    Res = mnesia:transaction(F),
    case Res of
        {atomic, [User]} -> User#user.banner_url;
        {atomic, []} -> user_not_existed;
        _ -> error
    end.

get_user_level(UserID) ->
    F = fun() ->
        mnesia:read(user, UserID)
    end,
    Res = mnesia:transaction(F),
    case Res of
        {atomic, [User]} -> User#user.level;
        {atomic, []} -> user_not_existed;
        _ -> error
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

get_user_id(Username) ->
    case get_user(Username) of
        not_exist ->
            {error, {user_not_found, Username}};
        User ->
            User#user.id
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

get_user_token(User_ID) ->
    F = fun() ->
        mnesia:read(user, User_ID)
    end,
    Res = mnesia:transaction(F),
    case Res of
        {atomic, [User]} -> User#user.token_id;
        {atomic, []} -> user_not_existed;
        _ -> error
    end.

get_user_by_email(Email) ->
    Res = mnesia:transaction(fun() ->
        mnesia:match_object(#user{email = Email, _ = '_'})
    end),
    case Res of
        {atomic, []} -> user_not_exist;
        {atomic, [User]} -> User;
        _ -> error
    end.

get_user_by_id(Id) ->
    Res = mnesia:transaction(fun() ->
        mnesia:match_object(#user{id = Id, _ = '_'})
    end),
    case Res of
        {atomic, []} -> user_not_exist;
        {atomic, [User]} -> User;
        _ -> error
    end.

get_token_by_id(TokenID) ->
    Res = mnesia:transaction(fun() ->
        mnesia:match_object(#user{token_id = TokenID, _ = '_'})
    end),
    case Res of
        {atomic, []} -> token_not_exist;
        {atomic, [User]} -> validate_user(User#user.id);
        _ -> error
    end.

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
        [User] = mnesia:match_object(#user{username = Username, _ = '_'}),
        mnesia:delete_object(User)
    end,
    mnesia:activity(transaction, F).

follow(Id, Following) ->
    Fun = fun() ->
        [User] = mnesia:match_object(#user{id = Id, _ = '_'}),
        NewFollowList = [Following | User#user.following],
        mnesia:write(User#user{following = NewFollowList,
                               date_updated = calendar:universal_time()}),
        [FollowingUser] = mnesia:read(user, Following),
        mnesia:write(FollowingUser#user{follower = [Id | FollowingUser#user.follower],
                                        date_updated = calendar:universal_time()})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

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

save_post(Id, PostId) ->
    Fun = fun() ->
        [User] = mnesia:read(user, Id),
        NewFollowList = [PostId | User#user.saved_posts],
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

get_save_posts(Id) ->
    {atomic, Res} = mnesia:transaction(fun() ->
        [User] = mnesia:read(user, Id),
        User#user.saved_posts
    end),
    Res.

get_following(Id) ->
    Res = mnesia:transaction(fun() ->
        mnesia:match_object(#user{id = Id, _ = '_'})
    end),
    case Res of
        {atomic, []} -> user_not_exist;
        {atomic, [User]} -> User#user.following;
        _ -> error
    end.

get_following_usernames(Id) ->
    case get_following(Id) of
        user_not_exist -> user_not_exist;
        error -> error;
        FollowingIds when is_list(FollowingIds) ->
            Res = mnesia:transaction(fun() ->
                lists:foldl(fun(FollowingId, Acc) ->
                    case mnesia:match_object(#user{id = FollowingId, _ = '_'}) of
                        [User] -> [User#user.username | Acc];
                        [] -> Acc
                    end
                end, [], FollowingIds)
            end),
            case Res of
                {atomic, Usernames} -> lists:reverse(Usernames);
                _ -> error
            end
    end.

search_followings(MyUserId, TargetUsername) ->
    case get_following(MyUserId) of
        user_not_exist -> user_not_exist;
        error -> error;
        FollowingIds when is_list(FollowingIds) ->
            Res = mnesia:transaction(fun() ->
                lists:foldl(fun(FollowingId, Acc) ->
                    case mnesia:match_object(#user{id = FollowingId, username = TargetUsername, _ = '_'}) of
                        [User] -> User;
                        [] -> Acc
                    end
                end, user_not_exist, FollowingIds)
            end),
            case Res of
                {atomic, User} when is_record(User, user) -> User;
                {atomic, user_not_exist} -> user_not_exist;
                _ -> error
            end
    end.

get_follower(Id) ->
    Res = mnesia:transaction(fun() ->
        mnesia:match_object(#user{id = Id, _ = '_'})
    end),
    case Res of
        {atomic, []} -> user_not_exist;
        {atomic, [User]} -> User#user.follower;
        _ -> error
    end.

get_follower_usernames(Id) ->
    case get_follower(Id) of
        user_not_exist -> user_not_exist;
        error -> error;
        FollowerIds when is_list(FollowerIds) ->
            Res = mnesia:transaction(fun() ->
                lists:foldl(fun(FollowerId, Acc) ->
                    case mnesia:match_object(#user{id = FollowerId, _ = '_'}) of
                        [User] -> [User#user.username | Acc];
                        [] -> Acc
                    end
                end, [], FollowerIds)
            end),
            case Res of
                {atomic, Usernames} -> lists:reverse(Usernames);
                _ -> error
            end
    end.

search_followers(MyUserId, TargetUsername) ->
    case get_follower(MyUserId) of
        user_not_exist -> user_not_exist;
        error -> error;
        FollowerIds when is_list(FollowerIds) ->
            Res = mnesia:transaction(fun() ->
                lists:foldl(fun(FollowerId, Acc) ->
                    case mnesia:match_object(#user{id = FollowerId, username = TargetUsername, _ = '_'}) of
                        [User] -> User;
                        [] -> Acc
                    end
                end, user_not_exist, FollowerIds)
            end),
            case Res of
                {atomic, User} when is_record(User, user) -> User;
                {atomic, user_not_exist} -> user_not_exist;
                _ -> error
            end
    end.

get_user_info(Id, Fields) ->
    Fun = fun() ->
        [User] = mnesia:read(user, Id),
        Dict = User#user.other_info,
        lists:map(fun(Field) ->
            proplists:get_value(Field, Dict, undefined)
        end, Fields)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

check_username(Username) ->
    Object = mnesia:match_object(#user{username = Username, _ = '_'}),
    case Object of
        [] -> undefined;
        _ -> username_existed
    end.

check_email(Email) ->
    Object = mnesia:match_object(#user{email = Email, _ = '_'}),
    case Object of
        [] -> undefined;
        _ -> email_existed
    end.

check_user_credential(Email, Password) ->
    Object = mnesia:match_object(#user{email = Email, _ = '_'}),
    case Object of
        [] -> false;
        [User] ->
            case erlpass:match(Password, User#user.password) of
                true -> {true, User};
                false -> false
            end
    end.

block(Id, Blocked) ->
    Fun = fun() ->
        [#user{blocked = BlockedList} = User] = mnesia:read(user, Id),
        mnesia:write(User#user{blocked = [Blocked | BlockedList],
                               date_updated = calendar:universal_time()})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

unblock(Id, Unblocked) ->
    Fun = fun() ->
        [#user{blocked = BlockedList} = User] = mnesia:read(user, Id),
        mnesia:write(User#user{blocked = lists:delete(Unblocked, BlockedList),
                               date_updated = calendar:universal_time()})
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_blocked(Id) ->
    Fun = fun() ->
        [#user{blocked = BlockedList}] = mnesia:read(user, Id),
        BlockedList
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

search_user(Username) ->
    Res = mnesia:transaction(fun() ->
        mnesia:match_object(#user{username = Username, _ = '_'})
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
search_user_pattern(Pattern, [H | T], ?LIMIT_SEARCH, Acc) ->
    case re:run(H, Pattern, [caseless]) of
        nomatch ->
            search_user_pattern(Pattern, T, ?LIMIT_SEARCH, Acc);
        {match, _} ->
            search_user_pattern(Pattern, T, ?LIMIT_SEARCH, [H | Acc])
    end.

report_user(MyID, UserID, Type, Description) ->
    Fun = fun() ->
        ID = nanoid:gen(),
        mnesia:read(user, MyID),
        Report = #report{id = ID,
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
    User#user.last_activity.

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

get_token(TokenID) ->
    F = fun() ->
        mnesia:index_read(user, TokenID, token_id)
    end,
    Res = mnesia:transaction(F),
    case Res of
        {atomic, [User]} -> User;
        {atomic, []} -> not_exist;
        _ -> error
    end.

validate_user(UserID) ->
    Fun = fun() ->
        case get_user_by_id(UserID) of
            {error, Reason} ->
                {error, Reason};
            User ->
                UpdatedUser = User#user{verified = true},
                mnesia:write(UpdatedUser),
                UpdatedUser
        end
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

set_verification_token(UserId, Token) ->
    Fun = fun() ->
        case mnesia:read(user, UserId) of
            [] -> {error, user_not_found};
            [User] ->
                UpdatedUser = User#user{token_id = Token,
                                        date_updated = calendar:universal_time()},
                mnesia:write(UpdatedUser),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, Reason}
    end.

verify_email_token(Token) ->
    Fun = fun() ->
        case mnesia:match_object(#user{token_id = Token, _ = '_'}) of
            [] -> {error, token_not_found};
            [User] ->
                UpdatedUser = User#user{verified = true,
                                        date_updated = calendar:universal_time()},
                mnesia:write(UpdatedUser),
                {ok, User#user.id}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, UserId}} -> {ok, UserId};
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, Reason}
    end.

get_user_by_verification_token(Token) ->
    Res = mnesia:transaction(fun() ->
        mnesia:match_object(#user{token_id = Token, _ = '_'})
    end),
    case Res of
        {atomic, []} -> user_not_exist;
        {atomic, [User]} -> User;
        _ -> error
    end.

mark_user_as_verified(UserId) ->
    Fun = fun() ->
        case mnesia:read(user, UserId) of
            [] -> {error, user_not_found};
            [User] ->
                UpdatedUser = User#user{verified = true,
                                        date_updated = calendar:universal_time()},
                mnesia:write(UpdatedUser),
                ok
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
        {aborted, Reason} -> {error, Reason}
    end.
