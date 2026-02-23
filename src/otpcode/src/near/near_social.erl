-module(near_social).
-author("Zaryn Technologies").
-export([
    create_post/3,
    create_post/4,
    like_post/3,
    unlike_post/3,
    comment_on_post/4,
    follow_account/3,
    unfollow_account/3,
    tip_account/4,
    tip_account/5,
    get_feed/2,
    get_feed/3,
    get_user_posts/2,
    get_user_posts/3,
    get_post/2,
    get_followers/2,
    get_following/2,
    get_social_profile/2,
    set_social_profile/3,
    get_notifications/2,
    get_notifications/3
]).

-define(NEAR_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 60000).

create_post(Token, WalletId, Text) ->
    create_post(Token, WalletId, Text, #{}).

create_post(Token, WalletId, Text, Opts) ->
    RequestBody0 = #{
        wallet_id => ensure_binary(WalletId),
        text => ensure_binary(Text)
    },
    RequestBody1 = case maps:get(media_urls, Opts, []) of
        [] -> RequestBody0;
        Urls -> maps:put(media_urls, lists:map(fun ensure_binary/1, Urls), RequestBody0)
    end,
    RequestBody2 = case maps:get(tags, Opts, []) of
        [] -> RequestBody1;
        Tags -> maps:put(tags, lists:map(fun ensure_binary/1, Tags), RequestBody1)
    end,
    RequestBody3 = case maps:get(contract, Opts, undefined) of
        undefined -> RequestBody2;
        Contract -> maps:put(contract, ensure_binary(Contract), RequestBody2)
    end,
    RequestBody = case maps:get(link, Opts, undefined) of
        undefined -> RequestBody3;
        Link -> maps:put(link, ensure_binary(Link), RequestBody3)
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/post",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_post_response(ResponseBody);
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            parse_post_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

like_post(Token, WalletId, PostKey) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        post_key => ensure_binary(PostKey)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/like",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_social_action_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

unlike_post(Token, WalletId, PostKey) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        post_key => ensure_binary(PostKey)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/unlike",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_social_action_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

comment_on_post(Token, WalletId, PostKey, Text) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        post_key => ensure_binary(PostKey),
        text => ensure_binary(Text)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/comment",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_social_action_response(ResponseBody);
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            parse_social_action_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

follow_account(Token, WalletId, TargetAccountId) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        target_account_id => ensure_binary(TargetAccountId)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/follow",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_social_action_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

unfollow_account(Token, WalletId, TargetAccountId) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        target_account_id => ensure_binary(TargetAccountId)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/unfollow",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_social_action_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

tip_account(Token, WalletId, TargetAccountId, AmountNear) ->
    tip_account(Token, WalletId, TargetAccountId, AmountNear, undefined).

tip_account(Token, WalletId, TargetAccountId, AmountNear, Memo) ->
    RequestBody0 = #{
        wallet_id => ensure_binary(WalletId),
        target_account_id => ensure_binary(TargetAccountId),
        amount_near => ensure_binary(AmountNear)
    },
    RequestBody = case Memo of
        undefined -> RequestBody0;
        _ -> maps:put(memo, ensure_binary(Memo), RequestBody0)
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/tip",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_social_action_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_feed(Token, WalletId) ->
    get_feed(Token, WalletId, #{}).

get_feed(Token, WalletId, Opts) ->
    Limit = maps:get(limit, Opts, 20),
    Offset = maps:get(offset, Opts, 0),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/feed/" ++ ensure_string(WalletId)
             ++ "?limit=" ++ integer_to_list(Limit)
             ++ "&offset=" ++ integer_to_list(Offset),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"posts">> := Posts} = Response ->
                    ParsedPosts = lists:map(fun parse_post_record/1, Posts),
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                        posts => ParsedPosts,
                        total => maps:get(<<"total">>, Response, length(ParsedPosts)),
                        has_more => maps:get(<<"has_more">>, Response, false)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, wallet_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_user_posts(Token, AccountId) ->
    get_user_posts(Token, AccountId, #{}).

get_user_posts(Token, AccountId, Opts) ->
    Limit = maps:get(limit, Opts, 20),
    Offset = maps:get(offset, Opts, 0),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/posts/" ++ ensure_string(AccountId)
             ++ "?limit=" ++ integer_to_list(Limit)
             ++ "&offset=" ++ integer_to_list(Offset),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"posts">> := Posts} = Response ->
                    ParsedPosts = lists:map(fun parse_post_record/1, Posts),
                    {ok, #{
                        account_id => binary_to_list(maps:get(<<"account_id">>, Response, <<"">>)),
                        posts => ParsedPosts,
                        total => maps:get(<<"total">>, Response, length(ParsedPosts)),
                        has_more => maps:get(<<"has_more">>, Response, false)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, account_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_post(Token, PostKey) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/post/" ++ ensure_string(PostKey),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"post_key">> := _} = Response ->
                    {ok, parse_post_record(Response)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, post_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_followers(Token, AccountId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/followers/" ++ ensure_string(AccountId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AId, <<"followers">> := Followers} = Response ->
                    {ok, #{
                        account_id => binary_to_list(AId),
                        followers => lists:map(fun binary_to_list/1, Followers),
                        total => maps:get(<<"total">>, Response, length(Followers))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, account_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_following(Token, AccountId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/following/" ++ ensure_string(AccountId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AId, <<"following">> := Following} = Response ->
                    {ok, #{
                        account_id => binary_to_list(AId),
                        following => lists:map(fun binary_to_list/1, Following),
                        total => maps:get(<<"total">>, Response, length(Following))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, account_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_social_profile(Token, AccountId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/profile/" ++ ensure_string(AccountId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AId} = Response ->
                    {ok, #{
                        account_id => binary_to_list(AId),
                        name => case maps:get(<<"name">>, Response, null) of
                            null -> undefined;
                            N -> binary_to_list(N)
                        end,
                        bio => case maps:get(<<"bio">>, Response, null) of
                            null -> undefined;
                            B -> binary_to_list(B)
                        end,
                        image => maps:get(<<"image">>, Response, undefined),
                        background_image => maps:get(<<"background_image">>, Response, undefined),
                        website => case maps:get(<<"website">>, Response, null) of
                            null -> undefined;
                            W -> binary_to_list(W)
                        end,
                        twitter => case maps:get(<<"twitter">>, Response, null) of
                            null -> undefined;
                            T -> binary_to_list(T)
                        end,
                        github => case maps:get(<<"github">>, Response, null) of
                            null -> undefined;
                            G -> binary_to_list(G)
                        end,
                        telegram => case maps:get(<<"telegram">>, Response, null) of
                            null -> undefined;
                            Tg -> binary_to_list(Tg)
                        end,
                        tags => maps:get(<<"tags">>, Response, []),
                        linktree => maps:get(<<"linktree">>, Response, #{})
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, profile_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

set_social_profile(Token, WalletId, ProfileData) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        profile => encode_profile(ProfileData)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/profile",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_social_action_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_notifications(Token, WalletId) ->
    get_notifications(Token, WalletId, #{}).

get_notifications(Token, WalletId, Opts) ->
    Limit = maps:get(limit, Opts, 20),
    Offset = maps:get(offset, Opts, 0),
    ApiUrl = ?NEAR_API_BASE ++ "/near/social/notifications/" ++ ensure_string(WalletId)
             ++ "?limit=" ++ integer_to_list(Limit)
             ++ "&offset=" ++ integer_to_list(Offset),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"notifications">> := Notifications} = Response ->
                    ParsedNotifs = lists:map(fun parse_notification/1, Notifications),
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                        notifications => ParsedNotifs,
                        total => maps:get(<<"total">>, Response, length(ParsedNotifs)),
                        has_more => maps:get(<<"has_more">>, Response, false)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, wallet_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

encode_profile(ProfileData) ->
    Fields = [name, bio, website, twitter, github, telegram],
    Base = lists:foldl(fun(Field, Acc) ->
        case maps:get(Field, ProfileData, undefined) of
            undefined -> Acc;
            Value -> maps:put(ensure_binary(Field), ensure_binary(Value), Acc)
        end
    end, #{}, Fields),
    case maps:get(tags, ProfileData, undefined) of
        undefined -> Base;
        Tags -> maps:put(<<"tags">>, lists:map(fun ensure_binary/1, Tags), Base)
    end.

parse_post_record(Post) ->
    #{
        post_key => binary_to_list(maps:get(<<"post_key">>, Post, <<"">>)),
        account_id => binary_to_list(maps:get(<<"account_id">>, Post, <<"">>)),
        text => binary_to_list(maps:get(<<"text">>, Post, <<"">>)),
        media_urls => lists:map(
            fun binary_to_list/1,
            maps:get(<<"media_urls">>, Post, [])
        ),
        tags => lists:map(
            fun binary_to_list/1,
            maps:get(<<"tags">>, Post, [])
        ),
        likes => maps:get(<<"likes">>, Post, 0),
        comments => maps:get(<<"comments">>, Post, 0),
        contract => case maps:get(<<"contract">>, Post, null) of
            null -> undefined;
            C -> binary_to_list(C)
        end,
        transaction_hash => case maps:get(<<"transaction_hash">>, Post, null) of
            null -> undefined;
            H -> binary_to_list(H)
        end,
        block_height => maps:get(<<"block_height">>, Post, undefined),
        created_at => maps:get(<<"created_at">>, Post)
    }.

parse_notification(Notif) ->
    #{
        notification_id => binary_to_list(maps:get(<<"notification_id">>, Notif, <<"">>)),
        type => binary_to_list(maps:get(<<"type">>, Notif, <<"unknown">>)),
        from_account_id => binary_to_list(maps:get(<<"from_account_id">>, Notif, <<"">>)),
        post_key => case maps:get(<<"post_key">>, Notif, null) of
            null -> undefined;
            PK -> binary_to_list(PK)
        end,
        amount_near => maps:get(<<"amount_near">>, Notif, undefined),
        text => case maps:get(<<"text">>, Notif, null) of
            null -> undefined;
            T -> binary_to_list(T)
        end,
        read => maps:get(<<"read">>, Notif, false),
        created_at => maps:get(<<"created_at">>, Notif)
    }.

parse_post_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"transaction_hash">> := TxHash} = Response ->
                {ok, #{
                    transaction_hash => binary_to_list(TxHash),
                    post_key => case maps:get(<<"post_key">>, Response, null) of
                        null -> undefined;
                        PK -> binary_to_list(PK)
                    end,
                    wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                    account_id => binary_to_list(maps:get(<<"account_id">>, Response, <<"">>)),
                    contract => case maps:get(<<"contract">>, Response, null) of
                        null -> undefined;
                        C -> binary_to_list(C)
                    end,
                    status => binary_to_list(maps:get(<<"status">>, Response, <<"confirmed">>)),
                    timestamp => maps:get(<<"timestamp">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        _:_ -> {error, parse_failed}
    end.

parse_social_action_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"transaction_hash">> := TxHash} = Response ->
                {ok, #{
                    transaction_hash => binary_to_list(TxHash),
                    wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                    status => binary_to_list(maps:get(<<"status">>, Response, <<"confirmed">>)),
                    timestamp => maps:get(<<"timestamp">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        _:_ -> {error, parse_failed}
    end.

parse_error_response(ErrorBody) ->
    try
        case jsx:decode(list_to_binary(ErrorBody), [return_maps]) of
            #{<<"error">> := ErrorMsg} ->
                {error, binary_to_list(ErrorMsg)};
            _ ->
                {error, unknown_error}
        end
    catch
        _:_ -> {error, invalid_error_response}
    end.

auth_headers(Token) ->
    [{"Authorization", "Bearer " ++ ensure_string(Token)}].

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
ensure_binary(Value) when is_float(Value) -> list_to_binary(float_to_list(Value, [{decimals, 10}, compact]));
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).

ensure_string(Value) when is_list(Value) -> Value;
ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_string(Value) when is_atom(Value) -> atom_to_list(Value);
ensure_string(Value) -> lists:flatten(io_lib:format("~p", [Value])).
