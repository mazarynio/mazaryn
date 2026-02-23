-module(near_staking).
-author("Zaryn Technologies").
-export([
    stake/4,
    unstake/3,
    unstake_all/2,
    withdraw/3,
    withdraw_all/2,
    get_staked_balance/2,
    get_all_staked/2,
    get_validators/1,
    get_validator_info/2,
    get_epoch_seat_price/1,
    get_user_stakes/2
]).

-define(NEAR_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 60000).

stake(Token, WalletId, ValidatorId, AmountNear) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        validator_id => ensure_binary(ValidatorId),
        amount_near => ensure_binary(AmountNear)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/stake",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_stake_response(ResponseBody);
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

unstake(Token, WalletId, AmountNear) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        amount_near => ensure_binary(AmountNear)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/unstake",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_stake_response(ResponseBody);
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

unstake_all(Token, WalletId) ->
    RequestBody = #{wallet_id => ensure_binary(WalletId)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/unstake-all",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_stake_response(ResponseBody);
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

withdraw(Token, WalletId, AmountNear) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        amount_near => ensure_binary(AmountNear)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/withdraw",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_stake_response(ResponseBody);
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

withdraw_all(Token, WalletId) ->
    RequestBody = #{wallet_id => ensure_binary(WalletId)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/withdraw-all",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_stake_response(ResponseBody);
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

get_staked_balance(Token, WalletId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/balance/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AccountId} = Response ->
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                        account_id => binary_to_list(AccountId),
                        staked_near => maps:get(<<"staked_near">>, Response, <<"0">>),
                        unstaked_near => maps:get(<<"unstaked_near">>, Response, <<"0">>),
                        can_withdraw => maps:get(<<"can_withdraw">>, Response, false),
                        validator_id => case maps:get(<<"validator_id">>, Response, null) of
                            null -> undefined;
                            V -> binary_to_list(V)
                        end
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

get_all_staked(Token, WalletId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/all/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AccountId, <<"stakes">> := Stakes} = Response ->
                    ParsedStakes = lists:map(fun(S) ->
                        #{
                            validator_id => binary_to_list(maps:get(<<"validator_id">>, S, <<"">>)),
                            staked_near => maps:get(<<"staked_near">>, S, <<"0">>),
                            unstaked_near => maps:get(<<"unstaked_near">>, S, <<"0">>),
                            can_withdraw => maps:get(<<"can_withdraw">>, S, false)
                        }
                    end, Stakes),
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                        account_id => binary_to_list(AccountId),
                        stakes => ParsedStakes,
                        total_staked_near => maps:get(<<"total_staked_near">>, Response, <<"0">>),
                        total_unstaked_near => maps:get(<<"total_unstaked_near">>, Response, <<"0">>)
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

get_validators(Token) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/validators",
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"validators">> := Validators} = Response ->
                    ParsedValidators = lists:map(fun parse_validator/1, Validators),
                    {ok, #{
                        validators => ParsedValidators,
                        total => maps:get(<<"total">>, Response, length(ParsedValidators)),
                        epoch_height => maps:get(<<"epoch_height">>, Response, undefined)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_validator_info(Token, ValidatorId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/validators/" ++ ensure_string(ValidatorId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"validator_id">> := _} = Response ->
                    {ok, parse_validator(Response)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, validator_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_epoch_seat_price(Token) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/epoch-seat-price",
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"seat_price_near">> := SeatPrice} = Response ->
                    {ok, #{
                        seat_price_near => SeatPrice,
                        epoch_height => maps:get(<<"epoch_height">>, Response, undefined),
                        epoch_start_height => maps:get(<<"epoch_start_height">>, Response, undefined)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_user_stakes(Token, UserId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/staking/user/" ++ ensure_string(UserId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"stakes">> := Stakes} = Response ->
                    ParsedStakes = lists:map(fun(S) ->
                        #{
                            stake_id => binary_to_list(maps:get(<<"stake_id">>, S, <<"">>)),
                            wallet_id => binary_to_list(maps:get(<<"wallet_id">>, S, <<"">>)),
                            validator_account_id => case maps:get(<<"validator_account_id">>, S, null) of
                                null -> undefined;
                                V -> binary_to_list(V)
                            end,
                            amount_near => maps:get(<<"amount_near">>, S, <<"0">>),
                            status => binary_to_list(maps:get(<<"status">>, S, <<"staked">>)),
                            transaction_hash => binary_to_list(maps:get(<<"transaction_hash">>, S, <<"">>)),
                            created_at => maps:get(<<"created_at">>, S),
                            unstaked_at => maps:get(<<"unstaked_at">>, S, undefined),
                            withdrawn_at => maps:get(<<"withdrawn_at">>, S, undefined)
                        }
                    end, Stakes),
                    {ok, #{
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response, <<"">>)),
                        stakes => ParsedStakes,
                        total => maps:get(<<"total">>, Response, length(ParsedStakes))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_validator(V) ->
    #{
        validator_id => binary_to_list(maps:get(<<"validator_id">>, V, <<"">>)),
        public_key => case maps:get(<<"public_key">>, V, null) of
            null -> undefined;
            PK -> binary_to_list(PK)
        end,
        stake_near => maps:get(<<"stake_near">>, V, <<"0">>),
        delegators => maps:get(<<"delegators">>, V, 0),
        commission_pct => maps:get(<<"commission_pct">>, V, undefined),
        uptime_pct => maps:get(<<"uptime_pct">>, V, undefined),
        is_slashed => maps:get(<<"is_slashed">>, V, false),
        shards => maps:get(<<"shards">>, V, [])
    }.

parse_stake_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"transaction_hash">> := TxHash} = Response ->
                {ok, #{
                    transaction_hash => binary_to_list(TxHash),
                    wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                    validator_id => case maps:get(<<"validator_id">>, Response, null) of
                        null -> undefined;
                        V -> binary_to_list(V)
                    end,
                    amount_near => maps:get(<<"amount_near">>, Response, undefined),
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
