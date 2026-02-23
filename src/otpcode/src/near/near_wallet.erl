-module(near_wallet).
-author("Zaryn Technologies").
-export([
    create_wallet/2,
    create_wallet/3,
    import_wallet/3,
    import_wallet/4,
    get_balance/2,
    get_multi_balance/2,
    transfer/4,
    transfer/5,
    get_wallet_info/2,
    get_user_wallets/2,
    delete_wallet/2
]).

-define(NEAR_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 30000).

create_wallet(Token, UserId) ->
    create_wallet(Token, UserId, undefined).

create_wallet(Token, UserId, Label) ->
    RequestBody0 = #{user_id => ensure_binary(UserId)},
    RequestBody = case Label of
        undefined -> RequestBody0;
        _ -> maps:put(label, ensure_binary(Label), RequestBody0)
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/accounts/create",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            parse_wallet_response(ResponseBody);
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_wallet_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

import_wallet(Token, UserId, SeedPhrase) ->
    import_wallet(Token, UserId, SeedPhrase, undefined).

import_wallet(Token, UserId, SeedPhrase, Label) ->
    RequestBody0 = #{
        user_id => ensure_binary(UserId),
        seed_phrase => ensure_binary(SeedPhrase)
    },
    RequestBody = case Label of
        undefined -> RequestBody0;
        _ -> maps:put(label, ensure_binary(Label), RequestBody0)
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/accounts/import",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            parse_wallet_response(ResponseBody);
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_wallet_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_balance(Token, WalletId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/state/balance/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AccountId} = Response ->
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                        account_id => binary_to_list(AccountId),
                        balance_near => maps:get(<<"balance_near">>, Response),
                        balance_yocto => binary_to_list(maps:get(<<"balance_yocto">>, Response, <<"">>)),
                        staked_near => maps:get(<<"staked_near">>, Response, undefined),
                        storage_used => maps:get(<<"storage_used">>, Response, undefined)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, wallet_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_multi_balance(Token, WalletId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/state/multi-balance/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AccountId} = Response ->
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                        account_id => binary_to_list(AccountId),
                        near => maps:get(<<"near">>, Response, undefined),
                        usdt => maps:get(<<"usdt">>, Response, undefined),
                        tokens => maps:get(<<"tokens">>, Response, [])
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, wallet_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

transfer(Token, FromWalletId, ToAccountId, AmountNear) ->
    transfer(Token, FromWalletId, ToAccountId, AmountNear, undefined).

transfer(Token, FromWalletId, ToAccountId, AmountNear, Memo) ->
    RequestBody0 = #{
        from_wallet_id => ensure_binary(FromWalletId),
        receiver_id => ensure_binary(ToAccountId),
        amount_near => ensure_binary(AmountNear)
    },
    RequestBody = case Memo of
        undefined -> RequestBody0;
        _ -> maps:put(memo, ensure_binary(Memo), RequestBody0)
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/tokens/transfer",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"transaction_hash">> := TxHash} = Response ->
                    {ok, #{
                        transaction_hash => binary_to_list(TxHash),
                        from_account_id => binary_to_list(maps:get(<<"from_account_id">>, Response)),
                        receiver_id => binary_to_list(maps:get(<<"receiver_id">>, Response)),
                        amount_near => maps:get(<<"amount_near">>, Response),
                        status => binary_to_list(maps:get(<<"status">>, Response)),
                        timestamp => maps:get(<<"timestamp">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
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

get_wallet_info(Token, WalletId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/accounts/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"wallet_id">> := WId} = Response ->
                    {ok, #{
                        wallet_id => binary_to_list(WId),
                        account_id => binary_to_list(maps:get(<<"account_id">>, Response)),
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                        label => case maps:get(<<"label">>, Response, null) of
                            null -> undefined;
                            L -> binary_to_list(L)
                        end,
                        network => binary_to_list(maps:get(<<"network">>, Response, <<"testnet">>)),
                        created_at => maps:get(<<"created_at">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_user_wallets(Token, UserId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/accounts/user/" ++ ensure_string(UserId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"wallets">> := Wallets} = Response ->
                    Parsed = lists:map(fun(W) ->
                        #{
                            wallet_id => binary_to_list(maps:get(<<"wallet_id">>, W)),
                            account_id => binary_to_list(maps:get(<<"account_id">>, W)),
                            user_id => binary_to_list(maps:get(<<"user_id">>, W)),
                            label => case maps:get(<<"label">>, W, null) of
                                null -> undefined;
                                L -> binary_to_list(L)
                            end,
                            network => binary_to_list(maps:get(<<"network">>, W, <<"testnet">>)),
                            created_at => maps:get(<<"created_at">>, W)
                        }
                    end, Wallets),
                    {ok, #{
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                        wallets => Parsed,
                        total => maps:get(<<"total">>, Response)
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

delete_wallet(Token, WalletId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/accounts/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(delete, {ApiUrl, Headers, "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            {ok, deleted};
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_wallet_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"wallet_id">> := WalletId, <<"account_id">> := AccountId} = Response ->
                {ok, #{
                    wallet_id => binary_to_list(WalletId),
                    account_id => binary_to_list(AccountId),
                    user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                    label => case maps:get(<<"label">>, Response, null) of
                        null -> undefined;
                        L -> binary_to_list(L)
                    end,
                    network => binary_to_list(maps:get(<<"network">>, Response, <<"testnet">>)),
                    created_at => maps:get(<<"created_at">>, Response)
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
