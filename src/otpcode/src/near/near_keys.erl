-module(near_keys).
-author("Zaryn Technologies").
-export([
    get_access_keys/2,
    add_full_access_key/3,
    add_function_call_key/5,
    add_function_call_key/6,
    delete_key/3,
    generate_seed_phrase/1,
    import_from_seed_phrase/3,
    import_from_seed_phrase/4,
    get_stored_keys/2,
    get_stored_keys_by_user/2
]).

-define(NEAR_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 30000).

get_access_keys(Token, WalletId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/keys/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AccountId, <<"keys">> := Keys} = Response ->
                    ParsedKeys = lists:map(fun(K) ->
                        #{
                            public_key => binary_to_list(maps:get(<<"public_key">>, K)),
                            access => maps:get(<<"access">>, K),
                            nonce => maps:get(<<"nonce">>, K, undefined)
                        }
                    end, Keys),
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                        account_id => binary_to_list(AccountId),
                        keys => ParsedKeys,
                        total => maps:get(<<"total">>, Response)
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

add_full_access_key(Token, WalletId, NewPublicKey) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        public_key => ensure_binary(NewPublicKey)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/keys/add-full-access",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_key_action_response(ResponseBody);
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

add_function_call_key(Token, WalletId, NewPublicKey, ContractId, MethodNames) ->
    add_function_call_key(Token, WalletId, NewPublicKey, ContractId, MethodNames, undefined).

add_function_call_key(Token, WalletId, NewPublicKey, ContractId, MethodNames, AllowanceNear) ->
    RequestBody0 = #{
        wallet_id => ensure_binary(WalletId),
        public_key => ensure_binary(NewPublicKey),
        contract_id => ensure_binary(ContractId),
        method_names => lists:map(fun ensure_binary/1, MethodNames)
    },
    RequestBody = case AllowanceNear of
        undefined -> RequestBody0;
        _ -> maps:put(allowance_near, ensure_binary(AllowanceNear), RequestBody0)
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/keys/add-function-call",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_key_action_response(ResponseBody);
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

delete_key(Token, WalletId, PublicKey) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        public_key => ensure_binary(PublicKey)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/keys/delete",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_key_action_response(ResponseBody);
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

generate_seed_phrase(Token) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/keys/generate-seed",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"seed_phrase">> := SeedPhrase, <<"public_key">> := PublicKey} = Response ->
                    {ok, #{
                        seed_phrase => binary_to_list(SeedPhrase),
                        public_key => binary_to_list(PublicKey),
                        implicit_account_id => case maps:get(<<"implicit_account_id">>, Response, null) of
                            null -> undefined;
                            Id -> binary_to_list(Id)
                        end
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

import_from_seed_phrase(Token, UserId, SeedPhrase) ->
    import_from_seed_phrase(Token, UserId, SeedPhrase, undefined).

import_from_seed_phrase(Token, UserId, SeedPhrase, Label) ->
    RequestBody0 = #{
        user_id => ensure_binary(UserId),
        seed_phrase => ensure_binary(SeedPhrase)
    },
    RequestBody = case Label of
        undefined -> RequestBody0;
        _ -> maps:put(label, ensure_binary(Label), RequestBody0)
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/keys/import-seed",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"wallet_id">> := WalletId, <<"account_id">> := AccountId} = Response ->
                    {ok, #{
                        wallet_id => binary_to_list(WalletId),
                        account_id => binary_to_list(AccountId),
                        public_key => binary_to_list(maps:get(<<"public_key">>, Response, <<"">>)),
                        label => case maps:get(<<"label">>, Response, null) of
                            null -> undefined;
                            L -> binary_to_list(L)
                        end
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_stored_keys(Token, WalletId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/keys/stored/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"wallet_id">> := WId, <<"keys">> := Keys} ->
                    ParsedKeys = lists:map(fun(K) ->
                        #{
                            key_id => binary_to_list(maps:get(<<"key_id">>, K)),
                            public_key => binary_to_list(maps:get(<<"public_key">>, K)),
                            key_type => binary_to_list(maps:get(<<"key_type">>, K)),
                            contract_id => case maps:get(<<"contract_id">>, K, null) of
                                null -> undefined;
                                C -> binary_to_list(C)
                            end,
                            method_names => lists:map(
                                fun binary_to_list/1,
                                maps:get(<<"method_names">>, K, [])
                            ),
                            allowance_near => case maps:get(<<"allowance_near">>, K, null) of
                                null -> undefined;
                                A -> binary_to_list(A)
                            end,
                            label => case maps:get(<<"label">>, K, null) of
                                null -> undefined;
                                L -> binary_to_list(L)
                            end,
                            created_at => maps:get(<<"created_at">>, K)
                        }
                    end, Keys),
                    {ok, #{
                        wallet_id => binary_to_list(WId),
                        keys => ParsedKeys,
                        total => length(ParsedKeys)
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

get_stored_keys_by_user(Token, UserId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/keys/user/" ++ ensure_string(UserId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"user_id">> := UId, <<"keys">> := Keys} ->
                    ParsedKeys = lists:map(fun(K) ->
                        #{
                            key_id => binary_to_list(maps:get(<<"key_id">>, K)),
                            wallet_id => binary_to_list(maps:get(<<"wallet_id">>, K)),
                            public_key => binary_to_list(maps:get(<<"public_key">>, K)),
                            key_type => binary_to_list(maps:get(<<"key_type">>, K)),
                            contract_id => case maps:get(<<"contract_id">>, K, null) of
                                null -> undefined;
                                C -> binary_to_list(C)
                            end,
                            method_names => lists:map(
                                fun binary_to_list/1,
                                maps:get(<<"method_names">>, K, [])
                            ),
                            allowance_near => case maps:get(<<"allowance_near">>, K, null) of
                                null -> undefined;
                                A -> binary_to_list(A)
                            end,
                            label => case maps:get(<<"label">>, K, null) of
                                null -> undefined;
                                L -> binary_to_list(L)
                            end,
                            created_at => maps:get(<<"created_at">>, K)
                        }
                    end, Keys),
                    {ok, #{
                        user_id => binary_to_list(UId),
                        keys => ParsedKeys,
                        total => length(ParsedKeys)
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

parse_key_action_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"transaction_hash">> := TxHash} = Response ->
                {ok, #{
                    transaction_hash => binary_to_list(TxHash),
                    wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                    account_id => binary_to_list(maps:get(<<"account_id">>, Response, <<"">>)),
                    public_key => case maps:get(<<"public_key">>, Response, null) of
                        null -> undefined;
                        PK -> binary_to_list(PK)
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
