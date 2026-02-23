-module(near_tokens).
-author("Zaryn Technologies").
-export([
    transfer_near/4,
    transfer_near/5,
    transfer_ft/5,
    transfer_ft/6,
    transfer_usdt/4,
    transfer_usdt/5,
    get_ft_balance/3,
    get_usdt_balance/2,
    register_ft_account/3,
    get_token_metadata/2
]).

-define(NEAR_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 60000).

transfer_near(Token, FromWalletId, ReceiverAccountId, AmountNear) ->
    transfer_near(Token, FromWalletId, ReceiverAccountId, AmountNear, undefined).

transfer_near(Token, FromWalletId, ReceiverAccountId, AmountNear, Memo) ->
    RequestBody0 = #{
        from_wallet_id => ensure_binary(FromWalletId),
        receiver_id => ensure_binary(ReceiverAccountId),
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
            parse_transfer_response(ResponseBody);
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

transfer_ft(Token, FromWalletId, ReceiverAccountId, ContractId, Amount) ->
    transfer_ft(Token, FromWalletId, ReceiverAccountId, ContractId, Amount, undefined).

transfer_ft(Token, FromWalletId, ReceiverAccountId, ContractId, Amount, Memo) ->
    RequestBody0 = #{
        from_wallet_id => ensure_binary(FromWalletId),
        receiver_id => ensure_binary(ReceiverAccountId),
        contract_id => ensure_binary(ContractId),
        amount => ensure_binary(Amount)
    },
    RequestBody = case Memo of
        undefined -> RequestBody0;
        _ -> maps:put(memo, ensure_binary(Memo), RequestBody0)
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/tokens/ft-transfer",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_transfer_response(ResponseBody);
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

transfer_usdt(Token, FromWalletId, ReceiverAccountId, Amount) ->
    transfer_usdt(Token, FromWalletId, ReceiverAccountId, Amount, undefined).

transfer_usdt(Token, FromWalletId, ReceiverAccountId, Amount, Memo) ->
    RequestBody0 = #{
        from_wallet_id => ensure_binary(FromWalletId),
        receiver_id => ensure_binary(ReceiverAccountId),
        amount => ensure_binary(Amount)
    },
    RequestBody = case Memo of
        undefined -> RequestBody0;
        _ -> maps:put(memo, ensure_binary(Memo), RequestBody0)
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/tokens/usdt-transfer",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_transfer_response(ResponseBody);
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

get_ft_balance(Token, WalletId, ContractId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/tokens/ft-balance/" ++ ensure_string(WalletId)
             ++ "?contract_id=" ++ ensure_string(ContractId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AccountId} = Response ->
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                        account_id => binary_to_list(AccountId),
                        contract_id => binary_to_list(maps:get(<<"contract_id">>, Response)),
                        balance => binary_to_list(maps:get(<<"balance">>, Response, <<"">>)),
                        decimals => maps:get(<<"decimals">>, Response, undefined),
                        symbol => case maps:get(<<"symbol">>, Response, null) of
                            null -> undefined;
                            S -> binary_to_list(S)
                        end
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_usdt_balance(Token, WalletId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/tokens/usdt-balance/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"account_id">> := AccountId} = Response ->
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                        account_id => binary_to_list(AccountId),
                        balance => binary_to_list(maps:get(<<"balance">>, Response, <<"">>)),
                        decimals => maps:get(<<"decimals">>, Response, 6),
                        symbol => binary_to_list(maps:get(<<"symbol">>, Response, <<"USDT">>))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

register_ft_account(Token, WalletId, ContractId) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        contract_id => ensure_binary(ContractId)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/tokens/ft-register",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"transaction_hash">> := TxHash} = Response ->
                    {ok, #{
                        transaction_hash => binary_to_list(TxHash),
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                        contract_id => binary_to_list(maps:get(<<"contract_id">>, Response)),
                        status => binary_to_list(maps:get(<<"status">>, Response))
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

get_token_metadata(Token, ContractId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/tokens/metadata/" ++ ensure_string(ContractId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"contract_id">> := CId} = Response ->
                    {ok, #{
                        contract_id => binary_to_list(CId),
                        name => case maps:get(<<"name">>, Response, null) of
                            null -> undefined;
                            N -> binary_to_list(N)
                        end,
                        symbol => case maps:get(<<"symbol">>, Response, null) of
                            null -> undefined;
                            S -> binary_to_list(S)
                        end,
                        decimals => maps:get(<<"decimals">>, Response, undefined),
                        icon => case maps:get(<<"icon">>, Response, null) of
                            null -> undefined;
                            I -> binary_to_list(I)
                        end,
                        total_supply => case maps:get(<<"total_supply">>, Response, null) of
                            null -> undefined;
                            Ts -> binary_to_list(Ts)
                        end
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, contract_not_found};
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

parse_transfer_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"transaction_hash">> := TxHash} = Response ->
                {ok, #{
                    transaction_hash => binary_to_list(TxHash),
                    from_account_id => binary_to_list(maps:get(<<"from_account_id">>, Response, <<"">>)),
                    receiver_id => binary_to_list(maps:get(<<"receiver_id">>, Response, <<"">>)),
                    amount_near => maps:get(<<"amount_near">>, Response, undefined),
                    amount => maps:get(<<"amount">>, Response, undefined),
                    contract_id => case maps:get(<<"contract_id">>, Response, null) of
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
