-module(near_transactions).
-author("Zaryn Technologies").
-export([
    send_batch/3,
    call_function/5,
    call_function/6,
    call_function/7,
    view_function/3,
    view_function/4,
    sign_transaction/2,
    broadcast_transaction/2,
    get_transaction_status/2,
    get_transaction_history/2,
    get_transaction_history/3,
    get_user_transaction_history/2
]).

-define(NEAR_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 60000).

send_batch(Token, WalletId, Actions) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        actions => lists:map(fun encode_action/1, Actions)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/tx/batch",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_tx_response(ResponseBody);
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

call_function(Token, WalletId, ContractId, MethodName, Args) ->
    call_function(Token, WalletId, ContractId, MethodName, Args, <<"0">>, <<"30">>).

call_function(Token, WalletId, ContractId, MethodName, Args, DepositNear) ->
    call_function(Token, WalletId, ContractId, MethodName, Args, DepositNear, <<"30">>).

call_function(Token, WalletId, ContractId, MethodName, Args, DepositNear, GasTera) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        contract_id => ensure_binary(ContractId),
        method_name => ensure_binary(MethodName),
        args => Args,
        deposit_near => ensure_binary(DepositNear),
        gas_tera => ensure_binary(GasTera)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/tx/call",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_tx_response(ResponseBody);
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

view_function(Token, ContractId, MethodName) ->
    view_function(Token, ContractId, MethodName, #{}).

view_function(Token, ContractId, MethodName, Args) ->
    RequestBody = #{
        contract_id => ensure_binary(ContractId),
        method_name => ensure_binary(MethodName),
        args => Args
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/tx/view",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"result">> := Result} ->
                    {ok, #{result => Result}};
                Other ->
                    {ok, #{result => Other}}
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

sign_transaction(Token, SignRequest) ->
    WalletId = maps:get(wallet_id, SignRequest),
    ReceiverAccountId = maps:get(receiver_id, SignRequest),
    Actions = maps:get(actions, SignRequest, []),
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        receiver_id => ensure_binary(ReceiverAccountId),
        actions => lists:map(fun encode_action/1, Actions)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/tx/sign",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"signed_transaction">> := SignedTx} = Response ->
                    {ok, #{
                        signed_transaction => SignedTx,
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                        receiver_id => binary_to_list(maps:get(<<"receiver_id">>, Response, <<"">>))
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
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

broadcast_transaction(Token, SignedTransaction) ->
    RequestBody = #{signed_transaction => SignedTransaction},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?NEAR_API_BASE ++ "/near/tx/broadcast",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_tx_response(ResponseBody);
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

get_transaction_status(Token, TxHash) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/tx/status/" ++ ensure_string(TxHash),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"transaction_hash">> := Hash} = Response ->
                    {ok, #{
                        transaction_hash => binary_to_list(Hash),
                        status => binary_to_list(maps:get(<<"status">>, Response, <<"unknown">>)),
                        block_hash => case maps:get(<<"block_hash">>, Response, null) of
                            null -> undefined;
                            BH -> binary_to_list(BH)
                        end,
                        receipts_outcome => maps:get(<<"receipts_outcome">>, Response, [])
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} ->
            {error, transaction_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_transaction_history(Token, WalletId) ->
    get_transaction_history(Token, WalletId, 20).

get_transaction_history(Token, WalletId, Limit) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/tx/history/" ++ ensure_string(WalletId)
             ++ "?limit=" ++ integer_to_list(Limit),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"transactions">> := Txs} = Response ->
                    ParsedTxs = lists:map(fun parse_tx_record/1, Txs),
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                        transactions => ParsedTxs,
                        total => maps:get(<<"total">>, Response, length(ParsedTxs))
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

get_user_transaction_history(Token, UserId) ->
    ApiUrl = ?NEAR_API_BASE ++ "/near/tx/user/" ++ ensure_string(UserId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"transactions">> := Txs} = Response ->
                    ParsedTxs = lists:map(fun parse_tx_record/1, Txs),
                    {ok, #{
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response, <<"">>)),
                        transactions => ParsedTxs,
                        total => maps:get(<<"total">>, Response, length(ParsedTxs))
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

encode_action(#{type := transfer, amount_near := Amount}) ->
    #{type => <<"transfer">>, amount_near => ensure_binary(Amount)};
encode_action(#{type := function_call} = Action) ->
    #{
        type => <<"function_call">>,
        function_call => #{
            method_name => ensure_binary(maps:get(method_name, Action)),
            args => maps:get(args, Action, #{}),
            gas_tera => ensure_binary(maps:get(gas_tera, Action, <<"30">>)),
            deposit_near => ensure_binary(maps:get(deposit_near, Action, <<"0">>))
        }
    };
encode_action(#{type := add_full_access_key, public_key := PK}) ->
    #{type => <<"addFullAccessKey">>, public_key => ensure_binary(PK)};
encode_action(#{type := add_function_call_key} = Action) ->
    #{
        type => <<"addFunctionCallKey">>,
        function_call_key => #{
            public_key => ensure_binary(maps:get(public_key, Action)),
            contract_id => ensure_binary(maps:get(contract_id, Action)),
            method_names => lists:map(fun ensure_binary/1, maps:get(method_names, Action, [])),
            allowance_near => case maps:get(allowance_near, Action, undefined) of
                undefined -> null;
                A -> ensure_binary(A)
            end
        }
    };
encode_action(#{type := delete_key, public_key := PK}) ->
    #{type => <<"deleteKey">>, public_key => ensure_binary(PK)};
encode_action(#{type := stake} = Action) ->
    #{
        type => <<"stake">>,
        stake => #{
            amount_near => ensure_binary(maps:get(amount_near, Action)),
            public_key => ensure_binary(maps:get(public_key, Action))
        }
    };
encode_action(Other) ->
    Other.

parse_tx_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"transaction_hash">> := TxHash} = Response ->
                {ok, #{
                    transaction_hash => binary_to_list(TxHash),
                    wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response, <<"">>)),
                    status => binary_to_list(maps:get(<<"status">>, Response, <<"confirmed">>)),
                    receiver_id => case maps:get(<<"receiver_id">>, Response, null) of
                        null -> undefined;
                        R -> binary_to_list(R)
                    end,
                    actions_count => maps:get(<<"actions_count">>, Response, undefined),
                    timestamp => maps:get(<<"timestamp">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        _:_ -> {error, parse_failed}
    end.

parse_tx_record(Tx) ->
    #{
        tx_id => binary_to_list(maps:get(<<"tx_id">>, Tx, <<"">>)),
        transaction_hash => case maps:get(<<"transaction_hash">>, Tx, null) of
            null -> undefined;
            H -> binary_to_list(H)
        end,
        tx_type => binary_to_list(maps:get(<<"tx_type">>, Tx, <<"unknown">>)),
        from_account_id => case maps:get(<<"from_account_id">>, Tx, null) of
            null -> undefined;
            F -> binary_to_list(F)
        end,
        receiver_id => case maps:get(<<"receiver_id">>, Tx, null) of
            null -> undefined;
            R -> binary_to_list(R)
        end,
        amount_near => maps:get(<<"amount_near">>, Tx, undefined),
        contract_id => case maps:get(<<"contract_id">>, Tx, null) of
            null -> undefined;
            C -> binary_to_list(C)
        end,
        method_name => case maps:get(<<"method_name">>, Tx, null) of
            null -> undefined;
            M -> binary_to_list(M)
        end,
        status => binary_to_list(maps:get(<<"status">>, Tx, <<"unknown">>)),
        created_at => maps:get(<<"created_at">>, Tx)
    }.

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
