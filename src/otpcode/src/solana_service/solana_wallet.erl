-module(solana_wallet).
-author("Zaryn Technologies").

-export([
    create_wallet/1,
    create_wallet/2,
    import_wallet/2,
    import_wallet/3,
    get_balance/1,
    transfer/3,
    transfer/4,
    get_transaction/1,
    get_wallet_info/1,
    get_user_wallets/1,
    export_private_key/1,
    delete_wallet/2
]).

-define(SOLANA_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 30000).

create_wallet(UserId) ->
    create_wallet(UserId, undefined).

create_wallet(UserId, WalletName) ->
    RequestBody = case WalletName of
        undefined ->
            #{user_id => ensure_binary(UserId)};
        _ ->
            #{
                user_id => ensure_binary(UserId),
                wallet_name => ensure_binary(WalletName)
            }
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/create",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"wallet_id">> := WalletId, <<"public_key">> := PublicKey} = Response ->
                    {ok, #{
                        wallet_id => binary_to_list(WalletId),
                        public_key => binary_to_list(PublicKey),
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                        created_at => maps:get(<<"created_at">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

import_wallet(UserId, PrivateKey) ->
    import_wallet(UserId, PrivateKey, undefined).

import_wallet(UserId, PrivateKey, WalletName) ->
    RequestBody = case WalletName of
        undefined ->
            #{
                user_id => ensure_binary(UserId),
                private_key => ensure_binary(PrivateKey)
            };
        _ ->
            #{
                user_id => ensure_binary(UserId),
                private_key => ensure_binary(PrivateKey),
                wallet_name => ensure_binary(WalletName)
            }
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/import",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"wallet_id">> := WalletId, <<"public_key">> := PublicKey} = Response ->
                    {ok, #{
                        wallet_id => binary_to_list(WalletId),
                        public_key => binary_to_list(PublicKey),
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                        created_at => maps:get(<<"created_at">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_balance(PublicKey) ->
    RequestBody = #{public_key => ensure_binary(PublicKey)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/balance",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"balance_lamports">> := BalanceLamports, <<"balance_sol">> := BalanceSol} = Response ->
                    {ok, #{
                        public_key => binary_to_list(maps:get(<<"public_key">>, Response)),
                        balance_lamports => BalanceLamports,
                        balance_sol => BalanceSol
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

transfer(FromWalletId, ToPublicKey, AmountLamports) ->
    transfer(FromWalletId, ToPublicKey, AmountLamports, undefined).

transfer(FromWalletId, ToPublicKey, AmountLamports, Memo) ->
    RequestBody = case Memo of
        undefined ->
            #{
                from_wallet_id => ensure_binary(FromWalletId),
                to_public_key => ensure_binary(ToPublicKey),
                amount_lamports => AmountLamports
            };
        _ ->
            #{
                from_wallet_id => ensure_binary(FromWalletId),
                to_public_key => ensure_binary(ToPublicKey),
                amount_lamports => AmountLamports,
                memo => ensure_binary(Memo)
            }
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/transfer",

    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"signature">> := Signature} = Response ->
                    {ok, #{
                        signature => binary_to_list(Signature),
                        from_public_key => binary_to_list(maps:get(<<"from_public_key">>, Response)),
                        to_public_key => binary_to_list(maps:get(<<"to_public_key">>, Response)),
                        amount_lamports => maps:get(<<"amount_lamports">>, Response),
                        status => binary_to_list(maps:get(<<"status">>, Response)),
                        timestamp => maps:get(<<"timestamp">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_transaction(Signature) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/transaction/" ++ ensure_string(Signature),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"signature">> := Sig} = Response ->
                    {ok, #{
                        signature => binary_to_list(Sig),
                        slot => maps:get(<<"slot">>, Response),
                        block_time => maps:get(<<"block_time">>, Response),
                        status => binary_to_list(maps:get(<<"status">>, Response)),
                        fee => maps:get(<<"fee">>, Response),
                        meta => maps:get(<<"meta">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_wallet_info(WalletId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/info/" ++ ensure_string(WalletId),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"wallet_id">> := WId} = Response ->
                    #{
                        wallet_id => binary_to_list(WId),
                        public_key => binary_to_list(maps:get(<<"public_key">>, Response)),
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                        wallet_name => case maps:get(<<"wallet_name">>, Response, null) of
                            null -> undefined;
                            Name -> binary_to_list(Name)
                        end,
                        created_at => maps:get(<<"created_at">>, Response)
                    };
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_user_wallets(UserId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/user/" ++ ensure_string(UserId),

    case httpc:request(get, {ApiUrl, []}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"wallets">> := Wallets} ->
                    ParsedWallets = lists:map(fun(W) ->
                        #{
                            wallet_id => binary_to_list(maps:get(<<"wallet_id">>, W)),
                            public_key => binary_to_list(maps:get(<<"public_key">>, W)),
                            user_id => binary_to_list(maps:get(<<"user_id">>, W)),
                            wallet_name => case maps:get(<<"wallet_name">>, W, null) of
                                null -> undefined;
                                Name -> binary_to_list(Name)
                            end,
                            created_at => maps:get(<<"created_at">>, W)
                        }
                    end, Wallets),
                    {ok, ParsedWallets};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

export_private_key(WalletId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/export/" ++ ensure_string(WalletId),

    case httpc:request(post, {ApiUrl, [], "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"private_key">> := PrivateKey} ->
                    {ok, binary_to_list(PrivateKey)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

delete_wallet(WalletId, UserId) ->
    RequestBody = #{user_id => ensure_binary(UserId)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/" ++ ensure_string(WalletId),

    case httpc:request(delete, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            deleted;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).

ensure_string(Value) when is_list(Value) -> Value;
ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_string(Value) when is_atom(Value) -> atom_to_list(Value);
ensure_string(Value) -> lists:flatten(io_lib:format("~p", [Value])).
