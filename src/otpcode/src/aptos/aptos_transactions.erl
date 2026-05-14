-module(aptos_transactions).
-author("Zaryn Technologies").
-export([
    transfer_apt/5,
    transfer_apt/6,
    get_transaction_by_hash/3,
    get_account_transactions/3,
    get_account_transactions/4,
    get_transactions/2,
    get_transaction_by_version/3,
    estimate_gas/4,
    get_ledger_info/2,
    get_block_by_height/3,
    get_block_by_version/3,
    get_local_history/2,
    get_local_history/4,
    get_local_transaction/2,
    get_local_user_history/2,
    get_local_history_by_type/3
]).

-define(APTOS_API_BASE, "http://localhost:3021").
-define(DEFAULT_TIMEOUT, 30000).

transfer_apt(Token, SenderAddress, RecipientAddress, AmountOctas, Network) ->
    transfer_apt(Token, SenderAddress, RecipientAddress, AmountOctas, Network, #{}).

transfer_apt(Token, SenderAddress, RecipientAddress, AmountOctas, Network, Options) ->
    Body = maps:merge(#{
        sender_address    => ensure_binary(SenderAddress),
        recipient_address => ensure_binary(RecipientAddress),
        amount_octas      => AmountOctas,
        network           => ensure_binary(atom_to_binary(Network, utf8))
    }, Options),
    Json    = jsx:encode(Body),
    Url     = ?APTOS_API_BASE ++ "/aptos/transactions/transfer",
    Headers = auth_headers(Token),
    case httpc:request(post, {Url, Headers, "application/json", Json},
                       [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, C, _}, _, RespBody}} when C >= 200, C < 300 ->
            parse_json_response(RespBody);
        {ok, {{_, 401, _}, _, _}} -> {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} -> {error, forbidden};
        {ok, {{_, 400, _}, _, B}} -> parse_error_response(B);
        {ok, {{_, Code, _}, _, B}} -> {error, {http_error, Code, B}};
        {error, Reason} -> {error, Reason}
    end.

get_transaction_by_hash(Token, TxHash, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/transactions/"
          ++ binary_to_list(ensure_binary(TxHash))
          ++ "?network=" ++ network_param(Network),
    get_request(Token, Url).

get_account_transactions(Token, AccountAddress, Network) ->
    get_account_transactions(Token, AccountAddress, Network, #{}).

get_account_transactions(Token, AccountAddress, Network, Options) ->
    Limit  = maps:get(limit, Options, 25),
    Offset = maps:get(offset, Options, 0),
    Url = ?APTOS_API_BASE ++ "/aptos/accounts/"
          ++ binary_to_list(ensure_binary(AccountAddress))
          ++ "/transactions?network=" ++ network_param(Network)
          ++ "&limit=" ++ integer_to_list(Limit)
          ++ "&start=" ++ integer_to_list(Offset),
    get_request(Token, Url).

get_transactions(Token, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/transactions?network=" ++ network_param(Network),
    get_request(Token, Url).

get_transaction_by_version(Token, Version, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/transactions/by-version/"
          ++ integer_to_list(Version)
          ++ "?network=" ++ network_param(Network),
    get_request(Token, Url).

estimate_gas(Token, SenderAddress, Function, Network) ->
    Body = #{
        sender_address => ensure_binary(SenderAddress),
        function       => ensure_binary(Function),
        network        => ensure_binary(atom_to_binary(Network, utf8))
    },
    Json    = jsx:encode(Body),
    Url     = ?APTOS_API_BASE ++ "/aptos/transactions/estimate-gas",
    Headers = auth_headers(Token),
    case httpc:request(post, {Url, Headers, "application/json", Json},
                       [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, C, _}, _, RespBody}} when C >= 200, C < 300 ->
            parse_json_response(RespBody);
        {ok, {{_, 401, _}, _, _}} -> {error, unauthorized};
        {ok, {{_, Code, _}, _, B}} -> {error, {http_error, Code, B}};
        {error, Reason} -> {error, Reason}
    end.

get_ledger_info(Token, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/ledger?network=" ++ network_param(Network),
    get_request(Token, Url).

get_block_by_height(Token, BlockHeight, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/blocks/by-height/"
          ++ integer_to_list(BlockHeight)
          ++ "?network=" ++ network_param(Network),
    get_request(Token, Url).

get_block_by_version(Token, Version, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/blocks/by-version/"
          ++ integer_to_list(Version)
          ++ "?network=" ++ network_param(Network),
    get_request(Token, Url).

get_local_history(Token, WalletId) ->
    get_local_history(Token, WalletId, 25, 0).

get_local_history(Token, WalletId, Limit, Offset) ->
    Url = ?APTOS_API_BASE ++ "/aptos/transactions/history/"
          ++ binary_to_list(ensure_binary(WalletId))
          ++ "?limit=" ++ integer_to_list(Limit)
          ++ "&offset=" ++ integer_to_list(Offset),
    case get_request(Token, Url) of
        {ok, Response} ->
            Txs = maps:get(<<"transactions">>, Response, []),
            Parsed = lists:map(fun parse_tx_record/1, Txs),
            {ok, #{
                wallet_id    => maps:get(<<"wallet_id">>, Response, ensure_binary(WalletId)),
                transactions => Parsed,
                total        => maps:get(<<"total">>, Response, length(Parsed)),
                limit        => Limit,
                offset       => Offset
            }};
        Error ->
            Error
    end.

get_local_transaction(Token, TxHash) ->
    case aptos_wallet_server:get_transaction_by_hash(ensure_binary(TxHash)) of
        {ok, Tx} ->
            {ok, format_tx_record(Tx)};
        Error ->
            Error
    end.

get_local_user_history(Token, UserId) ->
    case aptos_wallet_server:get_user_transactions(UserId) of
        {ok, Txs} ->
            Parsed = lists:map(fun format_tx_record/1, Txs),
            {ok, #{
                user_id      => ensure_binary(UserId),
                transactions => Parsed,
                total        => length(Parsed)
            }};
        Error ->
            Error
    end.

get_local_history_by_type(Token, WalletId, TxType) ->
    case aptos_wallet_server:get_transactions_by_type(WalletId, TxType) of
        {ok, Txs} ->
            Parsed = lists:map(fun format_tx_record/1, Txs),
            {ok, #{
                wallet_id    => ensure_binary(WalletId),
                tx_type      => ensure_binary(atom_to_binary(TxType, utf8)),
                transactions => Parsed,
                total        => length(Parsed)
            }};
        Error ->
            Error
    end.

parse_tx_record(Tx) ->
    #{
        tx_id            => maps:get(<<"tx_id">>, Tx, undefined),
        wallet_id        => maps:get(<<"wallet_id">>, Tx, undefined),
        transaction_hash => maps:get(<<"transaction_hash">>, Tx, undefined),
        tx_type          => maps:get(<<"tx_type">>, Tx, undefined),
        sender_address   => maps:get(<<"sender_address">>, Tx, undefined),
        receiver_address => maps:get(<<"receiver_address">>, Tx, undefined),
        amount_octas     => maps:get(<<"amount_octas">>, Tx, 0),
        coin_type        => maps:get(<<"coin_type">>, Tx, undefined),
        metadata_address => maps:get(<<"metadata_address">>, Tx, undefined),
        pool_address     => maps:get(<<"pool_address">>, Tx, undefined),
        collection_name  => maps:get(<<"collection_name">>, Tx, undefined),
        token_address    => maps:get(<<"token_address">>, Tx, undefined),
        gas_used         => maps:get(<<"gas_used">>, Tx, 0),
        gas_unit_price   => maps:get(<<"gas_unit_price">>, Tx, 0),
        version          => maps:get(<<"version">>, Tx, undefined),
        vm_status        => maps:get(<<"vm_status">>, Tx, undefined),
        status           => maps:get(<<"status">>, Tx, undefined),
        created_at       => maps:get(<<"created_at">>, Tx, undefined)
    }.

format_tx_record(Tx) ->
    #{
        tx_id            => element(2, Tx),
        wallet_id        => element(3, Tx),
        user_id          => element(4, Tx),
        transaction_hash => element(5, Tx),
        tx_type          => element(6, Tx),
        sender_address   => element(7, Tx),
        receiver_address => element(8, Tx),
        amount_octas     => element(9, Tx),
        gas_used         => element(10, Tx),
        gas_unit_price   => element(11, Tx),
        sequence_number  => element(12, Tx),
        function_called  => element(13, Tx),
        coin_type        => element(14, Tx),
        metadata_address => element(15, Tx),
        pool_address     => element(16, Tx),
        collection_name  => element(17, Tx),
        token_address    => element(18, Tx),
        status           => element(19, Tx),
        vm_status        => element(20, Tx),
        version          => element(21, Tx),
        error_message    => element(22, Tx),
        created_at       => element(23, Tx)
    }.

get_request(Token, Url) ->
    Headers = auth_headers(Token),
    case httpc:request(get, {Url, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, C, _}, _, Body}} when C >= 200, C < 300 ->
            parse_json_response(Body);
        {ok, {{_, 401, _}, _, _}} -> {error, unauthorized};
        {ok, {{_, 404, _}, _, _}} -> {error, not_found};
        {ok, {{_, Code, _}, _, B}} -> {error, {http_error, Code, B}};
        {error, Reason} -> {error, Reason}
    end.

auth_headers(Token) ->
    [{"Authorization", "Bearer " ++ binary_to_list(ensure_binary(Token))},
     {"Content-Type", "application/json"}].

parse_json_response(Body) ->
    try
        {ok, jsx:decode(list_to_binary(Body), [return_maps])}
    catch
        _:_ -> {error, {invalid_json, Body}}
    end.

parse_error_response(Body) ->
    case parse_json_response(Body) of
        {ok, #{<<"error">> := Msg}}   -> {error, Msg};
        {ok, #{<<"message">> := Msg}} -> {error, Msg};
        _                             -> {error, {unknown_error, Body}}
    end.

network_param(mainnet) -> "mainnet";
network_param(testnet) -> "testnet";
network_param(devnet)  -> "devnet";
network_param(local)   -> "local";
network_param(N) when is_binary(N) -> binary_to_list(N);
network_param(N) when is_list(N)   -> N;
network_param(_)       -> "testnet".

ensure_binary(V) when is_binary(V)  -> V;
ensure_binary(V) when is_list(V)    -> list_to_binary(V);
ensure_binary(V) when is_atom(V)    -> atom_to_binary(V, utf8);
ensure_binary(V) when is_integer(V) -> integer_to_binary(V);
ensure_binary(V)                    -> term_to_binary(V).
