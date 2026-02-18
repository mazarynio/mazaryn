-module(solana_transactions).
-author("Zaryn Technologies").

-export([
    get_history/2,
    get_history/3,
    get_history/4,
    get_history_by_wallet/2,
    get_history_by_wallet/3,
    get_stats/2,
    get_stats_by_wallet/2,
    filter_transactions/3,
    filter_transactions/4,
    get_transaction_types/1,
    print_history/2,
    print_stats/2
]).

-define(SOLANA_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 60000).

get_history(Token, PublicKey) ->
    get_history(Token, PublicKey, 20, []).

get_history(Token, PublicKey, Limit) ->
    get_history(Token, PublicKey, Limit, []).

get_history(Token, PublicKey, Limit, Options) ->
    RequestBody0 = #{
        public_key => ensure_binary(PublicKey),
        limit => Limit
    },

    RequestBody1 = case lists:keyfind(before, 1, Options) of
        {before, Before} ->
            maps:put(before, ensure_binary(Before), RequestBody0);
        false ->
            RequestBody0
    end,

    RequestBody = case lists:keyfind(until, 1, Options) of
        {until, Until} ->
            maps:put(until, ensure_binary(Until), RequestBody1);
        false ->
            RequestBody1
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/transactions/history",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_history_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

get_history_by_wallet(Token, WalletId) ->
    get_history_by_wallet(Token, WalletId, 20).

get_history_by_wallet(Token, WalletId, Limit) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        limit => Limit
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/transactions/history",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_history_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

get_stats(Token, PublicKey) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/transactions/stats/" ++ ensure_string(PublicKey),
    Headers = auth_headers(Token),

    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_stats_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

get_stats_by_wallet(Token, WalletId) ->
    RequestBody = #{wallet_id => ensure_binary(WalletId)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/transactions/stats",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_stats_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

filter_transactions(Token, PublicKey, Filters) ->
    filter_transactions(Token, PublicKey, Filters, 20).

filter_transactions(Token, PublicKey, Filters, Limit) ->
    RequestBody0 = #{
        public_key => ensure_binary(PublicKey),
        limit => Limit
    },

    RequestBody1 = case lists:keyfind(type, 1, Filters) of
        {type, Type} ->
            maps:put(type, ensure_binary(Type), RequestBody0);
        false ->
            RequestBody0
    end,

    RequestBody2 = case lists:keyfind(status, 1, Filters) of
        {status, Status} ->
            maps:put(status, ensure_binary(Status), RequestBody1);
        false ->
            RequestBody1
    end,

    RequestBody3 = case lists:keyfind(before, 1, Filters) of
        {before, Before} ->
            maps:put(before, ensure_binary(Before), RequestBody2);
        false ->
            RequestBody2
    end,

    RequestBody = case lists:keyfind(until, 1, Filters) of
        {until, Until} ->
            maps:put(until, ensure_binary(Until), RequestBody3);
        false ->
            RequestBody3
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/transactions/filter",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_history_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

get_transaction_types(Token) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/transactions/types",
    Headers = auth_headers(Token),

    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"types">> := Types} ->
                    ParsedTypes = lists:map(fun(T) ->
                        #{
                            type => binary_to_list(maps:get(<<"type">>, T)),
                            description => binary_to_list(maps:get(<<"description">>, T))
                        }
                    end, Types),
                    {ok, ParsedTypes};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limited};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

print_history(Token, PublicKey) ->
    case get_history(Token, PublicKey, 20) of
        {ok, Result} ->
            Txs = maps:get(transactions, Result),
            Total = maps:get(total, Result),
            HasMore = maps:get(has_more, Result),
            io:format("~n=== TRANSACTION HISTORY ===~n"),
            io:format("Public Key: ~s~n", [maps:get(public_key, Result)]),
            io:format("Total: ~p | Has More: ~p~n~n", [Total, HasMore]),
            lists:foreach(fun(Tx) ->
                io:format("--- ~s ---~n", [maps:get(type, Tx)]),
                io:format("  Sig:    ~s~n", [maps:get(signature, Tx)]),
                io:format("  Status: ~s~n", [maps:get(status, Tx)]),
                io:format("  Fee:    ~p lamports~n", [maps:get(fee, Tx)]),
                case maps:get(amount, Tx, undefined) of
                    undefined -> ok;
                    Amount -> io:format("  Amount: ~p lamports~n", [Amount])
                end,
                case maps:get(from, Tx, undefined) of
                    undefined -> ok;
                    From -> io:format("  From:   ~s~n", [From])
                end,
                case maps:get(to, Tx, undefined) of
                    undefined -> ok;
                    To -> io:format("  To:     ~s~n", [To])
                end,
                case maps:get(token_mint, Tx, undefined) of
                    undefined -> ok;
                    Mint -> io:format("  Token:  ~s~n", [Mint])
                end,
                case maps:get(error, Tx, undefined) of
                    undefined -> ok;
                    Err -> io:format("  Error:  ~s~n", [Err])
                end,
                io:format("~n")
            end, Txs),
            {ok, Total};
        {error, Reason} ->
            io:format("❌ Error getting history: ~p~n", [Reason]),
            {error, Reason}
    end.

print_stats(Token, PublicKey) ->
    case get_stats(Token, PublicKey) of
        {ok, Stats} ->
            io:format("~n=== TRANSACTION STATS ===~n"),
            io:format("Public Key:       ~s~n", [maps:get(public_key, Stats)]),
            io:format("Total Txs:        ~p~n", [maps:get(total_transactions, Stats)]),
            io:format("Total Sent:       ~p SOL~n", [maps:get(total_sent_sol, Stats)]),
            io:format("Total Received:   ~p SOL~n", [maps:get(total_received_sol, Stats)]),
            io:format("Total Fees:       ~p SOL~n", [maps:get(total_fees_paid, Stats)]),
            case maps:get(first_transaction, Stats, undefined) of
                undefined -> ok;
                First -> io:format("First Tx:         ~s~n", [First])
            end,
            case maps:get(last_transaction, Stats, undefined) of
                undefined -> ok;
                Last -> io:format("Last Tx:          ~s~n", [Last])
            end,
            {ok, Stats};
        {error, Reason} ->
            io:format("❌ Error getting stats: ~p~n", [Reason]),
            {error, Reason}
    end.

parse_history_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"transactions">> := Transactions} = Response ->
                ParsedTransactions = lists:map(fun(Tx) ->
                    #{
                        signature => binary_to_list(maps:get(<<"signature">>, Tx)),
                        block_time => maps:get(<<"block_time">>, Tx, null),
                        slot => maps:get(<<"slot">>, Tx),
                        status => binary_to_list(maps:get(<<"status">>, Tx)),
                        fee => maps:get(<<"fee">>, Tx),
                        type => binary_to_list(maps:get(<<"type">>, Tx)),
                        amount => maps:get(<<"amount">>, Tx, undefined),
                        from => case maps:get(<<"from">>, Tx, null) of
                            null -> undefined;
                            From -> binary_to_list(From)
                        end,
                        to => case maps:get(<<"to">>, Tx, null) of
                            null -> undefined;
                            To -> binary_to_list(To)
                        end,
                        token_mint => case maps:get(<<"token_mint">>, Tx, null) of
                            null -> undefined;
                            Mint -> binary_to_list(Mint)
                        end,
                        token_amount => maps:get(<<"token_amount">>, Tx, undefined),
                        token_decimals => maps:get(<<"token_decimals">>, Tx, undefined),
                        memo => case maps:get(<<"memo">>, Tx, null) of
                            null -> undefined;
                            Memo -> binary_to_list(Memo)
                        end,
                        error => case maps:get(<<"error">>, Tx, null) of
                            null -> undefined;
                            Err -> binary_to_list(Err)
                        end
                    }
                end, Transactions),
                {ok, #{
                    public_key => binary_to_list(maps:get(<<"public_key">>, Response)),
                    transactions => ParsedTransactions,
                    total => maps:get(<<"total">>, Response),
                    has_more => maps:get(<<"has_more">>, Response),
                    oldest_signature => case maps:get(<<"oldest_signature">>, Response, null) of
                        null -> undefined;
                        Sig -> binary_to_list(Sig)
                    end
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse history response: ~p ~p ~p", [Error, Reason, Stack]),
            {error, parse_failed}
    end.

parse_stats_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"total_transactions">> := Total} = Response ->
                {ok, #{
                    public_key => binary_to_list(maps:get(<<"public_key">>, Response)),
                    total_transactions => Total,
                    total_sent_sol => maps:get(<<"total_sent_sol">>, Response),
                    total_received_sol => maps:get(<<"total_received_sol">>, Response),
                    total_fees_paid => maps:get(<<"total_fees_paid">>, Response),
                    first_transaction => case maps:get(<<"first_transaction">>, Response, null) of
                        null -> undefined;
                        First -> binary_to_list(First)
                    end,
                    last_transaction => case maps:get(<<"last_transaction">>, Response, null) of
                        null -> undefined;
                        Last -> binary_to_list(Last)
                    end
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse stats response: ~p ~p ~p", [Error, Reason, Stack]),
            {error, parse_failed}
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
        _:_ ->
            {error, invalid_error_response}
    end.

auth_headers(Token) ->
    [{"Authorization", "Bearer " ++ ensure_string(Token)}].

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).

ensure_string(Value) when is_list(Value) -> Value;
ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_string(Value) when is_atom(Value) -> atom_to_list(Value);
ensure_string(Value) -> lists:flatten(io_lib:format("~p", [Value])).
