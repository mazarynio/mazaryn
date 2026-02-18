-module(solana_airdrop).
-author("Zaryn Technologies").

-export([
    create_sol_airdrop/4,
    create_sol_airdrop/5,
    create_token_airdrop/5,
    create_token_airdrop/6,
    create_nft_airdrop/3,
    create_nft_airdrop/4,
    validate_airdrop/4,
    get_airdrop/2,
    get_user_airdrops/2,
    get_wallet_airdrops/2
]).

-define(SOLANA_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 120000).

create_sol_airdrop(Token, WalletId, Recipients, AmountLamports) ->
    create_sol_airdrop(Token, WalletId, Recipients, AmountLamports, undefined).

create_sol_airdrop(Token, WalletId, Recipients, AmountLamports, Memo) ->
    ParsedRecipients = lists:map(fun(R) ->
        case R of
            #{address := Addr} ->
                #{address => ensure_binary(Addr)};
            Addr when is_list(Addr) ->
                #{address => ensure_binary(Addr)};
            Addr when is_binary(Addr) ->
                #{address => Addr}
        end
    end, Recipients),

    RequestBody = case Memo of
        undefined ->
            #{
                wallet_id => ensure_binary(WalletId),
                type => <<"sol">>,
                recipients => ParsedRecipients,
                amount_per_recipient => AmountLamports
            };
        _ ->
            #{
                wallet_id => ensure_binary(WalletId),
                type => <<"sol">>,
                recipients => ParsedRecipients,
                amount_per_recipient => AmountLamports,
                memo => ensure_binary(Memo)
            }
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/airdrop/create",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_airdrop_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

create_token_airdrop(Token, WalletId, Recipients, TokenMint, AmountPerRecipient) ->
    create_token_airdrop(Token, WalletId, Recipients, TokenMint, AmountPerRecipient, undefined).

create_token_airdrop(Token, WalletId, Recipients, TokenMint, AmountPerRecipient, Memo) ->
    ParsedRecipients = lists:map(fun(R) ->
        case R of
            #{address := Addr} ->
                #{address => ensure_binary(Addr)};
            Addr when is_list(Addr) ->
                #{address => ensure_binary(Addr)};
            Addr when is_binary(Addr) ->
                #{address => Addr}
        end
    end, Recipients),

    RequestBody = case Memo of
        undefined ->
            #{
                wallet_id => ensure_binary(WalletId),
                type => <<"token">>,
                recipients => ParsedRecipients,
                token_mint => ensure_binary(TokenMint),
                amount_per_recipient => AmountPerRecipient
            };
        _ ->
            #{
                wallet_id => ensure_binary(WalletId),
                type => <<"token">>,
                recipients => ParsedRecipients,
                token_mint => ensure_binary(TokenMint),
                amount_per_recipient => AmountPerRecipient,
                memo => ensure_binary(Memo)
            }
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/airdrop/create",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_airdrop_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

create_nft_airdrop(Token, WalletId, Recipients) ->
    create_nft_airdrop(Token, WalletId, Recipients, undefined).

create_nft_airdrop(Token, WalletId, Recipients, Memo) ->
    ParsedRecipients = lists:map(fun(R) ->
        case R of
            #{address := Addr, mint_address := Mint} ->
                #{
                    address => ensure_binary(Addr),
                    mint_address => ensure_binary(Mint)
                };
            #{address := Addr} ->
                #{address => ensure_binary(Addr)}
        end
    end, Recipients),

    RequestBody = case Memo of
        undefined ->
            #{
                wallet_id => ensure_binary(WalletId),
                type => <<"nft">>,
                recipients => ParsedRecipients
            };
        _ ->
            #{
                wallet_id => ensure_binary(WalletId),
                type => <<"nft">>,
                recipients => ParsedRecipients,
                memo => ensure_binary(Memo)
            }
    end,

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/airdrop/create",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_airdrop_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 500, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

validate_airdrop(Token, WalletId, Type, Recipients) ->
    ParsedRecipients = lists:map(fun(R) ->
        case R of
            #{address := Addr, mint_address := Mint} ->
                #{
                    address => ensure_binary(Addr),
                    mint_address => ensure_binary(Mint)
                };
            #{address := Addr} ->
                #{address => ensure_binary(Addr)};
            Addr when is_list(Addr) ->
                #{address => ensure_binary(Addr)};
            Addr when is_binary(Addr) ->
                #{address => Addr}
        end
    end, Recipients),

    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        type => ensure_binary(Type),
        recipients => ParsedRecipients
    },

    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/airdrop/validate",
    Headers = auth_headers(Token),

    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"valid">> := Valid} = Response ->
                    {ok, #{
                        valid => Valid,
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                        type => binary_to_list(maps:get(<<"type">>, Response)),
                        total_recipients => maps:get(<<"total_recipients">>, Response),
                        estimated_cost_lamports => maps:get(<<"estimated_cost_lamports">>, Response),
                        estimated_cost_sol => maps:get(<<"estimated_cost_sol">>, Response),
                        current_balance_lamports => maps:get(<<"current_balance_lamports">>, Response),
                        current_balance_sol => maps:get(<<"current_balance_sol">>, Response),
                        warnings => lists:map(fun binary_to_list/1,
                            maps:get(<<"warnings">>, Response, []))
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_airdrop(Token, AirdropId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/airdrop/" ++ ensure_string(AirdropId),
    Headers = auth_headers(Token),

    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"airdrop_id">> := AirdropIdBin} = Response ->
                    {ok, #{
                        airdrop_id => binary_to_list(AirdropIdBin),
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                        type => binary_to_list(maps:get(<<"type">>, Response)),
                        status => binary_to_list(maps:get(<<"status">>, Response)),
                        total_recipients => maps:get(<<"total_recipients">>, Response),
                        successful => maps:get(<<"successful">>, Response),
                        failed => maps:get(<<"failed">>, Response),
                        created_at => maps:get(<<"created_at">>, Response),
                        completed_at => maps:get(<<"completed_at">>, Response, undefined),
                        token_mint => case maps:get(<<"token_mint">>, Response, null) of
                            null -> undefined;
                            Mint -> binary_to_list(Mint)
                        end,
                        recipients => parse_airdrop_recipients(
                            maps:get(<<"recipients">>, Response, []))
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

get_user_airdrops(Token, UserId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/airdrop/user/" ++ ensure_string(UserId),
    Headers = auth_headers(Token),

    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"airdrops">> := Airdrops} = Response ->
                    ParsedAirdrops = lists:map(fun(A) ->
                        #{
                            airdrop_id => binary_to_list(maps:get(<<"airdrop_id">>, A)),
                            wallet_id => binary_to_list(maps:get(<<"wallet_id">>, A)),
                            type => binary_to_list(maps:get(<<"type">>, A)),
                            status => binary_to_list(maps:get(<<"status">>, A)),
                            total_recipients => maps:get(<<"total_recipients">>, A),
                            successful => maps:get(<<"successful">>, A),
                            failed => maps:get(<<"failed">>, A),
                            created_at => maps:get(<<"created_at">>, A),
                            completed_at => maps:get(<<"completed_at">>, A, undefined)
                        }
                    end, Airdrops),
                    {ok, #{
                        user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                        total => maps:get(<<"total">>, Response),
                        airdrops => ParsedAirdrops
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

get_wallet_airdrops(Token, WalletId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/airdrop/wallet/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),

    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"airdrops">> := Airdrops} = Response ->
                    ParsedAirdrops = lists:map(fun(A) ->
                        #{
                            airdrop_id => binary_to_list(maps:get(<<"airdrop_id">>, A)),
                            wallet_id => binary_to_list(maps:get(<<"wallet_id">>, A)),
                            type => binary_to_list(maps:get(<<"type">>, A)),
                            status => binary_to_list(maps:get(<<"status">>, A)),
                            total_recipients => maps:get(<<"total_recipients">>, A),
                            successful => maps:get(<<"successful">>, A),
                            failed => maps:get(<<"failed">>, A),
                            created_at => maps:get(<<"created_at">>, A),
                            completed_at => maps:get(<<"completed_at">>, A, undefined)
                        }
                    end, Airdrops),
                    {ok, #{
                        wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                        total => maps:get(<<"total">>, Response),
                        airdrops => ParsedAirdrops
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

parse_airdrop_response(ResponseBody) ->
    case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
        #{<<"airdrop_id">> := AirdropId} = Response ->
            {ok, #{
                airdrop_id => binary_to_list(AirdropId),
                wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                type => binary_to_list(maps:get(<<"type">>, Response)),
                total_recipients => maps:get(<<"total_recipients">>, Response),
                successful => maps:get(<<"successful">>, Response),
                failed => maps:get(<<"failed">>, Response),
                started_at => maps:get(<<"started_at">>, Response),
                completed_at => maps:get(<<"completed_at">>, Response),
                total_amount => maps:get(<<"total_amount">>, Response, undefined),
                results => parse_airdrop_results(maps:get(<<"results">>, Response, []))
            }};
        _ ->
            {error, invalid_response}
    end.

parse_airdrop_results(Results) ->
    lists:map(fun(R) ->
        #{
            recipient => binary_to_list(maps:get(<<"recipient">>, R)),
            success => maps:get(<<"success">>, R),
            signature => case maps:get(<<"signature">>, R, null) of
                null -> undefined;
                Sig -> binary_to_list(Sig)
            end,
            error => case maps:get(<<"error">>, R, null) of
                null -> undefined;
                Err -> binary_to_list(Err)
            end
        }
    end, Results).

parse_airdrop_recipients(Recipients) ->
    lists:map(fun(R) ->
        #{
            recipient_address => binary_to_list(maps:get(<<"recipient_address">>, R)),
            amount => maps:get(<<"amount">>, R, undefined),
            mint_address => case maps:get(<<"mint_address">>, R, null) of
                null -> undefined;
                Mint -> binary_to_list(Mint)
            end,
            success => maps:get(<<"success">>, R),
            signature => case maps:get(<<"signature">>, R, null) of
                null -> undefined;
                Sig -> binary_to_list(Sig)
            end,
            error => case maps:get(<<"error">>, R, null) of
                null -> undefined;
                Err -> binary_to_list(Err)
            end,
            processed_at => maps:get(<<"processed_at">>, R)
        }
    end, Recipients).

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
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).

ensure_string(Value) when is_list(Value) -> Value;
ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_string(Value) when is_atom(Value) -> atom_to_list(Value);
ensure_string(Value) -> lists:flatten(io_lib:format("~p", [Value])).
