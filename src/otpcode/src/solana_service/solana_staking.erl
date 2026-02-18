-module(solana_staking).
-author("Zaryn Technologies").

-export([
    create_stake/4,
    deactivate_stake/3,
    withdraw_stake/5,
    get_stake_account/2,
    get_validators/1,
    get_rewards/2,
    get_rewards/3,
    get_user_stakes/1,
    get_wallet_stakes/2,
    print_validators/1,
    print_user_stakes/1,
    print_stake_account/2,
    print_rewards/2
]).

-define(SOLANA_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 60000).

create_stake(Token, WalletId, AmountLamports, ValidatorVoteAddress) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        amount_lamports => AmountLamports,
        validator_vote_address => ensure_binary(ValidatorVoteAddress)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/staking/create",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            parse_stake_create_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, wallet_not_found};
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

deactivate_stake(Token, WalletId, StakeAccountAddress) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        stake_account_address => ensure_binary(StakeAccountAddress)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/staking/deactivate",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_deactivate_response(ResponseBody);
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

withdraw_stake(Token, WalletId, StakeAccountAddress, RecipientAddress, AmountLamports) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        stake_account_address => ensure_binary(StakeAccountAddress),
        recipient_address => ensure_binary(RecipientAddress),
        amount_lamports => AmountLamports
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/staking/withdraw",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_withdraw_response(ResponseBody);
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

get_stake_account(Token, StakeAccountAddress) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/staking/account/" ++ ensure_string(StakeAccountAddress),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_stake_account_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
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

get_validators(Token) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/staking/validators",
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_validators_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
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

get_rewards(Token, StakeAccountAddress) ->
    get_rewards(Token, StakeAccountAddress, 5).

get_rewards(Token, StakeAccountAddress, Epochs) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/staking/rewards/" ++ ensure_string(StakeAccountAddress)
             ++ "?epochs=" ++ integer_to_list(Epochs),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_rewards_response(ResponseBody);
        {ok, {{_, 400, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
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

get_user_stakes(Token) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/staking/user",
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_user_stakes_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
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

get_wallet_stakes(Token, WalletId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/staking/wallet/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_wallet_stakes_response(ResponseBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 404, _}, _, _}} ->
            {error, wallet_not_found};
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

print_validators(Token) ->
    case get_validators(Token) of
        {ok, Result} ->
            Validators = maps:get(validators, Result),
            Total = maps:get(total, Result),
            Active = maps:get(active, Result),
            Delinquent = maps:get(delinquent, Result),
            io:format("~n=== VALIDATORS ===~n"),
            io:format("Total: ~p | Active: ~p | Delinquent: ~p~n~n", [Total, Active, Delinquent]),
            lists:foreach(fun(V) ->
                Status = case maps:get(epoch_vote_account, V) of
                    true -> "ACTIVE";
                    false -> "DELINQUENT"
                end,
                io:format("--- ~s ---~n", [Status]),
                io:format("  Vote:       ~s~n", [maps:get(vote_address, V)]),
                io:format("  Node:       ~s~n", [maps:get(node_address, V)]),
                io:format("  Commission: ~p%~n", [maps:get(commission, V)]),
                io:format("  Stake:      ~p lamports~n", [maps:get(activated_stake, V)]),
                io:format("  Last Vote:  ~p~n~n", [maps:get(last_vote, V)])
            end, lists:sublist(Validators, 5)),
            {ok, Total};
        {error, Reason} ->
            io:format("❌ Error getting validators: ~p~n", [Reason]),
            {error, Reason}
    end.

print_user_stakes(Token) ->
    case get_user_stakes(Token) of
        {ok, Result} ->
            Stakes = maps:get(stakes, Result),
            Total = maps:get(total, Result),
            Active = maps:get(active, Result),
            Deactivating = maps:get(deactivating, Result),
            Withdrawn = maps:get(withdrawn, Result),
            io:format("~n=== USER STAKES ===~n"),
            io:format("Total: ~p | Active: ~p | Deactivating: ~p | Withdrawn: ~p~n~n",
                     [Total, Active, Deactivating, Withdrawn]),
            case Stakes of
                [] ->
                    io:format("No stake accounts found.~n");
                _ ->
                    lists:foreach(fun(S) ->
                        io:format("--- ~s ---~n", [maps:get(stake_id, S)]),
                        io:format("  Stake Account: ~s~n", [maps:get(stake_account_address, S)]),
                        io:format("  Validator:     ~s~n", [maps:get(validator_vote_address, S)]),
                        io:format("  Amount:        ~p lamports (~p SOL)~n",
                                 [maps:get(amount_lamports, S), maps:get(amount_sol, S)]),
                        io:format("  Status:        ~s~n", [maps:get(status, S)]),
                        io:format("  Created:       ~p~n", [maps:get(created_at, S)]),
                        case maps:get(delegated_at, S, undefined) of
                            undefined -> ok;
                            DelegatedAt -> io:format("  Delegated:     ~p~n", [DelegatedAt])
                        end,
                        case maps:get(deactivated_at, S, undefined) of
                            undefined -> ok;
                            DeactivatedAt -> io:format("  Deactivated:   ~p~n", [DeactivatedAt])
                        end,
                        case maps:get(withdrawn_at, S, undefined) of
                            undefined -> ok;
                            WithdrawnAt -> io:format("  Withdrawn:     ~p~n", [WithdrawnAt])
                        end,
                        io:format("  Signature:     ~s~n~n", [maps:get(signature, S)])
                    end, Stakes)
            end,
            {ok, Total};
        {error, Reason} ->
            io:format("❌ Error getting user stakes: ~p~n", [Reason]),
            {error, Reason}
    end.

print_stake_account(Token, StakeAccountAddress) ->
    case get_stake_account(Token, StakeAccountAddress) of
        {ok, Info} ->
            io:format("~n=== STAKE ACCOUNT INFO ===~n"),
            io:format("Address:    ~s~n", [maps:get(stake_account_address, Info)]),
            io:format("Balance:    ~p lamports (~p SOL)~n",
                     [maps:get(balance_lamports, Info), maps:get(balance_sol, Info)]),
            io:format("Status:     ~s~n", [maps:get(status, Info)]),
            io:format("Validator:  ~s~n", [maps:get(voter, Info)]),
            case maps:get(activating_epoch, Info, undefined) of
                undefined -> ok;
                ActivatingEpoch -> io:format("Activating Epoch:   ~p~n", [ActivatingEpoch])
            end,
            case maps:get(active_epoch, Info, undefined) of
                undefined -> ok;
                ActiveEpoch -> io:format("Active Epoch:       ~p~n", [ActiveEpoch])
            end,
            case maps:get(deactivating_epoch, Info, undefined) of
                undefined -> ok;
                DeactivatingEpoch -> io:format("Deactivating Epoch: ~p~n", [DeactivatingEpoch])
            end,
            case maps:get(deactivated_epoch, Info, undefined) of
                undefined -> ok;
                DeactivatedEpoch -> io:format("Deactivated Epoch:  ~p~n", [DeactivatedEpoch])
            end,
            {ok, Info};
        {error, Reason} ->
            io:format("❌ Error getting stake account info: ~p~n", [Reason]),
            {error, Reason}
    end.

print_rewards(Token, StakeAccountAddress) ->
    case get_rewards(Token, StakeAccountAddress, 5) of
        {ok, Result} ->
            Rewards = maps:get(rewards, Result),
            TotalLamports = maps:get(total_rewards_lamports, Result),
            TotalSol = maps:get(total_rewards_sol, Result),
            io:format("~n=== STAKING REWARDS ===~n"),
            io:format("Stake Account: ~s~n", [maps:get(stake_account_address, Result)]),
            io:format("Total Rewards: ~p lamports (~p SOL)~n~n", [TotalLamports, TotalSol]),
            lists:foreach(fun(R) ->
                io:format("  Epoch ~p: ~p lamports (~p SOL) | Commission: ~p%~n",
                         [maps:get(epoch, R),
                          maps:get(amount_lamports, R),
                          maps:get(amount_sol, R),
                          maps:get(commission, R)])
            end, Rewards),
            {ok, Result};
        {error, Reason} ->
            io:format("❌ Error getting rewards: ~p~n", [Reason]),
            {error, Reason}
    end.

parse_stake_create_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"stake_account_address">> := StakeAddress,
              <<"signature">> := Signature} = Response ->
                {ok, #{
                    stake_account_address => binary_to_list(StakeAddress),
                    wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                    validator_vote_address => binary_to_list(maps:get(<<"validator_vote_address">>, Response)),
                    amount_lamports => maps:get(<<"amount_lamports">>, Response),
                    amount_sol => maps:get(<<"amount_sol">>, Response),
                    signature => binary_to_list(Signature),
                    created_at => maps:get(<<"created_at">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse stake create response: ~p ~p ~p",
                        [Error, Reason, Stack]),
            {error, parse_failed}
    end.

parse_deactivate_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"stake_account_address">> := StakeAddress,
              <<"signature">> := Signature} = Response ->
                {ok, #{
                    stake_account_address => binary_to_list(StakeAddress),
                    signature => binary_to_list(Signature),
                    deactivated_at => maps:get(<<"deactivated_at">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse deactivate response: ~p ~p ~p",
                        [Error, Reason, Stack]),
            {error, parse_failed}
    end.

parse_withdraw_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"stake_account_address">> := StakeAddress,
              <<"signature">> := Signature} = Response ->
                {ok, #{
                    stake_account_address => binary_to_list(StakeAddress),
                    recipient_address => binary_to_list(maps:get(<<"recipient_address">>, Response)),
                    amount_lamports => maps:get(<<"amount_lamports">>, Response),
                    amount_sol => maps:get(<<"amount_sol">>, Response),
                    signature => binary_to_list(Signature),
                    withdrawn_at => maps:get(<<"withdrawn_at">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse withdraw response: ~p ~p ~p",
                        [Error, Reason, Stack]),
            {error, parse_failed}
    end.

parse_stake_account_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"stake_account_address">> := StakeAddress} = Response ->
                {ok, #{
                    stake_account_address => binary_to_list(StakeAddress),
                    balance_lamports => maps:get(<<"balance_lamports">>, Response),
                    balance_sol => maps:get(<<"balance_sol">>, Response),
                    status => binary_to_list(maps:get(<<"status">>, Response)),
                    voter => binary_to_list(maps:get(<<"voter">>, Response)),
                    activating_epoch => maps:get(<<"activating_epoch">>, Response, undefined),
                    active_epoch => maps:get(<<"active_epoch">>, Response, undefined),
                    deactivating_epoch => maps:get(<<"deactivating_epoch">>, Response, undefined),
                    deactivated_epoch => maps:get(<<"deactivated_epoch">>, Response, undefined)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse stake account response: ~p ~p ~p",
                        [Error, Reason, Stack]),
            {error, parse_failed}
    end.

parse_validators_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"validators">> := Validators} = Response ->
                ParsedValidators = lists:map(fun(V) ->
                    #{
                        vote_address => binary_to_list(maps:get(<<"vote_address">>, V)),
                        node_address => binary_to_list(maps:get(<<"node_address">>, V)),
                        commission => maps:get(<<"commission">>, V),
                        last_vote => maps:get(<<"last_vote">>, V),
                        root_slot => maps:get(<<"root_slot">>, V),
                        activated_stake => maps:get(<<"activated_stake">>, V),
                        epoch_vote_account => maps:get(<<"epoch_vote_account">>, V)
                    }
                end, Validators),
                {ok, #{
                    validators => ParsedValidators,
                    total => maps:get(<<"total">>, Response),
                    active => maps:get(<<"active">>, Response),
                    delinquent => maps:get(<<"delinquent">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse validators response: ~p ~p ~p",
                        [Error, Reason, Stack]),
            {error, parse_failed}
    end.

parse_rewards_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"rewards">> := Rewards,
              <<"stake_account_address">> := StakeAddress} = Response ->
                ParsedRewards = lists:map(fun(R) ->
                    #{
                        epoch => maps:get(<<"epoch">>, R),
                        amount_lamports => maps:get(<<"amount_lamports">>, R),
                        amount_sol => maps:get(<<"amount_sol">>, R),
                        post_balance => maps:get(<<"post_balance">>, R),
                        commission => maps:get(<<"commission">>, R)
                    }
                end, Rewards),
                {ok, #{
                    stake_account_address => binary_to_list(StakeAddress),
                    rewards => ParsedRewards,
                    total_rewards_lamports => maps:get(<<"total_rewards_lamports">>, Response),
                    total_rewards_sol => maps:get(<<"total_rewards_sol">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse rewards response: ~p ~p ~p",
                        [Error, Reason, Stack]),
            {error, parse_failed}
    end.

parse_user_stakes_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"stakes">> := Stakes} = Response ->
                ParsedStakes = lists:map(fun(S) -> parse_stake_record(S) end, Stakes),
                {ok, #{
                    user_id => binary_to_list(maps:get(<<"user_id">>, Response)),
                    stakes => ParsedStakes,
                    total => maps:get(<<"total">>, Response),
                    active => maps:get(<<"active">>, Response),
                    deactivating => maps:get(<<"deactivating">>, Response),
                    withdrawn => maps:get(<<"withdrawn">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse user stakes response: ~p ~p ~p",
                        [Error, Reason, Stack]),
            {error, parse_failed}
    end.

parse_wallet_stakes_response(ResponseBody) ->
    try
        case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
            #{<<"stakes">> := Stakes} = Response ->
                ParsedStakes = lists:map(fun(S) -> parse_stake_record(S) end, Stakes),
                {ok, #{
                    wallet_id => binary_to_list(maps:get(<<"wallet_id">>, Response)),
                    stakes => ParsedStakes,
                    total => maps:get(<<"total">>, Response),
                    active => maps:get(<<"active">>, Response),
                    deactivating => maps:get(<<"deactivating">>, Response),
                    withdrawn => maps:get(<<"withdrawn">>, Response)
                }};
            _ ->
                {error, invalid_response}
        end
    catch
        Error:Reason:Stack ->
            logger:error("Failed to parse wallet stakes response: ~p ~p ~p",
                        [Error, Reason, Stack]),
            {error, parse_failed}
    end.

parse_stake_record(S) ->
    #{
        stake_id => binary_to_list(maps:get(<<"stake_id">>, S)),
        stake_account_address => binary_to_list(maps:get(<<"stake_account_address">>, S)),
        validator_vote_address => binary_to_list(maps:get(<<"validator_vote_address">>, S)),
        amount_lamports => maps:get(<<"amount_lamports">>, S),
        amount_sol => maps:get(<<"amount_sol">>, S),
        status => binary_to_list(maps:get(<<"status">>, S)),
        created_at => maps:get(<<"created_at">>, S),
        delegated_at => maps:get(<<"delegated_at">>, S, undefined),
        deactivated_at => maps:get(<<"deactivated_at">>, S, undefined),
        withdrawn_at => maps:get(<<"withdrawn_at">>, S, undefined),
        signature => binary_to_list(maps:get(<<"signature">>, S))
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
