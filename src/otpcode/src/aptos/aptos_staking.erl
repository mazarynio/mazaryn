-module(aptos_staking).
-author("Zaryn Technologies").
-export([
    add_stake/5,
    add_stake/6,
    unlock_stake/5,
    unlock_stake/6,
    withdraw_stake/4,
    withdraw_stake/5,
    reactivate_stake/5,
    reactivate_stake/6,
    get_stake_info/4,
    get_delegation_pools/2,
    get_pool_info/3,
    get_staking_activities/3,
    get_user_stakes/2,
    get_wallet_stakes/2
]).

-define(APTOS_API_BASE, "http://localhost:3021").
-define(DEFAULT_TIMEOUT, 60000).

add_stake(Token, WalletId, AccountAddress, PoolAddress, AmountOctas) ->
    add_stake(Token, WalletId, AccountAddress, PoolAddress, AmountOctas, #{}).

add_stake(Token, WalletId, AccountAddress, PoolAddress, AmountOctas, Options) ->
    Body = maps:merge(#{
        account_address => ensure_binary(AccountAddress),
        pool_address    => ensure_binary(PoolAddress),
        amount_octas    => AmountOctas
    }, Options),
    case post_request(Token, "/aptos/staking/add-stake", Body) of
        {ok, Response} ->
            maybe_record_stake(WalletId, PoolAddress, AmountOctas, stake_add, Response),
            {ok, Response};
        Error ->
            Error
    end.

unlock_stake(Token, WalletId, AccountAddress, PoolAddress, AmountOctas) ->
    unlock_stake(Token, WalletId, AccountAddress, PoolAddress, AmountOctas, #{}).

unlock_stake(Token, WalletId, AccountAddress, PoolAddress, AmountOctas, Options) ->
    Body = maps:merge(#{
        account_address => ensure_binary(AccountAddress),
        pool_address    => ensure_binary(PoolAddress),
        amount_octas    => AmountOctas
    }, Options),
    case post_request(Token, "/aptos/staking/unlock", Body) of
        {ok, Response} ->
            maybe_record_stake(WalletId, PoolAddress, AmountOctas, stake_unlock, Response),
            {ok, Response};
        Error ->
            Error
    end.

withdraw_stake(Token, WalletId, AccountAddress, PoolAddress) ->
    withdraw_stake(Token, WalletId, AccountAddress, PoolAddress, #{}).

withdraw_stake(Token, WalletId, AccountAddress, PoolAddress, Options) ->
    Body = maps:merge(#{
        account_address => ensure_binary(AccountAddress),
        pool_address    => ensure_binary(PoolAddress)
    }, Options),
    case post_request(Token, "/aptos/staking/withdraw", Body) of
        {ok, Response} ->
            maybe_record_stake(WalletId, PoolAddress, 0, stake_withdraw, Response),
            {ok, Response};
        Error ->
            Error
    end.

reactivate_stake(Token, WalletId, AccountAddress, PoolAddress, AmountOctas) ->
    reactivate_stake(Token, WalletId, AccountAddress, PoolAddress, AmountOctas, #{}).

reactivate_stake(Token, WalletId, AccountAddress, PoolAddress, AmountOctas, Options) ->
    Body = maps:merge(#{
        account_address => ensure_binary(AccountAddress),
        pool_address    => ensure_binary(PoolAddress),
        amount_octas    => AmountOctas
    }, Options),
    case post_request(Token, "/aptos/staking/reactivate", Body) of
        {ok, Response} ->
            maybe_record_stake(WalletId, PoolAddress, AmountOctas, stake_reactivate, Response),
            {ok, Response};
        Error ->
            Error
    end.

get_stake_info(Token, WalletId, DelegatorAddress, PoolAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/staking/stake-info?delegator_address="
          ++ binary_to_list(ensure_binary(DelegatorAddress))
          ++ "&pool_address=" ++ binary_to_list(ensure_binary(PoolAddress)),
    case get_request(Token, Url) of
        {ok, Response} ->
            maybe_update_stake_balances(WalletId, PoolAddress, Response),
            {ok, parse_stake_info_response(Response)};
        Error ->
            Error
    end.

get_delegation_pools(Token, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/staking/pools?network=" ++ network_param(Network),
    case get_request(Token, Url) of
        {ok, Response} ->
            {ok, parse_pools_response(Response)};
        Error ->
            Error
    end.

get_pool_info(Token, PoolAddress, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/staking/pools/"
          ++ binary_to_list(ensure_binary(PoolAddress))
          ++ "?network=" ++ network_param(Network),
    case get_request(Token, Url) of
        {ok, Response} ->
            {ok, parse_pool_info_response(Response)};
        Error ->
            Error
    end.

get_staking_activities(Token, DelegatorAddress, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/staking/activities/"
          ++ binary_to_list(ensure_binary(DelegatorAddress))
          ++ "?network=" ++ network_param(Network),
    case get_request(Token, Url) of
        {ok, Response} ->
            {ok, parse_activities_response(Response)};
        Error ->
            Error
    end.

get_user_stakes(Token, UserId) ->
    case aptos_wallet_server:get_user_stakes(UserId) of
        {ok, Stakes} ->
            Parsed = lists:map(fun format_stake_record/1, Stakes),
            {ok, #{
                user_id => ensure_binary(UserId),
                stakes  => Parsed,
                total   => length(Parsed)
            }};
        Error ->
            Error
    end.

get_wallet_stakes(Token, WalletId) ->
    case aptos_wallet_server:get_wallet_stakes(WalletId) of
        {ok, Stakes} ->
            Parsed = lists:map(fun format_stake_record/1, Stakes),
            {ok, #{
                wallet_id => ensure_binary(WalletId),
                stakes    => Parsed,
                total     => length(Parsed)
            }};
        Error ->
            Error
    end.

maybe_record_stake(WalletId, PoolAddress, AmountOctas, TxType, Response) ->
    TxHash   = maps:get(<<"transaction_hash">>, Response, undefined),
    GasUsed  = maps:get(<<"gas_used">>, Response, 0),
    GasPrice = maps:get(<<"gas_unit_price">>, Response, 0),
    VmStatus = maps:get(<<"vm_status">>, Response, undefined),
    Version  = maps:get(<<"version">>, Response, undefined),
    Active   = maps:get(<<"active">>, Response, undefined),
    Inactive = maps:get(<<"inactive">>, Response, undefined),
    Pending  = maps:get(<<"pending_inactive">>, Response, undefined),
    StakeData = #{
        pool_address           => ensure_binary(PoolAddress),
        amount_octas           => AmountOctas,
        tx_type                => TxType,
        transaction_hash       => TxHash,
        gas_used               => GasUsed,
        gas_unit_price         => GasPrice,
        vm_status              => VmStatus,
        version                => Version,
        active_stake           => Active,
        inactive_stake         => Inactive,
        pending_inactive_stake => Pending,
        status                 => confirmed
    },
    case aptos_wallet_server:create_stake(WalletId, StakeData) of
        {ok, _StakeId} -> ok;
        _              -> ok
    end.

maybe_update_stake_balances(WalletId, PoolAddress, Response) ->
    Active   = maps:get(<<"active">>, Response, undefined),
    Inactive = maps:get(<<"inactive">>, Response, undefined),
    Pending  = maps:get(<<"pending_inactive">>, Response, undefined),
    case aptos_wallet_server:get_stakes_by_pool(WalletId, ensure_binary(PoolAddress)) of
        {ok, [Latest | _]} ->
            StakeId = Latest,
            Updates = #{
                active_stake           => Active,
                inactive_stake         => Inactive,
                pending_inactive_stake => Pending
            },
            aptos_wallet_server:update_stake(StakeId, Updates);
        _ ->
            ok
    end.

parse_stake_info_response(Response) ->
    #{
        delegator_address      => maps:get(<<"delegator_address">>, Response, undefined),
        pool_address           => maps:get(<<"pool_address">>, Response, undefined),
        active                 => maps:get(<<"active">>, Response, <<"0">>),
        inactive               => maps:get(<<"inactive">>, Response, <<"0">>),
        pending_inactive       => maps:get(<<"pending_inactive">>, Response, <<"0">>),
        total_stake            => maps:get(<<"total_stake">>, Response, <<"0">>),
        network                => maps:get(<<"network">>, Response, <<"testnet">>)
    }.

parse_pools_response(Response) ->
    Pools = maps:get(<<"pools">>, Response, []),
    ParsedPools = lists:map(fun parse_pool_record/1, Pools),
    #{
        pools   => ParsedPools,
        total   => maps:get(<<"total">>, Response, length(ParsedPools)),
        network => maps:get(<<"network">>, Response, <<"testnet">>)
    }.

parse_pool_info_response(Response) ->
    #{
        pool_address   => maps:get(<<"pool_address">>, Response, undefined),
        operator       => maps:get(<<"operator">>, Response, undefined),
        voter          => maps:get(<<"voter">>, Response, undefined),
        total_stake    => maps:get(<<"total_stake">>, Response, <<"0">>),
        voting_power   => maps:get(<<"voting_power">>, Response, undefined),
        commission_pct => maps:get(<<"commission_percentage">>, Response, undefined),
        is_active      => maps:get(<<"is_active">>, Response, false),
        network        => maps:get(<<"network">>, Response, <<"testnet">>)
    }.

parse_activities_response(Response) ->
    Activities = maps:get(<<"activities">>, Response, []),
    ParsedActivities = lists:map(fun parse_activity_record/1, Activities),
    #{
        delegator_address => maps:get(<<"delegator_address">>, Response, undefined),
        activities        => ParsedActivities,
        total             => maps:get(<<"total">>, Response, length(ParsedActivities))
    }.

parse_pool_record(Pool) ->
    #{
        pool_address   => maps:get(<<"pool_address">>, Pool, undefined),
        operator       => maps:get(<<"operator">>, Pool, undefined),
        total_stake    => maps:get(<<"total_stake">>, Pool, <<"0">>),
        commission_pct => maps:get(<<"commission_percentage">>, Pool, undefined),
        is_active      => maps:get(<<"is_active">>, Pool, false)
    }.

parse_activity_record(Activity) ->
    #{
        event_type        => maps:get(<<"event_type">>, Activity, undefined),
        delegator_address => maps:get(<<"delegator_address">>, Activity, undefined),
        pool_address      => maps:get(<<"pool_address">>, Activity, undefined),
        amount_octas      => maps:get(<<"amount">>, Activity, <<"0">>),
        transaction_hash  => maps:get(<<"transaction_version">>, Activity, undefined),
        created_at        => maps:get(<<"creation_number">>, Activity, undefined)
    }.

format_stake_record(Stake) ->
    #{
        stake_id               => element(2, Stake),
        wallet_id              => element(3, Stake),
        user_id                => element(4, Stake),
        delegator_address      => element(5, Stake),
        pool_address           => element(6, Stake),
        amount_octas           => element(7, Stake),
        active_stake           => element(8, Stake),
        inactive_stake         => element(9, Stake),
        pending_inactive_stake => element(10, Stake),
        tx_type                => element(11, Stake),
        transaction_hash       => element(12, Stake),
        gas_used               => element(13, Stake),
        gas_unit_price         => element(14, Stake),
        vm_status              => element(15, Stake),
        version                => element(16, Stake),
        network                => element(17, Stake),
        status                 => element(18, Stake),
        created_at             => element(19, Stake)
    }.

post_request(Token, Path, Body) ->
    Json    = jsx:encode(Body),
    Url     = ?APTOS_API_BASE ++ Path,
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

get_request(Token, Url) ->
    Headers = auth_headers(Token),
    case httpc:request(get, {Url, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, C, _}, _, Body}} when C >= 200, C < 300 ->
            parse_json_response(Body);
        {ok, {{_, 401, _}, _, _}} -> {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} -> {error, forbidden};
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
