-module(aptos_wallet).
-author("Zaryn Technologies").
-export([
    create_wallet/2,
    create_wallet/3,
    import_wallet/3,
    import_wallet/4,
    get_balance/2,
    get_apt_balance/2,
    get_fa_balance/3,
    get_wallet_info/2,
    get_user_wallets/2,
    transfer_apt/5,
    transfer_apt/6,
    transfer_fa/6,
    delete_wallet/2,

    get_coin_balance/3,
    get_all_coins/2,
    get_fa_metadata/2,
    get_all_fa_balances/2,
    transfer_coin/6,
    mint_coin/5,
    call_view_function/3,
    get_fa_activities/3,
    get_fa_metadata_by_creator/2,

    add_stake/4,
    unlock_stake/4,
    withdraw_stake/3,
    reactivate_stake/4,
    get_stake_info/3,
    get_delegation_pools/1,
    get_pool_info/2,
    get_staking_activities/2,

    get_local_transaction_history/2,
    get_local_transaction_history/4
]).

-define(APTOS_API_BASE, "http://localhost:3021").
-define(DEFAULT_TIMEOUT, 30000).

create_wallet(Token, UserId) ->
    create_wallet(Token, UserId, undefined).

create_wallet(Token, UserId, Label) ->
    Body0 = #{user_id => ensure_binary(UserId)},
    Body  = case Label of
        undefined -> Body0;
        _         -> maps:put(label, ensure_binary(Label), Body0)
    end,
    post_request(Token, "/aptos/accounts/create", Body, 201).

import_wallet(Token, UserId, PrivateKeyHex) ->
    import_wallet(Token, UserId, PrivateKeyHex, undefined).

import_wallet(Token, UserId, PrivateKeyHex, Label) ->
    Body0 = #{
        user_id     => ensure_binary(UserId),
        private_key => ensure_binary(PrivateKeyHex)
    },
    Body = case Label of
        undefined -> Body0;
        _         -> maps:put(label, ensure_binary(Label), Body0)
    end,
    post_request(Token, "/aptos/accounts/import", Body, 200).

delete_wallet(Token, AccountAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/accounts/" ++ binary_to_list(ensure_binary(AccountAddress)),
    Headers = auth_headers(Token),
    case httpc:request(delete, {Url, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} -> {ok, deleted};
        {ok, {{_, 404, _}, _, _}} -> {error, not_found};
        {ok, {{_, 401, _}, _, _}} -> {error, unauthorized};
        {ok, {{_, Code, _}, _, Body}} -> {error, {http_error, Code, Body}};
        {error, Reason} -> {error, Reason}
    end.

get_balance(Token, AccountAddress) ->
    get_apt_balance(Token, AccountAddress).

get_apt_balance(Token, AccountAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/accounts/"
          ++ binary_to_list(ensure_binary(AccountAddress)) ++ "/balance",
    Headers = auth_headers(Token),
    case httpc:request(get, {Url, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Body}} -> parse_balance_response(Body);
        {ok, {{_, 404, _}, _, _}}    -> {ok, 0};
        {ok, {{_, 401, _}, _, _}}    -> {error, unauthorized};
        {ok, {{_, Code, _}, _, B}}   -> {error, {http_error, Code, B}};
        {error, Reason}              -> {error, Reason}
    end.

get_fa_balance(Token, AccountAddress, MetadataAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/accounts/"
          ++ binary_to_list(ensure_binary(AccountAddress))
          ++ "/fa-balance/" ++ binary_to_list(ensure_binary(MetadataAddress)),
    Headers = auth_headers(Token),
    case httpc:request(get, {Url, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Body}} -> parse_balance_response(Body);
        {ok, {{_, 404, _}, _, _}}    -> {ok, 0};
        {ok, {{_, 401, _}, _, _}}    -> {error, unauthorized};
        {ok, {{_, Code, _}, _, B}}   -> {error, {http_error, Code, B}};
        {error, Reason}              -> {error, Reason}
    end.

get_wallet_info(Token, AccountAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/accounts/"
          ++ binary_to_list(ensure_binary(AccountAddress)),
    Headers = auth_headers(Token),
    case httpc:request(get, {Url, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Body}} -> parse_json_response(Body);
        {ok, {{_, 404, _}, _, _}}    -> {error, account_not_found};
        {ok, {{_, 401, _}, _, _}}    -> {error, unauthorized};
        {ok, {{_, Code, _}, _, B}}   -> {error, {http_error, Code, B}};
        {error, Reason}              -> {error, Reason}
    end.

get_user_wallets(Token, UserId) ->
    Url = ?APTOS_API_BASE ++ "/aptos/accounts/user/"
          ++ binary_to_list(ensure_binary(UserId)),
    Headers = auth_headers(Token),
    case httpc:request(get, {Url, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Body}} -> parse_json_response(Body);
        {ok, {{_, 401, _}, _, _}}    -> {error, unauthorized};
        {ok, {{_, Code, _}, _, B}}   -> {error, {http_error, Code, B}};
        {error, Reason}              -> {error, Reason}
    end.

transfer_apt(Token, SenderAddress, RecipientAddress, AmountOctas, Network) ->
    transfer_apt(Token, SenderAddress, RecipientAddress, AmountOctas, Network, #{}).

transfer_apt(Token, SenderAddress, RecipientAddress, AmountOctas, Network, Options) ->
    Body = maps:merge(#{
        sender_address    => ensure_binary(SenderAddress),
        recipient_address => ensure_binary(RecipientAddress),
        amount_octas      => AmountOctas,
        network           => ensure_binary(atom_to_binary(Network, utf8))
    }, Options),
    post_request(Token, "/aptos/transactions/transfer", Body, 200).

transfer_fa(Token, SenderAddress, RecipientAddress, MetadataAddress, Amount, Network) ->
    Body = #{
        sender_address    => ensure_binary(SenderAddress),
        recipient_address => ensure_binary(RecipientAddress),
        metadata_address  => ensure_binary(MetadataAddress),
        amount            => Amount,
        network           => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/tokens/transfer-fa", Body, 200).

get_coin_balance(Token, AccountAddress, CoinType) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/"
          ++ binary_to_list(ensure_binary(AccountAddress))
          ++ "/coin-balance?coin_type=" ++ uri_encode(ensure_binary(CoinType)),
    get_request(Token, Url).

get_all_coins(Token, AccountAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/coins/"
          ++ binary_to_list(ensure_binary(AccountAddress)),
    get_request(Token, Url).

get_fa_metadata(Token, AssetType) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/fa-metadata?asset_type="
          ++ uri_encode(ensure_binary(AssetType)),
    get_request(Token, Url).

get_all_fa_balances(Token, AccountAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/fa-balances/"
          ++ binary_to_list(ensure_binary(AccountAddress)),
    get_request(Token, Url).

transfer_coin(Token, SenderAddress, RecipientAddress, CoinType, Amount, Network) ->
    Body = #{
        sender_address    => ensure_binary(SenderAddress),
        recipient_address => ensure_binary(RecipientAddress),
        coin_type         => ensure_binary(CoinType),
        amount            => Amount,
        network           => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/tokens/transfer-coin", Body, 200).

mint_coin(Token, AccountAddress, CoinType, Amount, Network) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        coin_type       => ensure_binary(CoinType),
        amount          => Amount,
        network         => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/tokens/mint-coin", Body, 200).

call_view_function(Token, FunctionId, Args) ->
    Body = #{
        function          => ensure_binary(FunctionId),
        type_arguments    => [],
        function_arguments => Args
    },
    post_request(Token, "/aptos/tokens/view", Body, 200).

get_fa_activities(Token, AssetType, Limit) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/fa-activities?asset_type="
          ++ uri_encode(ensure_binary(AssetType))
          ++ "&limit=" ++ integer_to_list(Limit),
    get_request(Token, Url).

get_fa_metadata_by_creator(Token, CreatorAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/fa-metadata/creator/"
          ++ binary_to_list(ensure_binary(CreatorAddress)),
    get_request(Token, Url).

add_stake(Token, AccountAddress, PoolAddress, AmountOctas) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        pool_address    => ensure_binary(PoolAddress),
        amount_octas    => AmountOctas
    },
    post_request(Token, "/aptos/staking/add-stake", Body, 200).

unlock_stake(Token, AccountAddress, PoolAddress, AmountOctas) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        pool_address    => ensure_binary(PoolAddress),
        amount_octas    => AmountOctas
    },
    post_request(Token, "/aptos/staking/unlock", Body, 200).

withdraw_stake(Token, AccountAddress, PoolAddress) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        pool_address    => ensure_binary(PoolAddress)
    },
    post_request(Token, "/aptos/staking/withdraw", Body, 200).

reactivate_stake(Token, AccountAddress, PoolAddress, AmountOctas) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        pool_address    => ensure_binary(PoolAddress),
        amount_octas    => AmountOctas
    },
    post_request(Token, "/aptos/staking/reactivate", Body, 200).

get_stake_info(Token, DelegatorAddress, PoolAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/staking/stake-info?delegator_address="
          ++ binary_to_list(ensure_binary(DelegatorAddress))
          ++ "&pool_address=" ++ binary_to_list(ensure_binary(PoolAddress)),
    get_request(Token, Url).

get_delegation_pools(Token) ->
    Url = ?APTOS_API_BASE ++ "/aptos/staking/pools",
    get_request(Token, Url).

get_pool_info(Token, PoolAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/staking/pools/"
          ++ binary_to_list(ensure_binary(PoolAddress)),
    get_request(Token, Url).

get_staking_activities(Token, DelegatorAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/staking/activities/"
          ++ binary_to_list(ensure_binary(DelegatorAddress)),
    get_request(Token, Url).

get_local_transaction_history(Token, WalletId) ->
    get_local_transaction_history(Token, WalletId, 25, 0).

get_local_transaction_history(Token, WalletId, Limit, Offset) ->
    Url = ?APTOS_API_BASE ++ "/aptos/transactions/history/"
          ++ binary_to_list(ensure_binary(WalletId))
          ++ "?limit=" ++ integer_to_list(Limit)
          ++ "&offset=" ++ integer_to_list(Offset),
    get_request(Token, Url).

post_request(Token, Path, Body, ExpectedCode) ->
    Json = jsx:encode(Body),
    Url  = ?APTOS_API_BASE ++ Path,
    Headers = auth_headers(Token),
    case httpc:request(post, {Url, Headers, "application/json", Json},
                       [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, C, _}, _, RespBody}} when C =:= ExpectedCode ->
            parse_json_response(RespBody);
        {ok, {{_, 200, _}, _, RespBody}} ->
            parse_json_response(RespBody);
        {ok, {{_, 201, _}, _, RespBody}} ->
            parse_json_response(RespBody);
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, 400, _}, _, ErrBody}} ->
            parse_error_response(ErrBody);
        {ok, {{_, 409, _}, _, _}} ->
            {error, address_already_exists};
        {ok, {{_, Code, _}, _, ErrBody}} ->
            {error, {http_error, Code, ErrBody}};
        {error, Reason} ->
            {error, Reason}
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
        Decoded = jsx:decode(list_to_binary(Body), [return_maps]),
        {ok, Decoded}
    catch
        _:_ -> {error, {invalid_json, Body}}
    end.

parse_balance_response(Body) ->
    case parse_json_response(Body) of
        {ok, #{<<"balance">> := Bal}} -> {ok, Bal};
        {ok, #{<<"amount">> := Amt}}  -> {ok, Amt};
        {ok, Map} -> {ok, Map};
        Error     -> Error
    end.

parse_error_response(Body) ->
    case parse_json_response(Body) of
        {ok, #{<<"error">> := Msg}}   -> {error, Msg};
        {ok, #{<<"message">> := Msg}} -> {error, Msg};
        _                             -> {error, {unknown_error, Body}}
    end.

ensure_binary(V) when is_binary(V)  -> V;
ensure_binary(V) when is_list(V)    -> list_to_binary(V);
ensure_binary(V) when is_atom(V)    -> atom_to_binary(V, utf8);
ensure_binary(V) when is_integer(V) -> integer_to_binary(V);
ensure_binary(V)                    -> term_to_binary(V).

uri_encode(Bin) ->
    http_uri:encode(binary_to_list(Bin)).
