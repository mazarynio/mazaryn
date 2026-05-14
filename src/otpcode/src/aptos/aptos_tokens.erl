-module(aptos_tokens).
-author("Zaryn Technologies").
-export([
    create_collection/5,
    create_collection/6,
    mint_nft/7,
    mint_nft/8,
    mint_soul_bound/8,
    transfer_nft/5,
    burn_nft/4,
    get_owned_tokens/3,
    get_collection_data/4,
    get_token_data/3,
    set_token_description/5,
    set_token_name/5,
    set_token_uri/5,
    add_token_property/7,
    update_token_property/7,
    remove_token_property/7,

    get_coin_balance/4,
    get_all_coins/3,
    get_fa_metadata/3,
    get_all_fa_balances/3,
    transfer_coin/7,
    transfer_fa/7,
    mint_coin/6,
    call_view_function/4,
    get_fa_activities/4,
    get_fa_metadata_by_creator/3,

    sync_coin_balance/4,
    sync_fa_balances/3,
    get_local_coin_balance/3,
    get_local_fa_balance/3,
    get_local_all_coin_balances/2,
    get_local_all_fa_balances/2
]).

-define(APTOS_API_BASE, "http://localhost:3021").
-define(DEFAULT_TIMEOUT, 30000).

create_collection(Token, AccountAddress, Name, Description, Uri) ->
    create_collection(Token, AccountAddress, Name, Description, Uri, #{}).

create_collection(Token, AccountAddress, Name, Description, Uri, Options) ->
    Body = maps:merge(#{
        account_address => ensure_binary(AccountAddress),
        name            => ensure_binary(Name),
        description     => ensure_binary(Description),
        uri             => ensure_binary(Uri)
    }, Options),
    post_request(Token, "/aptos/nft/collections/create", Body).

mint_nft(Token, AccountAddress, Collection, Name, Description, Uri, Properties) ->
    mint_nft(Token, AccountAddress, Collection, Name, Description, Uri, Properties, #{}).

mint_nft(Token, AccountAddress, Collection, Name, Description, Uri, Properties, Options) ->
    Body = maps:merge(#{
        account_address  => ensure_binary(AccountAddress),
        collection       => ensure_binary(Collection),
        name             => ensure_binary(Name),
        description      => ensure_binary(Description),
        uri              => ensure_binary(Uri),
        property_keys    => maps:get(keys, Properties, []),
        property_types   => maps:get(types, Properties, []),
        property_values  => maps:get(values, Properties, [])
    }, Options),
    post_request(Token, "/aptos/nft/tokens/mint", Body).

mint_soul_bound(Token, AccountAddress, Collection, Name, Description, Uri, RecipientAddress, Properties) ->
    Body = #{
        account_address   => ensure_binary(AccountAddress),
        collection        => ensure_binary(Collection),
        name              => ensure_binary(Name),
        description       => ensure_binary(Description),
        uri               => ensure_binary(Uri),
        recipient_address => ensure_binary(RecipientAddress),
        property_keys     => maps:get(keys, Properties, []),
        property_types    => maps:get(types, Properties, []),
        property_values   => maps:get(values, Properties, [])
    },
    post_request(Token, "/aptos/nft/tokens/mint-soul-bound", Body).

transfer_nft(Token, AccountAddress, TokenAddress, RecipientAddress, Network) ->
    Body = #{
        account_address   => ensure_binary(AccountAddress),
        token_address     => ensure_binary(TokenAddress),
        recipient_address => ensure_binary(RecipientAddress),
        network           => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/nft/tokens/transfer", Body).

burn_nft(Token, AccountAddress, TokenAddress, Network) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        token_address   => ensure_binary(TokenAddress),
        network         => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/nft/tokens/burn", Body).

set_token_description(Token, AccountAddress, TokenAddress, Description, Network) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        token_address   => ensure_binary(TokenAddress),
        description     => ensure_binary(Description),
        network         => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/nft/tokens/set-description", Body).

set_token_name(Token, AccountAddress, TokenAddress, Name, Network) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        token_address   => ensure_binary(TokenAddress),
        name            => ensure_binary(Name),
        network         => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/nft/tokens/set-name", Body).

set_token_uri(Token, AccountAddress, TokenAddress, Uri, Network) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        token_address   => ensure_binary(TokenAddress),
        uri             => ensure_binary(Uri),
        network         => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/nft/tokens/set-uri", Body).

add_token_property(Token, AccountAddress, TokenAddress, Key, Type, Value, Network) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        token_address   => ensure_binary(TokenAddress),
        property_key    => ensure_binary(Key),
        property_type   => ensure_binary(Type),
        property_value  => Value,
        network         => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/nft/tokens/add-property", Body).

update_token_property(Token, AccountAddress, TokenAddress, Key, Type, Value, Network) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        token_address   => ensure_binary(TokenAddress),
        property_key    => ensure_binary(Key),
        property_type   => ensure_binary(Type),
        property_value  => Value,
        network         => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/nft/tokens/update-property", Body).

remove_token_property(Token, AccountAddress, TokenAddress, Key, Type, Value, Network) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        token_address   => ensure_binary(TokenAddress),
        property_key    => ensure_binary(Key),
        property_type   => ensure_binary(Type),
        property_value  => Value,
        network         => ensure_binary(atom_to_binary(Network, utf8))
    },
    post_request(Token, "/aptos/nft/tokens/remove-property", Body).

get_owned_tokens(Token, AccountAddress, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/nft/tokens/owned/"
          ++ binary_to_list(ensure_binary(AccountAddress))
          ++ "?network=" ++ network_param(Network),
    get_request(Token, Url).

get_collection_data(Token, CollectionName, CreatorAddress, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/nft/collections/"
          ++ uri_encode(ensure_binary(CollectionName))
          ++ "?creator=" ++ binary_to_list(ensure_binary(CreatorAddress))
          ++ "&network=" ++ network_param(Network),
    get_request(Token, Url).

get_token_data(Token, TokenAddress, Network) ->
    Url = ?APTOS_API_BASE ++ "/aptos/nft/tokens/"
          ++ binary_to_list(ensure_binary(TokenAddress))
          ++ "?network=" ++ network_param(Network),
    get_request(Token, Url).

get_coin_balance(Token, WalletId, AccountAddress, CoinType) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/"
          ++ binary_to_list(ensure_binary(AccountAddress))
          ++ "/coin-balance?coin_type=" ++ uri_encode(ensure_binary(CoinType)),
    case get_request(Token, Url) of
        {ok, Response} ->
            Balance = maps:get(<<"balance">>, Response, 0),
            maybe_sync_coin_balance(WalletId, CoinType, Balance, Response),
            {ok, parse_coin_balance_response(Response)};
        Error ->
            Error
    end.

get_all_coins(Token, WalletId, AccountAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/coins/"
          ++ binary_to_list(ensure_binary(AccountAddress)),
    case get_request(Token, Url) of
        {ok, Response} ->
            Coins = maps:get(<<"coins">>, Response, []),
            lists:foreach(fun(Coin) ->
                CoinType = maps:get(<<"coin_type">>, Coin, undefined),
                Balance  = maps:get(<<"balance">>, Coin, 0),
                case CoinType of
                    undefined -> ok;
                    _ ->
                        maybe_sync_coin_balance(WalletId, CoinType, Balance, Coin)
                end
            end, Coins),
            {ok, parse_coins_response(Response)};
        Error ->
            Error
    end.

get_fa_metadata(Token, _WalletId, AssetType) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/fa-metadata?asset_type="
          ++ uri_encode(ensure_binary(AssetType)),
    case get_request(Token, Url) of
        {ok, Response} ->
            {ok, parse_fa_metadata_response(Response)};
        Error ->
            Error
    end.

get_all_fa_balances(Token, WalletId, AccountAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/fa-balances/"
          ++ binary_to_list(ensure_binary(AccountAddress)),
    case get_request(Token, Url) of
        {ok, Response} ->
            Balances = maps:get(<<"balances">>, Response, []),
            lists:foreach(fun(Bal) ->
                AssetType       = maps:get(<<"asset_type">>, Bal, undefined),
                MetadataAddress = maps:get(<<"metadata_address">>, Bal, undefined),
                Amount          = maps:get(<<"amount">>, Bal, 0),
                IsPrimary       = maps:get(<<"is_primary">>, Bal, false),
                case AssetType of
                    undefined -> ok;
                    _ ->
                        BalData = #{
                            asset_type       => AssetType,
                            metadata_address => MetadataAddress,
                            amount           => Amount,
                            is_primary       => IsPrimary
                        },
                        aptos_wallet_server:upsert_fa_balance(WalletId, BalData)
                end
            end, Balances),
            {ok, parse_fa_balances_response(Response)};
        Error ->
            Error
    end.

transfer_coin(Token, WalletId, SenderAddress, RecipientAddress, CoinType, Amount, Network) ->
    Body = #{
        sender_address    => ensure_binary(SenderAddress),
        recipient_address => ensure_binary(RecipientAddress),
        coin_type         => ensure_binary(CoinType),
        amount            => Amount,
        network           => ensure_binary(atom_to_binary(Network, utf8))
    },
    case post_request(Token, "/aptos/tokens/transfer-coin", Body) of
        {ok, Response} ->
            maybe_record_token_tx(WalletId, coin_transfer, Response, #{
                coin_type        => ensure_binary(CoinType),
                receiver_address => ensure_binary(RecipientAddress),
                amount_octas     => Amount
            }),
            {ok, Response};
        Error ->
            Error
    end.

transfer_fa(Token, WalletId, SenderAddress, RecipientAddress, MetadataAddress, Amount, Network) ->
    Body = #{
        sender_address    => ensure_binary(SenderAddress),
        recipient_address => ensure_binary(RecipientAddress),
        metadata_address  => ensure_binary(MetadataAddress),
        amount            => Amount,
        network           => ensure_binary(atom_to_binary(Network, utf8))
    },
    case post_request(Token, "/aptos/tokens/transfer-fa", Body) of
        {ok, Response} ->
            maybe_record_token_tx(WalletId, fa_transfer, Response, #{
                metadata_address => ensure_binary(MetadataAddress),
                receiver_address => ensure_binary(RecipientAddress),
                amount_octas     => Amount
            }),
            {ok, Response};
        Error ->
            Error
    end.

mint_coin(Token, WalletId, AccountAddress, CoinType, Amount, Network) ->
    Body = #{
        account_address => ensure_binary(AccountAddress),
        coin_type       => ensure_binary(CoinType),
        amount          => Amount,
        network         => ensure_binary(atom_to_binary(Network, utf8))
    },
    case post_request(Token, "/aptos/tokens/mint-coin", Body) of
        {ok, Response} ->
            maybe_record_token_tx(WalletId, coin_mint, Response, #{
                coin_type    => ensure_binary(CoinType),
                amount_octas => Amount
            }),
            {ok, Response};
        Error ->
            Error
    end.

call_view_function(Token, _WalletId, FunctionId, Args) ->
    Body = #{
        function           => ensure_binary(FunctionId),
        type_arguments     => [],
        function_arguments => Args
    },
    case post_request(Token, "/aptos/tokens/view", Body) of
        {ok, Response} ->
            {ok, parse_view_response(Response)};
        Error ->
            Error
    end.

get_fa_activities(Token, _WalletId, AssetType, Limit) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/fa-activities?asset_type="
          ++ uri_encode(ensure_binary(AssetType))
          ++ "&limit=" ++ integer_to_list(Limit),
    case get_request(Token, Url) of
        {ok, Response} ->
            {ok, parse_fa_activities_response(Response)};
        Error ->
            Error
    end.

get_fa_metadata_by_creator(Token, _WalletId, CreatorAddress) ->
    Url = ?APTOS_API_BASE ++ "/aptos/tokens/fa-metadata/creator/"
          ++ binary_to_list(ensure_binary(CreatorAddress)),
    case get_request(Token, Url) of
        {ok, Response} ->
            {ok, parse_fa_metadata_list_response(Response)};
        Error ->
            Error
    end.

sync_coin_balance(Token, WalletId, AccountAddress, CoinType) ->
    case get_coin_balance(Token, WalletId, AccountAddress, CoinType) of
        {ok, Result} -> {ok, Result};
        Error        -> Error
    end.

sync_fa_balances(Token, WalletId, AccountAddress) ->
    case get_all_fa_balances(Token, WalletId, AccountAddress) of
        {ok, Result} -> {ok, Result};
        Error        -> Error
    end.

get_local_coin_balance(WalletId, _AccountAddress, CoinType) ->
    aptos_wallet_server:get_coin_balance(WalletId, ensure_binary(CoinType)).

get_local_fa_balance(WalletId, _AccountAddress, AssetType) ->
    aptos_wallet_server:get_fa_balance(WalletId, ensure_binary(AssetType)).

get_local_all_coin_balances(WalletId, _AccountAddress) ->
    aptos_wallet_server:get_wallet_coin_balances(WalletId).

get_local_all_fa_balances(WalletId, _AccountAddress) ->
    aptos_wallet_server:get_wallet_fa_balances(WalletId).

maybe_sync_coin_balance(WalletId, CoinType, Balance, _Response) ->
    BalData = #{
        coin_type => ensure_binary(CoinType),
        balance   => Balance
    },
    case aptos_wallet_server:upsert_coin_balance(WalletId, BalData) of
        {ok, _} -> ok;
        _       -> ok
    end.

maybe_record_token_tx(WalletId, TxType, Response, Extra) ->
    TxHash   = maps:get(<<"transaction_hash">>, Response, undefined),
    GasUsed  = maps:get(<<"gas_used">>, Response, 0),
    GasPrice = maps:get(<<"gas_unit_price">>, Response, 0),
    VmStatus = maps:get(<<"vm_status">>, Response, undefined),
    Version  = maps:get(<<"version">>, Response, undefined),
    TxData   = maps:merge(#{
        tx_type          => TxType,
        transaction_hash => TxHash,
        gas_used         => GasUsed,
        gas_unit_price   => GasPrice,
        vm_status        => VmStatus,
        version          => Version,
        status           => confirmed
    }, Extra),
    case aptos_wallet_server:create_transaction(WalletId, TxData) of
        {ok, _TxId} -> ok;
        _           -> ok
    end.

parse_coin_balance_response(Response) ->
    #{
        coin_type => maps:get(<<"coin_type">>, Response, undefined),
        balance   => maps:get(<<"balance">>, Response, 0),
        name      => maps:get(<<"name">>, Response, undefined),
        symbol    => maps:get(<<"symbol">>, Response, undefined),
        decimals  => maps:get(<<"decimals">>, Response, 8)
    }.

parse_coins_response(Response) ->
    Coins = maps:get(<<"coins">>, Response, []),
    ParsedCoins = lists:map(fun(Coin) ->
        #{
            coin_type => maps:get(<<"coin_type">>, Coin, undefined),
            balance   => maps:get(<<"balance">>, Coin, 0),
            name      => maps:get(<<"name">>, Coin, undefined),
            symbol    => maps:get(<<"symbol">>, Coin, undefined),
            decimals  => maps:get(<<"decimals">>, Coin, 8),
            logo_url  => maps:get(<<"logo_url">>, Coin, undefined)
        }
    end, Coins),
    #{
        account_address => maps:get(<<"account_address">>, Response, undefined),
        coins           => ParsedCoins,
        total           => maps:get(<<"total">>, Response, length(ParsedCoins))
    }.

parse_fa_metadata_response(Response) ->
    #{
        asset_type       => maps:get(<<"asset_type">>, Response, undefined),
        name             => maps:get(<<"name">>, Response, undefined),
        symbol           => maps:get(<<"symbol">>, Response, undefined),
        decimals         => maps:get(<<"decimals">>, Response, 8),
        metadata_address => maps:get(<<"metadata_address">>, Response, undefined),
        icon_uri         => maps:get(<<"icon_uri">>, Response, undefined),
        project_uri      => maps:get(<<"project_uri">>, Response, undefined),
        creator_address  => maps:get(<<"creator_address">>, Response, undefined),
        supply           => maps:get(<<"supply">>, Response, undefined)
    }.

parse_fa_balances_response(Response) ->
    Balances = maps:get(<<"balances">>, Response, []),
    ParsedBalances = lists:map(fun(Bal) ->
        #{
            asset_type       => maps:get(<<"asset_type">>, Bal, undefined),
            metadata_address => maps:get(<<"metadata_address">>, Bal, undefined),
            amount           => maps:get(<<"amount">>, Bal, 0),
            name             => maps:get(<<"name">>, Bal, undefined),
            symbol           => maps:get(<<"symbol">>, Bal, undefined),
            decimals         => maps:get(<<"decimals">>, Bal, 8),
            is_primary       => maps:get(<<"is_primary">>, Bal, false),
            icon_uri         => maps:get(<<"icon_uri">>, Bal, undefined)
        }
    end, Balances),
    #{
        account_address => maps:get(<<"account_address">>, Response, undefined),
        balances        => ParsedBalances,
        total           => maps:get(<<"total">>, Response, length(ParsedBalances))
    }.

parse_view_response(Response) ->
    #{
        result => maps:get(<<"result">>, Response, [])
    }.

parse_fa_activities_response(Response) ->
    Activities = maps:get(<<"activities">>, Response, []),
    ParsedActivities = lists:map(fun(Activity) ->
        #{
            event_type       => maps:get(<<"type">>, Activity, undefined),
            asset_type       => maps:get(<<"asset_type">>, Activity, undefined),
            from_address     => maps:get(<<"from_address">>, Activity, undefined),
            to_address       => maps:get(<<"to_address">>, Activity, undefined),
            amount           => maps:get(<<"amount">>, Activity, 0),
            transaction_hash => maps:get(<<"transaction_version">>, Activity, undefined),
            is_gas_fee       => maps:get(<<"is_gas_fee">>, Activity, false),
            is_transaction   => maps:get(<<"is_transaction_fee">>, Activity, false),
            entry_function   => maps:get(<<"entry_function_id_str">>, Activity, undefined),
            created_at       => maps:get(<<"last_transaction_version">>, Activity, undefined)
        }
    end, Activities),
    #{
        asset_type  => maps:get(<<"asset_type">>, Response, undefined),
        activities  => ParsedActivities,
        total       => maps:get(<<"total">>, Response, length(ParsedActivities))
    }.

parse_fa_metadata_list_response(Response) ->
    Assets = maps:get(<<"assets">>, Response, []),
    ParsedAssets = lists:map(fun(Asset) ->
        #{
            asset_type       => maps:get(<<"asset_type">>, Asset, undefined),
            name             => maps:get(<<"name">>, Asset, undefined),
            symbol           => maps:get(<<"symbol">>, Asset, undefined),
            decimals         => maps:get(<<"decimals">>, Asset, 8),
            metadata_address => maps:get(<<"metadata_address">>, Asset, undefined),
            icon_uri         => maps:get(<<"icon_uri">>, Asset, undefined),
            project_uri      => maps:get(<<"project_uri">>, Asset, undefined),
            supply           => maps:get(<<"supply">>, Asset, undefined)
        }
    end, Assets),
    #{
        creator_address => maps:get(<<"creator_address">>, Response, undefined),
        assets          => ParsedAssets,
        total           => maps:get(<<"total">>, Response, length(ParsedAssets))
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

uri_encode(Bin) ->
    http_uri:encode(binary_to_list(Bin)).
