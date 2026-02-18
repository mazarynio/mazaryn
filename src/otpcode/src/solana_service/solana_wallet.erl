-module(solana_wallet).
-author("Zaryn Technologies").

-export([
    create_wallet/2,
    create_wallet/3,
    import_wallet/3,
    import_wallet/4,
    get_balance/1,
    transfer/4,
    transfer/5,
    get_transaction/1,
    get_wallet_info/2,
    get_user_wallets/2,
    export_private_key/2,
    delete_wallet/2,
    get_token_accounts/1,
    get_token_balance/2,
    transfer_token/5,
    transfer_token/6,
    create_token_account/3,
    get_nfts/1,
    get_nft_metadata/1,
    transfer_nft/4
]).

-define(SOLANA_API_BASE, "http://localhost:3020").
-define(DEFAULT_TIMEOUT, 30000).

create_wallet(Token, UserId) ->
    create_wallet(Token, UserId, undefined).

create_wallet(Token, UserId, WalletName) ->
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
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
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
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

import_wallet(Token, UserId, PrivateKey) ->
    import_wallet(Token, UserId, PrivateKey, undefined).

import_wallet(Token, UserId, PrivateKey, WalletName) ->
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
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
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
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
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

transfer(Token, FromWalletId, ToPublicKey, AmountLamports) ->
    transfer(Token, FromWalletId, ToPublicKey, AmountLamports, undefined).

transfer(Token, FromWalletId, ToPublicKey, AmountLamports, Memo) ->
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
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
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

get_wallet_info(Token, WalletId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/info/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
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

get_user_wallets(Token, UserId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/user/" ++ ensure_string(UserId),
    Headers = auth_headers(Token),
    case httpc:request(get, {ApiUrl, Headers}, [{timeout, ?DEFAULT_TIMEOUT}], []) of
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
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 403, _}, _, _}} ->
            {error, forbidden};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

export_private_key(Token, WalletId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/export/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"private_key">> := PrivateKey} ->
                    {ok, binary_to_list(PrivateKey)};
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

delete_wallet(Token, WalletId) ->
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/" ++ ensure_string(WalletId),
    Headers = auth_headers(Token),
    case httpc:request(delete, {ApiUrl, Headers, "application/json", "{}"},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            deleted;
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

get_token_accounts(PublicKey) ->
    RequestBody = #{public_key => ensure_binary(PublicKey)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/token/accounts",
    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"token_accounts">> := TokenAccounts} = Response ->
                    ParsedAccounts = lists:map(fun(Account) ->
                        #{
                            address => binary_to_list(maps:get(<<"address">>, Account)),
                            mint => binary_to_list(maps:get(<<"mint">>, Account)),
                            owner => binary_to_list(maps:get(<<"owner">>, Account)),
                            amount => binary_to_list(maps:get(<<"amount">>, Account)),
                            decimals => maps:get(<<"decimals">>, Account),
                            ui_amount => maps:get(<<"ui_amount">>, Account)
                        }
                    end, TokenAccounts),
                    {ok, #{
                        public_key => binary_to_list(maps:get(<<"public_key">>, Response)),
                        token_accounts => ParsedAccounts
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_token_balance(PublicKey, TokenMint) ->
    RequestBody = #{
        public_key => ensure_binary(PublicKey),
        token_mint => ensure_binary(TokenMint)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/token/balance",
    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"balance">> := Balance} = Response ->
                    {ok, #{
                        public_key => binary_to_list(maps:get(<<"public_key">>, Response)),
                        token_mint => binary_to_list(maps:get(<<"token_mint">>, Response)),
                        balance => binary_to_list(Balance),
                        decimals => maps:get(<<"decimals">>, Response),
                        ui_amount => maps:get(<<"ui_amount">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

transfer_token(Token, FromWalletId, ToPublicKey, TokenMint, Amount) ->
    transfer_token(Token, FromWalletId, ToPublicKey, TokenMint, Amount, undefined).

transfer_token(Token, FromWalletId, ToPublicKey, TokenMint, Amount, Memo) ->
    RequestBody = case Memo of
        undefined ->
            #{
                from_wallet_id => ensure_binary(FromWalletId),
                to_public_key => ensure_binary(ToPublicKey),
                token_mint => ensure_binary(TokenMint),
                amount => Amount
            };
        _ ->
            #{
                from_wallet_id => ensure_binary(FromWalletId),
                to_public_key => ensure_binary(ToPublicKey),
                token_mint => ensure_binary(TokenMint),
                amount => Amount,
                memo => ensure_binary(Memo)
            }
    end,
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/token/transfer",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"signature">> := Signature} = Response ->
                    {ok, #{
                        signature => binary_to_list(Signature),
                        from_public_key => binary_to_list(maps:get(<<"from_public_key">>, Response)),
                        to_public_key => binary_to_list(maps:get(<<"to_public_key">>, Response)),
                        token_mint => binary_to_list(maps:get(<<"token_mint">>, Response)),
                        amount => maps:get(<<"amount">>, Response),
                        status => binary_to_list(maps:get(<<"status">>, Response)),
                        timestamp => maps:get(<<"timestamp">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
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

create_token_account(Token, WalletId, TokenMint) ->
    RequestBody = #{
        wallet_id => ensure_binary(WalletId),
        token_mint => ensure_binary(TokenMint)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/token/create-account",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"token_account">> := TokenAccount} = Response ->
                    {ok, #{
                        token_account => binary_to_list(TokenAccount),
                        owner => binary_to_list(maps:get(<<"owner">>, Response)),
                        mint => binary_to_list(maps:get(<<"mint">>, Response)),
                        signature => binary_to_list(maps:get(<<"signature">>, Response))
                    }};
                _ ->
                    {error, invalid_response}
            end;
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

get_nfts(PublicKey) ->
    RequestBody = #{public_key => ensure_binary(PublicKey)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/nft/list",
    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"nfts">> := NFTs} = Response ->
                    ParsedNFTs = lists:map(fun(NFT) ->
                        #{
                            mint => binary_to_list(maps:get(<<"mint">>, NFT)),
                            token_account => binary_to_list(maps:get(<<"token_account">>, NFT)),
                            owner => binary_to_list(maps:get(<<"owner">>, NFT)),
                            name => binary_to_list(maps:get(<<"name">>, NFT)),
                            symbol => binary_to_list(maps:get(<<"symbol">>, NFT)),
                            uri => binary_to_list(maps:get(<<"uri">>, NFT)),
                            metadata => case maps:get(<<"metadata">>, NFT, undefined) of
                                undefined -> undefined;
                                Meta -> parse_nft_metadata(Meta)
                            end,
                            image => case maps:get(<<"image">>, NFT, undefined) of
                                undefined -> undefined;
                                Img -> binary_to_list(Img)
                            end,
                            description => case maps:get(<<"description">>, NFT, undefined) of
                                undefined -> undefined;
                                Desc -> binary_to_list(Desc)
                            end
                        }
                    end, NFTs),
                    {ok, #{
                        public_key => binary_to_list(maps:get(<<"public_key">>, Response)),
                        nfts => ParsedNFTs
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            {error, {http_error, StatusCode, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_nft_metadata(MintAddress) ->
    RequestBody = #{mint_address => ensure_binary(MintAddress)},
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/nft/metadata",
    case httpc:request(post, {ApiUrl, [], "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"metadata">> := Metadata} = Response ->
                    {ok, #{
                        mint => binary_to_list(maps:get(<<"mint">>, Response)),
                        metadata => parse_nft_metadata(Metadata),
                        external_metadata => case maps:get(<<"external_metadata">>, Response, undefined) of
                            undefined -> undefined;
                            ExtMeta -> parse_external_metadata(ExtMeta)
                        end
                    }};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, nft_not_found};
        {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
            parse_error_response(ErrorBody);
        {error, Reason} ->
            {error, Reason}
    end.

transfer_nft(Token, FromWalletId, ToPublicKey, MintAddress) ->
    RequestBody = #{
        from_wallet_id => ensure_binary(FromWalletId),
        to_public_key => ensure_binary(ToPublicKey),
        mint_address => ensure_binary(MintAddress)
    },
    Json = jsx:encode(RequestBody),
    ApiUrl = ?SOLANA_API_BASE ++ "/wallet/nft/transfer",
    Headers = auth_headers(Token),
    case httpc:request(post, {ApiUrl, Headers, "application/json", Json},
                      [{timeout, ?DEFAULT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"signature">> := Signature} = Response ->
                    {ok, #{
                        signature => binary_to_list(Signature),
                        from_public_key => binary_to_list(maps:get(<<"from_public_key">>, Response)),
                        to_public_key => binary_to_list(maps:get(<<"to_public_key">>, Response)),
                        mint_address => binary_to_list(maps:get(<<"mint_address">>, Response)),
                        status => binary_to_list(maps:get(<<"status">>, Response)),
                        timestamp => maps:get(<<"timestamp">>, Response)
                    }};
                _ ->
                    {error, invalid_response}
            end;
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

parse_nft_metadata(Metadata) ->
    #{
        name => binary_to_list(maps:get(<<"name">>, Metadata)),
        symbol => binary_to_list(maps:get(<<"symbol">>, Metadata)),
        uri => binary_to_list(maps:get(<<"uri">>, Metadata)),
        seller_fee_basis_points => maps:get(<<"seller_fee_basis_points">>, Metadata),
        creators => lists:map(fun(Creator) ->
            #{
                address => binary_to_list(maps:get(<<"address">>, Creator)),
                verified => maps:get(<<"verified">>, Creator),
                share => maps:get(<<"share">>, Creator)
            }
        end, maps:get(<<"creators">>, Metadata, [])),
        collection => case maps:get(<<"collection">>, Metadata, undefined) of
            undefined -> undefined;
            Coll -> #{
                verified => maps:get(<<"verified">>, Coll),
                key => binary_to_list(maps:get(<<"key">>, Coll))
            }
        end,
        uses => case maps:get(<<"uses">>, Metadata, undefined) of
            undefined -> undefined;
            Uses -> #{
                use_method => binary_to_list(maps:get(<<"use_method">>, Uses)),
                remaining => maps:get(<<"remaining">>, Uses),
                total => maps:get(<<"total">>, Uses)
            }
        end
    }.

parse_external_metadata(ExtMeta) ->
    #{
        name => case maps:get(<<"name">>, ExtMeta, undefined) of
            undefined -> undefined;
            N -> binary_to_list(N)
        end,
        description => case maps:get(<<"description">>, ExtMeta, undefined) of
            undefined -> undefined;
            D -> binary_to_list(D)
        end,
        image => case maps:get(<<"image">>, ExtMeta, undefined) of
            undefined -> undefined;
            I -> binary_to_list(I)
        end,
        attributes => case maps:get(<<"attributes">>, ExtMeta, undefined) of
            undefined -> undefined;
            Attrs -> lists:map(fun(Attr) ->
                #{
                    trait_type => binary_to_list(maps:get(<<"trait_type">>, Attr)),
                    value => case maps:get(<<"value">>, Attr) of
                        V when is_binary(V) -> binary_to_list(V);
                        V -> V
                    end
                }
            end, Attrs)
        end
    }.

parse_error_response(ErrorBody) ->
    try
        case jsx:decode(list_to_binary(ErrorBody), [return_maps]) of
            #{<<"error">> := ErrorMsg} ->
                ErrorStr = binary_to_list(ErrorMsg),
                case string:find(ErrorStr, "Insufficient SOL balance") of
                    nomatch -> {error, binary_to_list(ErrorMsg)};
                    _ -> {error, insufficient_balance}
                end;
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
