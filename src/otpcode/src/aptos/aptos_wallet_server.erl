-module(aptos_wallet_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0,

         create_wallet/3,
         create_wallet/4,
         get_wallet/1,
         get_wallet_by_address/1,
         get_user_wallets/1,
         get_primary_wallet/1,
         set_primary_wallet/2,
         update_wallet_last_used/1,
         update_wallet_label/2,
         delete_wallet/1,
         wallet_exists/1,
         address_exists/1,

         create_transaction/2,
         get_transaction/1,
         get_transaction_by_hash/1,
         get_wallet_transactions/1,
         get_wallet_transactions/3,
         get_user_transactions/1,
         get_transactions_by_type/2,

         create_nft_collection/2,
         get_nft_collection/1,
         get_wallet_nft_collections/1,
         get_user_nft_collections/1,
         get_nft_collection_by_name/2,

         create_nft_token/2,
         get_nft_token/1,
         get_wallet_nft_tokens/1,
         get_collection_nft_tokens/1,

         create_fungible_asset/2,
         get_fungible_asset/1,
         get_wallet_fungible_assets/1,
         get_user_fungible_assets/1,
         get_fungible_asset_by_metadata/1,

         create_stake/2,
         get_stake/1,
         get_wallet_stakes/1,
         get_user_stakes/1,
         get_stakes_by_pool/2,
         update_stake/2,
         delete_stake/1,

         upsert_coin_balance/2,
         get_coin_balance/2,
         get_wallet_coin_balances/1,
         get_user_coin_balances/1,
         delete_coin_balance/1,

         upsert_fa_balance/2,
         get_fa_balance/2,
         get_wallet_fa_balances/1,
         get_user_fa_balances/1,
         delete_fa_balance/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 50,
                worker_pool = []}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

create_wallet(UserId, Address, EncryptedData) ->
    gen_server:call({global, ?MODULE},
                    {create_wallet, UserId, Address, EncryptedData}, 30000).

create_wallet(UserId, Address, EncryptedData, Label) ->
    gen_server:call({global, ?MODULE},
                    {create_wallet_labeled, UserId, Address, EncryptedData, Label}, 30000).

get_wallet(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet, WalletId}).

get_wallet_by_address(Address) ->
    gen_server:call({global, ?MODULE}, {get_wallet_by_address, Address}).

get_user_wallets(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_wallets, UserId}).

get_primary_wallet(UserId) ->
    gen_server:call({global, ?MODULE}, {get_primary_wallet, UserId}).

set_primary_wallet(UserId, WalletId) ->
    gen_server:call({global, ?MODULE}, {set_primary_wallet, UserId, WalletId}).

update_wallet_last_used(WalletId) ->
    gen_server:call({global, ?MODULE}, {update_wallet_last_used, WalletId}).

update_wallet_label(WalletId, NewLabel) ->
    gen_server:call({global, ?MODULE}, {update_wallet_label, WalletId, NewLabel}).

delete_wallet(WalletId) ->
    gen_server:call({global, ?MODULE}, {delete_wallet, WalletId}).

wallet_exists(WalletId) ->
    gen_server:call({global, ?MODULE}, {wallet_exists, WalletId}).

address_exists(Address) ->
    gen_server:call({global, ?MODULE}, {address_exists, Address}).

create_transaction(WalletId, TxData) ->
    gen_server:call({global, ?MODULE},
                    {create_transaction, WalletId, TxData}, 30000).

get_transaction(TxId) ->
    gen_server:call({global, ?MODULE}, {get_transaction, TxId}).

get_transaction_by_hash(TxHash) ->
    gen_server:call({global, ?MODULE}, {get_transaction_by_hash, TxHash}).

get_wallet_transactions(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_transactions, WalletId}).

get_wallet_transactions(WalletId, Limit, Offset) ->
    gen_server:call({global, ?MODULE},
                    {get_wallet_transactions_paged, WalletId, Limit, Offset}).

get_user_transactions(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_transactions, UserId}).

get_transactions_by_type(WalletId, TxType) ->
    gen_server:call({global, ?MODULE}, {get_transactions_by_type, WalletId, TxType}).

create_nft_collection(WalletId, CollectionData) ->
    gen_server:call({global, ?MODULE},
                    {create_nft_collection, WalletId, CollectionData}, 30000).

get_nft_collection(CollectionId) ->
    gen_server:call({global, ?MODULE}, {get_nft_collection, CollectionId}).

get_wallet_nft_collections(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_nft_collections, WalletId}).

get_user_nft_collections(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_nft_collections, UserId}).

get_nft_collection_by_name(CreatorAddress, CollectionName) ->
    gen_server:call({global, ?MODULE},
                    {get_nft_collection_by_name, CreatorAddress, CollectionName}).

create_nft_token(WalletId, TokenData) ->
    gen_server:call({global, ?MODULE},
                    {create_nft_token, WalletId, TokenData}, 30000).

get_nft_token(TokenId) ->
    gen_server:call({global, ?MODULE}, {get_nft_token, TokenId}).

get_wallet_nft_tokens(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_nft_tokens, WalletId}).

get_collection_nft_tokens(CollectionId) ->
    gen_server:call({global, ?MODULE}, {get_collection_nft_tokens, CollectionId}).

create_fungible_asset(WalletId, FaData) ->
    gen_server:call({global, ?MODULE},
                    {create_fungible_asset, WalletId, FaData}, 30000).

get_fungible_asset(FaId) ->
    gen_server:call({global, ?MODULE}, {get_fungible_asset, FaId}).

get_wallet_fungible_assets(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_fungible_assets, WalletId}).

get_user_fungible_assets(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_fungible_assets, UserId}).

get_fungible_asset_by_metadata(MetadataAddress) ->
    gen_server:call({global, ?MODULE}, {get_fungible_asset_by_metadata, MetadataAddress}).

create_stake(WalletId, StakeData) ->
    gen_server:call({global, ?MODULE},
                    {create_stake, WalletId, StakeData}, 30000).

get_stake(StakeId) ->
    gen_server:call({global, ?MODULE}, {get_stake, StakeId}).

get_wallet_stakes(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_stakes, WalletId}).

get_user_stakes(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_stakes, UserId}).

get_stakes_by_pool(WalletId, PoolAddress) ->
    gen_server:call({global, ?MODULE}, {get_stakes_by_pool, WalletId, PoolAddress}).

update_stake(StakeId, Updates) ->
    gen_server:call({global, ?MODULE}, {update_stake, StakeId, Updates}).

delete_stake(StakeId) ->
    gen_server:call({global, ?MODULE}, {delete_stake, StakeId}).

upsert_coin_balance(WalletId, BalanceData) ->
    gen_server:call({global, ?MODULE},
                    {upsert_coin_balance, WalletId, BalanceData}, 30000).

get_coin_balance(WalletId, CoinType) ->
    gen_server:call({global, ?MODULE}, {get_coin_balance, WalletId, CoinType}).

get_wallet_coin_balances(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_coin_balances, WalletId}).

get_user_coin_balances(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_coin_balances, UserId}).

delete_coin_balance(BalanceId) ->
    gen_server:call({global, ?MODULE}, {delete_coin_balance, BalanceId}).

upsert_fa_balance(WalletId, BalanceData) ->
    gen_server:call({global, ?MODULE},
                    {upsert_fa_balance, WalletId, BalanceData}, 30000).

get_fa_balance(WalletId, AssetType) ->
    gen_server:call({global, ?MODULE}, {get_fa_balance, WalletId, AssetType}).

get_wallet_fa_balances(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_fa_balances, WalletId}).

get_user_fa_balances(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_fa_balances, UserId}).

delete_fa_balance(FaBalanceId) ->
    gen_server:call({global, ?MODULE}, {delete_fa_balance, FaBalanceId}).

init([]) ->
    ?LOG_NOTICE("Aptos wallet server has been started - ~p", [self()]),
    PoolSize = application:get_env(otpcode, aptos_wallet_worker_pool_size, 50),
    Workers  = [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, PoolSize)],
    {ok, #state{worker_pool_size = PoolSize, worker_pool = Workers}}.

handle_call(Request, _From, State) ->
    Worker = lists:nth(rand:uniform(length(State#state.worker_pool)),
                       State#state.worker_pool),
    Worker ! {self(), Request},
    receive
        {Worker, Response} -> {reply, Response, State}
    after 30000 ->
        {reply, {error, timeout}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State = #state{worker_pool = Workers}) ->
    case lists:member(Pid, Workers) of
        true ->
            ?LOG_WARNING("Aptos wallet worker ~p crashed with reason: ~p. Replacing.", [Pid, Reason]),
            NewWorker = spawn_link(fun() -> worker_loop() end),
            NewWorkers = lists:delete(Pid, Workers) ++ [NewWorker],
            {noreply, State#state{worker_pool = NewWorkers}};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

worker_loop() ->
    receive
        {From, {create_wallet, UserId, Address, EncryptedData}} ->
            EncKey  = maps:get(encrypted_private_key, EncryptedData),
            IV      = maps:get(encryption_iv, EncryptedData),
            Tag     = maps:get(encryption_tag, EncryptedData),
            Network = tag_to_network(Tag),
            Result  = aptos_walletdb:create_wallet(UserId, Address, EncKey, IV, Tag, undefined, Network),
            From ! {self(), Result};

        {From, {create_wallet_labeled, UserId, Address, EncryptedData, Label}} ->
            EncKey  = maps:get(encrypted_private_key, EncryptedData),
            IV      = maps:get(encryption_iv, EncryptedData),
            Tag     = maps:get(encryption_tag, EncryptedData),
            Network = tag_to_network(Tag),
            Result  = aptos_walletdb:create_wallet(UserId, Address, EncKey, IV, Tag, Label, Network),
            From ! {self(), Result};

        {From, {get_wallet, WalletId}} ->
            From ! {self(), aptos_walletdb:get_wallet(WalletId)};

        {From, {get_wallet_by_address, Address}} ->
            From ! {self(), aptos_walletdb:get_wallet_by_address(Address)};

        {From, {get_user_wallets, UserId}} ->
            From ! {self(), aptos_walletdb:get_user_wallets(UserId)};

        {From, {get_primary_wallet, UserId}} ->
            From ! {self(), aptos_walletdb:get_primary_wallet(UserId)};

        {From, {set_primary_wallet, UserId, WalletId}} ->
            From ! {self(), aptos_walletdb:set_primary_wallet(UserId, WalletId)};

        {From, {update_wallet_last_used, WalletId}} ->
            From ! {self(), aptos_walletdb:update_wallet_last_used(WalletId)};

        {From, {update_wallet_label, WalletId, NewLabel}} ->
            From ! {self(), aptos_walletdb:update_wallet_label(WalletId, NewLabel)};

        {From, {delete_wallet, WalletId}} ->
            From ! {self(), aptos_walletdb:delete_wallet(WalletId)};

        {From, {wallet_exists, WalletId}} ->
            From ! {self(), aptos_walletdb:wallet_exists(WalletId)};

        {From, {address_exists, Address}} ->
            From ! {self(), aptos_walletdb:address_exists(Address)};

        {From, {create_transaction, WalletId, TxData}} ->
            From ! {self(), aptos_walletdb:create_transaction(WalletId, TxData)};

        {From, {get_transaction, TxId}} ->
            From ! {self(), aptos_walletdb:get_transaction(TxId)};

        {From, {get_transaction_by_hash, TxHash}} ->
            From ! {self(), aptos_walletdb:get_transaction_by_hash(TxHash)};

        {From, {get_wallet_transactions, WalletId}} ->
            From ! {self(), aptos_walletdb:get_wallet_transactions(WalletId)};

        {From, {get_wallet_transactions_paged, WalletId, Limit, Offset}} ->
            From ! {self(), aptos_walletdb:get_wallet_transactions(WalletId, Limit, Offset)};

        {From, {get_user_transactions, UserId}} ->
            From ! {self(), aptos_walletdb:get_user_transactions(UserId)};

        {From, {get_transactions_by_type, WalletId, TxType}} ->
            From ! {self(), aptos_walletdb:get_transactions_by_type(WalletId, TxType)};

        {From, {create_nft_collection, WalletId, CollectionData}} ->
            From ! {self(), aptos_walletdb:create_nft_collection(WalletId, CollectionData)};

        {From, {get_nft_collection, CollectionId}} ->
            From ! {self(), aptos_walletdb:get_nft_collection(CollectionId)};

        {From, {get_wallet_nft_collections, WalletId}} ->
            From ! {self(), aptos_walletdb:get_wallet_nft_collections(WalletId)};

        {From, {get_user_nft_collections, UserId}} ->
            From ! {self(), aptos_walletdb:get_user_nft_collections(UserId)};

        {From, {get_nft_collection_by_name, CreatorAddress, CollectionName}} ->
            From ! {self(), aptos_walletdb:get_nft_collection_by_name(CreatorAddress, CollectionName)};

        {From, {create_nft_token, WalletId, TokenData}} ->
            From ! {self(), aptos_walletdb:create_nft_token(WalletId, TokenData)};

        {From, {get_nft_token, TokenId}} ->
            From ! {self(), aptos_walletdb:get_nft_token(TokenId)};

        {From, {get_wallet_nft_tokens, WalletId}} ->
            From ! {self(), aptos_walletdb:get_wallet_nft_tokens(WalletId)};

        {From, {get_collection_nft_tokens, CollectionId}} ->
            From ! {self(), aptos_walletdb:get_collection_nft_tokens(CollectionId)};

        {From, {create_fungible_asset, WalletId, FaData}} ->
            From ! {self(), aptos_walletdb:create_fungible_asset(WalletId, FaData)};

        {From, {get_fungible_asset, FaId}} ->
            From ! {self(), aptos_walletdb:get_fungible_asset(FaId)};

        {From, {get_wallet_fungible_assets, WalletId}} ->
            From ! {self(), aptos_walletdb:get_wallet_fungible_assets(WalletId)};

        {From, {get_user_fungible_assets, UserId}} ->
            From ! {self(), aptos_walletdb:get_user_fungible_assets(UserId)};

        {From, {get_fungible_asset_by_metadata, MetadataAddress}} ->
            From ! {self(), aptos_walletdb:get_fungible_asset_by_metadata(MetadataAddress)};

        {From, {create_stake, WalletId, StakeData}} ->
            From ! {self(), aptos_walletdb:create_stake(WalletId, StakeData)};

        {From, {get_stake, StakeId}} ->
            From ! {self(), aptos_walletdb:get_stake(StakeId)};

        {From, {get_wallet_stakes, WalletId}} ->
            From ! {self(), aptos_walletdb:get_wallet_stakes(WalletId)};

        {From, {get_user_stakes, UserId}} ->
            From ! {self(), aptos_walletdb:get_user_stakes(UserId)};

        {From, {get_stakes_by_pool, WalletId, PoolAddress}} ->
            From ! {self(), aptos_walletdb:get_stakes_by_pool(WalletId, PoolAddress)};

        {From, {update_stake, StakeId, Updates}} ->
            From ! {self(), aptos_walletdb:update_stake(StakeId, Updates)};

        {From, {delete_stake, StakeId}} ->
            From ! {self(), aptos_walletdb:delete_stake(StakeId)};

        {From, {upsert_coin_balance, WalletId, BalanceData}} ->
            From ! {self(), aptos_walletdb:upsert_coin_balance(WalletId, BalanceData)};

        {From, {get_coin_balance, WalletId, CoinType}} ->
            From ! {self(), aptos_walletdb:get_coin_balance(WalletId, CoinType)};

        {From, {get_wallet_coin_balances, WalletId}} ->
            From ! {self(), aptos_walletdb:get_wallet_coin_balances(WalletId)};

        {From, {get_user_coin_balances, UserId}} ->
            From ! {self(), aptos_walletdb:get_user_coin_balances(UserId)};

        {From, {delete_coin_balance, BalanceId}} ->
            From ! {self(), aptos_walletdb:delete_coin_balance(BalanceId)};

        {From, {upsert_fa_balance, WalletId, BalanceData}} ->
            From ! {self(), aptos_walletdb:upsert_fa_balance(WalletId, BalanceData)};

        {From, {get_fa_balance, WalletId, AssetType}} ->
            From ! {self(), aptos_walletdb:get_fa_balance(WalletId, AssetType)};

        {From, {get_wallet_fa_balances, WalletId}} ->
            From ! {self(), aptos_walletdb:get_wallet_fa_balances(WalletId)};

        {From, {get_user_fa_balances, UserId}} ->
            From ! {self(), aptos_walletdb:get_user_fa_balances(UserId)};

        {From, {delete_fa_balance, FaBalanceId}} ->
            From ! {self(), aptos_walletdb:delete_fa_balance(FaBalanceId)};

        _Other ->
            ok
    end,
    worker_loop().

tag_to_network(Tag) when is_list(Tag)   -> binary_to_atom(list_to_binary(Tag), utf8);
tag_to_network(Tag) when is_binary(Tag) -> binary_to_atom(Tag, utf8);
tag_to_network(Tag) when is_atom(Tag)   -> Tag;
tag_to_network(_)                       -> testnet.
