-module(solana_wallet_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").

-export([start_link/0,
         create_wallet/3,
         create_wallet/6,
         import_wallet/7,
         get_wallet/1,
         get_wallet_by_public_key/1,
         get_user_wallets/1,
         update_wallet_last_used/1,
         update_wallet_label/2,
         set_primary_wallet/2,
         get_primary_wallet/1,
         delete_wallet/1,
         wallet_exists/1,
         public_key_exists/1,
         export_private_key/2,
         create_transaction/2,
         get_transaction/1,
         get_wallet_transactions/1,
         get_wallet_transactions/3,
         get_user_transactions/1,
         get_transactions_by_type/2,
         update_transaction_status/3,
         create_airdrop/2,
         get_airdrop/1,
         update_airdrop/2,
         get_user_airdrops/1,
         get_wallet_airdrops/1,
         create_airdrop_recipient/2,
         get_airdrop_recipients/1,
         update_airdrop_recipient/2,
         create_stake_account/2,
         get_stake_account/1,
         get_stake_by_address/1,
         update_stake_account/2,
         get_user_stakes/1,
         get_wallet_stakes/1,
         create_token_account/2,
         get_token_account/1,
         get_wallet_token_accounts/1,
         update_token_account_balance/3,
         create_nft/2,
         get_nft/1,
         get_wallet_nfts/1,
         update_nft/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 50,
                worker_pool = []}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

create_wallet(UserId, Label, UserPassword) ->
    gen_server:call({global, ?MODULE}, {create_wallet_secure, UserId, Label, UserPassword}, 30000).

create_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, Label) ->
    gen_server:call({global, ?MODULE}, {create_wallet_legacy, UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, Label}, 30000).

import_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, DerivationPath, Label) ->
    gen_server:call({global, ?MODULE}, {import_wallet, UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, DerivationPath, Label}, 30000).

get_wallet(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet, WalletId}).

get_wallet_by_public_key(PublicKey) ->
    gen_server:call({global, ?MODULE}, {get_wallet_by_public_key, PublicKey}).

get_user_wallets(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_wallets, UserId}).

update_wallet_last_used(WalletId) ->
    gen_server:call({global, ?MODULE}, {update_wallet_last_used, WalletId}).

update_wallet_label(WalletId, NewLabel) ->
    gen_server:call({global, ?MODULE}, {update_wallet_label, WalletId, NewLabel}).

set_primary_wallet(UserId, WalletId) ->
    gen_server:call({global, ?MODULE}, {set_primary_wallet, UserId, WalletId}).

get_primary_wallet(UserId) ->
    gen_server:call({global, ?MODULE}, {get_primary_wallet, UserId}).

delete_wallet(WalletId) ->
    gen_server:call({global, ?MODULE}, {delete_wallet, WalletId}).

wallet_exists(WalletId) ->
    gen_server:call({global, ?MODULE}, {wallet_exists, WalletId}).

public_key_exists(PublicKey) ->
    gen_server:call({global, ?MODULE}, {public_key_exists, PublicKey}).

export_private_key(WalletId, UserPassword) ->
    gen_server:call({global, ?MODULE}, {export_private_key, WalletId, UserPassword}, 30000).

create_transaction(WalletId, TxData) ->
    gen_server:call({global, ?MODULE}, {create_transaction, WalletId, TxData}).

get_transaction(TxId) ->
    gen_server:call({global, ?MODULE}, {get_transaction, TxId}).

get_wallet_transactions(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_transactions, WalletId}).

get_wallet_transactions(WalletId, Limit, Offset) ->
    gen_server:call({global, ?MODULE}, {get_wallet_transactions_paginated, WalletId, Limit, Offset}).

get_user_transactions(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_transactions, UserId}).

get_transactions_by_type(WalletId, TxType) ->
    gen_server:call({global, ?MODULE}, {get_transactions_by_type, WalletId, TxType}).

update_transaction_status(TxId, Status, ConfirmedAt) ->
    gen_server:call({global, ?MODULE}, {update_transaction_status, TxId, Status, ConfirmedAt}).

create_airdrop(WalletId, AirdropData) ->
    gen_server:call({global, ?MODULE}, {create_airdrop, WalletId, AirdropData}).

get_airdrop(AirdropId) ->
    gen_server:call({global, ?MODULE}, {get_airdrop, AirdropId}).

update_airdrop(AirdropId, Updates) ->
    gen_server:call({global, ?MODULE}, {update_airdrop, AirdropId, Updates}).

get_user_airdrops(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_airdrops, UserId}).

get_wallet_airdrops(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_airdrops, WalletId}).

create_airdrop_recipient(AirdropId, RecipientData) ->
    gen_server:call({global, ?MODULE}, {create_airdrop_recipient, AirdropId, RecipientData}).

get_airdrop_recipients(AirdropId) ->
    gen_server:call({global, ?MODULE}, {get_airdrop_recipients, AirdropId}).

update_airdrop_recipient(RecipientId, Updates) ->
    gen_server:call({global, ?MODULE}, {update_airdrop_recipient, RecipientId, Updates}).

create_stake_account(WalletId, StakeData) ->
    gen_server:call({global, ?MODULE}, {create_stake_account, WalletId, StakeData}).

get_stake_account(StakeId) ->
    gen_server:call({global, ?MODULE}, {get_stake_account, StakeId}).

get_stake_by_address(StakeAccountAddress) ->
    gen_server:call({global, ?MODULE}, {get_stake_by_address, StakeAccountAddress}).

update_stake_account(StakeId, Updates) ->
    gen_server:call({global, ?MODULE}, {update_stake_account, StakeId, Updates}).

get_user_stakes(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_stakes, UserId}).

get_wallet_stakes(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_stakes, WalletId}).

create_token_account(WalletId, TokenData) ->
    gen_server:call({global, ?MODULE}, {create_token_account, WalletId, TokenData}).

get_token_account(TokenAccountId) ->
    gen_server:call({global, ?MODULE}, {get_token_account, TokenAccountId}).

get_wallet_token_accounts(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_token_accounts, WalletId}).

update_token_account_balance(TokenAccountId, NewBalance, LastSynced) ->
    gen_server:call({global, ?MODULE}, {update_token_account_balance, TokenAccountId, NewBalance, LastSynced}).

create_nft(WalletId, NFTData) ->
    gen_server:call({global, ?MODULE}, {create_nft, WalletId, NFTData}).

get_nft(NFTId) ->
    gen_server:call({global, ?MODULE}, {get_nft, NFTId}).

get_wallet_nfts(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_nfts, WalletId}).

update_nft(NFTId, Updates) ->
    gen_server:call({global, ?MODULE}, {update_nft, NFTId, Updates}).

init([]) ->
    ?LOG_NOTICE("Solana wallet server has been started - ~p", [self()]),
    PoolSize = application:get_env(social_network, solana_wallet_worker_pool_size, 50),
    WorkerPool = initialize_worker_pool(PoolSize),
    {ok, #state{worker_pool_size = PoolSize, worker_pool = WorkerPool}}.

initialize_worker_pool(Size) ->
    [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, Size)].

worker_loop() ->
    receive
        {create_wallet_secure, UserId, Label, UserPassword, From} ->
            Result = solana_walletdb:create_wallet(UserId, Label, UserPassword),
            gen_server:reply(From, Result),
            worker_loop();

        {create_wallet_legacy, UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, Label, From} ->
            Result = solana_walletdb:create_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, Label),
            gen_server:reply(From, Result),
            worker_loop();

        {import_wallet, UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, DerivationPath, Label, From} ->
            Result = solana_walletdb:import_wallet(UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, DerivationPath, Label),
            gen_server:reply(From, Result),
            worker_loop();

        {export_private_key, WalletId, UserPassword, From} ->
            Result = solana_walletdb:export_private_key(WalletId, UserPassword),
            gen_server:reply(From, Result),
            worker_loop();

        {stop, From} ->
            From ! {stopped, self()};

        Other ->
            ?LOG_WARNING("Solana wallet worker received unknown message: ~p", [Other]),
            worker_loop()
    end.

handle_call({create_wallet_secure, UserId, Label, UserPassword}, From,
            State = #state{worker_pool = [Worker | RestWorkers]}) ->
    Worker ! {create_wallet_secure, UserId, Label, UserPassword, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};

handle_call({create_wallet_legacy, UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, Label}, From,
            State = #state{worker_pool = [Worker | RestWorkers]}) ->
    Worker ! {create_wallet_legacy, UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, Label, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};

handle_call({import_wallet, UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, DerivationPath, Label}, From,
            State = #state{worker_pool = [Worker | RestWorkers]}) ->
    Worker ! {import_wallet, UserId, PublicKey, EncryptedPrivateKey, IV, AuthTag, DerivationPath, Label, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};

handle_call({export_private_key, WalletId, UserPassword}, From,
            State = #state{worker_pool = [Worker | RestWorkers]}) ->
    Worker ! {export_private_key, WalletId, UserPassword, From},
    {noreply, State#state{worker_pool = RestWorkers ++ [Worker]}};

handle_call({get_wallet, WalletId}, _From, State) ->
    Res = solana_walletdb:get_wallet(WalletId),
    {reply, Res, State};

handle_call({get_wallet_by_public_key, PublicKey}, _From, State) ->
    Res = solana_walletdb:get_wallet_by_public_key(PublicKey),
    {reply, Res, State};

handle_call({get_user_wallets, UserId}, _From, State) ->
    Res = solana_walletdb:get_user_wallets(UserId),
    {reply, Res, State};

handle_call({update_wallet_last_used, WalletId}, _From, State) ->
    Res = solana_walletdb:update_wallet_last_used(WalletId),
    {reply, Res, State};

handle_call({update_wallet_label, WalletId, NewLabel}, _From, State) ->
    Res = solana_walletdb:update_wallet_label(WalletId, NewLabel),
    {reply, Res, State};

handle_call({set_primary_wallet, UserId, WalletId}, _From, State) ->
    Res = solana_walletdb:set_primary_wallet(UserId, WalletId),
    {reply, Res, State};

handle_call({get_primary_wallet, UserId}, _From, State) ->
    Res = solana_walletdb:get_primary_wallet(UserId),
    {reply, Res, State};

handle_call({delete_wallet, WalletId}, _From, State) ->
    Res = solana_walletdb:delete_wallet(WalletId),
    {reply, Res, State};

handle_call({wallet_exists, WalletId}, _From, State) ->
    Res = solana_walletdb:wallet_exists(WalletId),
    {reply, Res, State};

handle_call({public_key_exists, PublicKey}, _From, State) ->
    Res = solana_walletdb:public_key_exists(PublicKey),
    {reply, Res, State};

handle_call({create_transaction, WalletId, TxData}, _From, State) ->
    Res = solana_walletdb:create_transaction(WalletId, TxData),
    {reply, Res, State};

handle_call({get_transaction, TxId}, _From, State) ->
    Res = solana_walletdb:get_transaction(TxId),
    {reply, Res, State};

handle_call({get_wallet_transactions, WalletId}, _From, State) ->
    Res = solana_walletdb:get_wallet_transactions(WalletId),
    {reply, Res, State};

handle_call({get_wallet_transactions_paginated, WalletId, Limit, Offset}, _From, State) ->
    Res = solana_walletdb:get_wallet_transactions(WalletId, Limit, Offset),
    {reply, Res, State};

handle_call({get_user_transactions, UserId}, _From, State) ->
    Res = solana_walletdb:get_user_transactions(UserId),
    {reply, Res, State};

handle_call({get_transactions_by_type, WalletId, TxType}, _From, State) ->
    Res = solana_walletdb:get_transactions_by_type(WalletId, TxType),
    {reply, Res, State};

handle_call({update_transaction_status, TxId, Status, ConfirmedAt}, _From, State) ->
    Res = solana_walletdb:update_transaction_status(TxId, Status, ConfirmedAt),
    {reply, Res, State};

handle_call({create_airdrop, WalletId, AirdropData}, _From, State) ->
    Res = solana_walletdb:create_airdrop(WalletId, AirdropData),
    {reply, Res, State};

handle_call({get_airdrop, AirdropId}, _From, State) ->
    Res = solana_walletdb:get_airdrop(AirdropId),
    {reply, Res, State};

handle_call({update_airdrop, AirdropId, Updates}, _From, State) ->
    Res = solana_walletdb:update_airdrop(AirdropId, Updates),
    {reply, Res, State};

handle_call({get_user_airdrops, UserId}, _From, State) ->
    Res = solana_walletdb:get_user_airdrops(UserId),
    {reply, Res, State};

handle_call({get_wallet_airdrops, WalletId}, _From, State) ->
    Res = solana_walletdb:get_wallet_airdrops(WalletId),
    {reply, Res, State};

handle_call({create_airdrop_recipient, AirdropId, RecipientData}, _From, State) ->
    Res = solana_walletdb:create_airdrop_recipient(AirdropId, RecipientData),
    {reply, Res, State};

handle_call({get_airdrop_recipients, AirdropId}, _From, State) ->
    Res = solana_walletdb:get_airdrop_recipients(AirdropId),
    {reply, Res, State};

handle_call({update_airdrop_recipient, RecipientId, Updates}, _From, State) ->
    Res = solana_walletdb:update_airdrop_recipient(RecipientId, Updates),
    {reply, Res, State};

handle_call({create_stake_account, WalletId, StakeData}, _From, State) ->
    Res = solana_walletdb:create_stake_account(WalletId, StakeData),
    {reply, Res, State};

handle_call({get_stake_account, StakeId}, _From, State) ->
    Res = solana_walletdb:get_stake_account(StakeId),
    {reply, Res, State};

handle_call({get_stake_by_address, StakeAccountAddress}, _From, State) ->
    Res = solana_walletdb:get_stake_by_address(StakeAccountAddress),
    {reply, Res, State};

handle_call({update_stake_account, StakeId, Updates}, _From, State) ->
    Res = solana_walletdb:update_stake_account(StakeId, Updates),
    {reply, Res, State};

handle_call({get_user_stakes, UserId}, _From, State) ->
    Res = solana_walletdb:get_user_stakes(UserId),
    {reply, Res, State};

handle_call({get_wallet_stakes, WalletId}, _From, State) ->
    Res = solana_walletdb:get_wallet_stakes(WalletId),
    {reply, Res, State};

handle_call({create_token_account, WalletId, TokenData}, _From, State) ->
    Res = solana_walletdb:create_token_account(WalletId, TokenData),
    {reply, Res, State};

handle_call({get_token_account, TokenAccountId}, _From, State) ->
    Res = solana_walletdb:get_token_account(TokenAccountId),
    {reply, Res, State};

handle_call({get_wallet_token_accounts, WalletId}, _From, State) ->
    Res = solana_walletdb:get_wallet_token_accounts(WalletId),
    {reply, Res, State};

handle_call({update_token_account_balance, TokenAccountId, NewBalance, LastSynced}, _From, State) ->
    Res = solana_walletdb:update_token_account_balance(TokenAccountId, NewBalance, LastSynced),
    {reply, Res, State};

handle_call({create_nft, WalletId, NFTData}, _From, State) ->
    Res = solana_walletdb:create_nft(WalletId, NFTData),
    {reply, Res, State};

handle_call({get_nft, NFTId}, _From, State) ->
    Res = solana_walletdb:get_nft(NFTId),
    {reply, Res, State};

handle_call({get_wallet_nfts, WalletId}, _From, State) ->
    Res = solana_walletdb:get_wallet_nfts(WalletId),
    {reply, Res, State};

handle_call({update_nft, NFTId, Updates}, _From, State) ->
    Res = solana_walletdb:update_nft(NFTId, Updates),
    {reply, Res, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State = #state{worker_pool = Workers}) ->
    case lists:member(Pid, Workers) of
        true ->
            ?LOG_WARNING("Solana wallet worker ~p crashed with reason: ~p. Replacing.", [Pid, Reason]),
            NewWorker = spawn_link(fun() -> worker_loop() end),
            NewWorkers = lists:delete(Pid, Workers) ++ [NewWorker],
            {noreply, State#state{worker_pool = NewWorkers}};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{worker_pool = Workers}) ->
    [Worker ! {stop, self()} || Worker <- Workers],
    [receive {stopped, W} -> ok after 1000 -> ok end || W <- Workers],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
