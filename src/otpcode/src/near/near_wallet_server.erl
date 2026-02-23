-module(near_wallet_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0,

         create_wallet/3,
         create_wallet/4,
         get_wallet/1,
         get_wallet_by_account_id/1,
         get_user_wallets/1,
         get_primary_wallet/1,
         set_primary_wallet/2,
         update_wallet_last_used/1,
         update_wallet_label/2,
         delete_wallet/1,
         wallet_exists/1,
         account_id_exists/1,

         create_transaction/2,
         get_transaction/1,
         get_wallet_transactions/1,
         get_wallet_transactions/3,
         get_user_transactions/1,
         get_transactions_by_type/2,

         create_access_key/2,
         get_access_key/1,
         get_wallet_access_keys/1,
         get_user_access_keys/1,
         delete_access_key/1,

         create_stake/2,
         get_stake/1,
         get_wallet_stakes/1,
         get_user_stakes/1,
         update_stake/2,

         create_implicit_account/2,
         get_implicit_account/1,
         get_implicit_account_by_account_id/1,
         get_wallet_implicit_accounts/1,
         mark_implicit_funded/2,

         create_social_post/2,
         get_social_post/1,
         get_wallet_social_posts/1,
         get_user_social_posts/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {worker_pool_size = 50,
                worker_pool = []}).


start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

create_wallet(UserId, AccountId, EncryptedData) ->
    gen_server:call({global, ?MODULE},
                    {create_wallet, UserId, AccountId, EncryptedData}, 30000).

create_wallet(UserId, AccountId, EncryptedData, Label) ->
    gen_server:call({global, ?MODULE},
                    {create_wallet_labeled, UserId, AccountId, EncryptedData, Label}, 30000).

get_wallet(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet, WalletId}).

get_wallet_by_account_id(AccountId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_by_account_id, AccountId}).

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

account_id_exists(AccountId) ->
    gen_server:call({global, ?MODULE}, {account_id_exists, AccountId}).

create_transaction(WalletId, TxData) ->
    gen_server:call({global, ?MODULE}, {create_transaction, WalletId, TxData}).

get_transaction(TxId) ->
    gen_server:call({global, ?MODULE}, {get_transaction, TxId}).

get_wallet_transactions(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_transactions, WalletId}).

get_wallet_transactions(WalletId, Limit, Offset) ->
    gen_server:call({global, ?MODULE},
                    {get_wallet_transactions_paginated, WalletId, Limit, Offset}).

get_user_transactions(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_transactions, UserId}).

get_transactions_by_type(WalletId, TxType) ->
    gen_server:call({global, ?MODULE}, {get_transactions_by_type, WalletId, TxType}).

create_access_key(WalletId, KeyData) ->
    gen_server:call({global, ?MODULE}, {create_access_key, WalletId, KeyData}).

get_access_key(KeyId) ->
    gen_server:call({global, ?MODULE}, {get_access_key, KeyId}).

get_wallet_access_keys(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_access_keys, WalletId}).

get_user_access_keys(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_access_keys, UserId}).

delete_access_key(KeyId) ->
    gen_server:call({global, ?MODULE}, {delete_access_key, KeyId}).


create_stake(WalletId, StakeData) ->
    gen_server:call({global, ?MODULE}, {create_stake, WalletId, StakeData}).

get_stake(StakeId) ->
    gen_server:call({global, ?MODULE}, {get_stake, StakeId}).

get_wallet_stakes(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_stakes, WalletId}).

get_user_stakes(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_stakes, UserId}).

update_stake(StakeId, Updates) ->
    gen_server:call({global, ?MODULE}, {update_stake, StakeId, Updates}).

create_implicit_account(WalletId, ImplicitData) ->
    gen_server:call({global, ?MODULE}, {create_implicit_account, WalletId, ImplicitData}).

get_implicit_account(ImplicitId) ->
    gen_server:call({global, ?MODULE}, {get_implicit_account, ImplicitId}).

get_implicit_account_by_account_id(AccountId) ->
    gen_server:call({global, ?MODULE}, {get_implicit_account_by_account_id, AccountId}).

get_wallet_implicit_accounts(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_implicit_accounts, WalletId}).

mark_implicit_funded(ImplicitId, FundedAt) ->
    gen_server:call({global, ?MODULE}, {mark_implicit_funded, ImplicitId, FundedAt}).


create_social_post(WalletId, PostData) ->
    gen_server:call({global, ?MODULE}, {create_social_post, WalletId, PostData}).

get_social_post(PostId) ->
    gen_server:call({global, ?MODULE}, {get_social_post, PostId}).

get_wallet_social_posts(WalletId) ->
    gen_server:call({global, ?MODULE}, {get_wallet_social_posts, WalletId}).

get_user_social_posts(UserId) ->
    gen_server:call({global, ?MODULE}, {get_user_social_posts, UserId}).


init([]) ->
    ?LOG_NOTICE("Near wallet server has been started - ~p", [self()]),
    PoolSize = application:get_env(social_network, near_wallet_worker_pool_size, 50),
    WorkerPool = initialize_worker_pool(PoolSize),
    {ok, #state{worker_pool_size = PoolSize, worker_pool = WorkerPool}}.

initialize_worker_pool(Size) ->
    [spawn_link(fun() -> worker_loop() end) || _ <- lists:seq(1, Size)].


worker_loop() ->
    receive
        {create_wallet, UserId, AccountId, EncryptedData, From} ->
            EncKey = maps:get(encrypted_private_key, EncryptedData),
            IV     = maps:get(encryption_iv, EncryptedData),
            Tag    = maps:get(encryption_tag, EncryptedData),
            Result = near_walletdb:create_wallet(UserId, AccountId, EncKey, IV, Tag),
            gen_server:reply(From, Result),
            worker_loop();

        {create_wallet_labeled, UserId, AccountId, EncryptedData, Label, From} ->
            EncKey = maps:get(encrypted_private_key, EncryptedData),
            IV     = maps:get(encryption_iv, EncryptedData),
            Tag    = maps:get(encryption_tag, EncryptedData),
            Result = near_walletdb:create_wallet(UserId, AccountId, EncKey, IV, Tag, Label),
            gen_server:reply(From, Result),
            worker_loop();

        {stop, From} ->
            From ! {stopped, self()};

        Other ->
            ?LOG_WARNING("Near wallet worker received unknown message: ~p", [Other]),
            worker_loop()
    end.

handle_call({create_wallet, UserId, AccountId, EncryptedData}, From,
            State = #state{worker_pool = [Worker | Rest]}) ->
    Worker ! {create_wallet, UserId, AccountId, EncryptedData, From},
    {noreply, State#state{worker_pool = Rest ++ [Worker]}};

handle_call({create_wallet_labeled, UserId, AccountId, EncryptedData, Label}, From,
            State = #state{worker_pool = [Worker | Rest]}) ->
    Worker ! {create_wallet_labeled, UserId, AccountId, EncryptedData, Label, From},
    {noreply, State#state{worker_pool = Rest ++ [Worker]}};


handle_call({get_wallet, WalletId}, _From, State) ->
    {reply, near_walletdb:get_wallet(WalletId), State};

handle_call({get_wallet_by_account_id, AccountId}, _From, State) ->
    {reply, near_walletdb:get_wallet_by_account_id(AccountId), State};

handle_call({get_user_wallets, UserId}, _From, State) ->
    {reply, near_walletdb:get_user_wallets(UserId), State};

handle_call({get_primary_wallet, UserId}, _From, State) ->
    {reply, near_walletdb:get_primary_wallet(UserId), State};

handle_call({set_primary_wallet, UserId, WalletId}, _From, State) ->
    {reply, near_walletdb:set_primary_wallet(UserId, WalletId), State};

handle_call({update_wallet_last_used, WalletId}, _From, State) ->
    {reply, near_walletdb:update_wallet_last_used(WalletId), State};

handle_call({update_wallet_label, WalletId, NewLabel}, _From, State) ->
    {reply, near_walletdb:update_wallet_label(WalletId, NewLabel), State};

handle_call({delete_wallet, WalletId}, _From, State) ->
    {reply, near_walletdb:delete_wallet(WalletId), State};

handle_call({wallet_exists, WalletId}, _From, State) ->
    {reply, near_walletdb:wallet_exists(WalletId), State};

handle_call({account_id_exists, AccountId}, _From, State) ->
    {reply, near_walletdb:account_id_exists(AccountId), State};


handle_call({create_transaction, WalletId, TxData}, _From, State) ->
    {reply, near_walletdb:create_transaction(WalletId, TxData), State};

handle_call({get_transaction, TxId}, _From, State) ->
    {reply, near_walletdb:get_transaction(TxId), State};

handle_call({get_wallet_transactions, WalletId}, _From, State) ->
    {reply, near_walletdb:get_wallet_transactions(WalletId), State};

handle_call({get_wallet_transactions_paginated, WalletId, Limit, Offset}, _From, State) ->
    {reply, near_walletdb:get_wallet_transactions(WalletId, Limit, Offset), State};

handle_call({get_user_transactions, UserId}, _From, State) ->
    {reply, near_walletdb:get_user_transactions(UserId), State};

handle_call({get_transactions_by_type, WalletId, TxType}, _From, State) ->
    {reply, near_walletdb:get_transactions_by_type(WalletId, TxType), State};


handle_call({create_access_key, WalletId, KeyData}, _From, State) ->
    {reply, near_walletdb:create_access_key(WalletId, KeyData), State};

handle_call({get_access_key, KeyId}, _From, State) ->
    {reply, near_walletdb:get_access_key(KeyId), State};

handle_call({get_wallet_access_keys, WalletId}, _From, State) ->
    {reply, near_walletdb:get_wallet_access_keys(WalletId), State};

handle_call({get_user_access_keys, UserId}, _From, State) ->
    {reply, near_walletdb:get_user_access_keys(UserId), State};

handle_call({delete_access_key, KeyId}, _From, State) ->
    {reply, near_walletdb:delete_access_key(KeyId), State};


handle_call({create_stake, WalletId, StakeData}, _From, State) ->
    {reply, near_walletdb:create_stake(WalletId, StakeData), State};

handle_call({get_stake, StakeId}, _From, State) ->
    {reply, near_walletdb:get_stake(StakeId), State};

handle_call({get_wallet_stakes, WalletId}, _From, State) ->
    {reply, near_walletdb:get_wallet_stakes(WalletId), State};

handle_call({get_user_stakes, UserId}, _From, State) ->
    {reply, near_walletdb:get_user_stakes(UserId), State};

handle_call({update_stake, StakeId, Updates}, _From, State) ->
    {reply, near_walletdb:update_stake(StakeId, Updates), State};


handle_call({create_implicit_account, WalletId, ImplicitData}, _From, State) ->
    {reply, near_walletdb:create_implicit_account(WalletId, ImplicitData), State};

handle_call({get_implicit_account, ImplicitId}, _From, State) ->
    {reply, near_walletdb:get_implicit_account(ImplicitId), State};

handle_call({get_implicit_account_by_account_id, AccountId}, _From, State) ->
    {reply, near_walletdb:get_implicit_account_by_account_id(AccountId), State};

handle_call({get_wallet_implicit_accounts, WalletId}, _From, State) ->
    {reply, near_walletdb:get_wallet_implicit_accounts(WalletId), State};

handle_call({mark_implicit_funded, ImplicitId, FundedAt}, _From, State) ->
    {reply, near_walletdb:mark_implicit_funded(ImplicitId, FundedAt), State};


handle_call({create_social_post, WalletId, PostData}, _From, State) ->
    {reply, near_walletdb:create_social_post(WalletId, PostData), State};

handle_call({get_social_post, PostId}, _From, State) ->
    {reply, near_walletdb:get_social_post(PostId), State};

handle_call({get_wallet_social_posts, WalletId}, _From, State) ->
    {reply, near_walletdb:get_wallet_social_posts(WalletId), State};

handle_call({get_user_social_posts, UserId}, _From, State) ->
    {reply, near_walletdb:get_user_social_posts(UserId), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'EXIT', Pid, Reason}, State = #state{worker_pool = Workers}) ->
    case lists:member(Pid, Workers) of
        true ->
            ?LOG_WARNING("Near wallet worker ~p crashed with reason: ~p. Replacing.", [Pid, Reason]),
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
