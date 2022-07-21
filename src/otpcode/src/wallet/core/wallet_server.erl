-module(wallet_server).
-export([start_link/0, create/2, get_wallet/1, get_wallets/0,
         deposit/2, withdraw/2,
         get_address/1, get_balance/1, send_token/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-include_lib("kernel/include/logger.hrl"). 
-include("../../records.hrl").

-behaviour(gen_server).
-record(state, {}).

start_link() ->
  ?LOG_NOTICE("Wallet server has been started - ~p", [self()]),
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

create(Name, Password) ->
  gen_server:call({global, ?MODULE}, {create, Name, Password}). 

get_wallet(Name) ->
  gen_server:call({global, ?MODULE}, {get_wallet, Name}).

get_wallets() ->
  gen_server:call({global, ?MODULE}, {get_wallets}).

get_address(Name) ->
  gen_server:call({global, ?MODULE}, {get_address, Name}).

deposit(Name, Amount) ->
  gen_server:call({global, ?MODULE}, {deposit, Name, Amount}).

withdraw(Name, Amount) ->
  gen_server:call({global, ?MODULE}, {withdraw, Name, Amount}).

get_balance(Name) ->
  Wallet = get_wallet(Name),
  Wallet#wallet.balance.

send_token(Name, To, From, Amount) ->
  gen_server:call({global, ?MODULE}, {send_token, Name, To, From, Amount}).

init([]) ->
    {ok, []}.

handle_call({create, Name, Password}, _From, State) ->
  Address = walletdb:insert(Name, Password),
  {reply, Address, State};

handle_call({get_wallet, Name}, _From, State) ->
  Wallets = walletdb:get_wallet(Name),
  {reply, Wallets, State};

handle_call({get_wallets}, _From, State = #state{}) ->
  Res = walletdb:get_wallets(),
  {reply, Res, State};

handle_call({get_address, Name}, _From, State = #state{}) ->
  Res = walletdb:get_address(Name),
  {reply, Res, State};

handle_call({deposit, Name, Amount}, _From, State) ->
    Res = walletdb:deposit(Name, Amount),
    {reply, Res, State};

handle_call({withdraw, Name, Amount}, _From, State) ->
  Res = walletdb:withdraw(Name, Amount),
  {reply, Res, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.
      
handle_cast(_Request, State) ->
    {noreply, State}.
      
handle_info(_Info, State) ->
    {noreply, State}.
      
terminate(_Reason, _State) ->
    ok.
      
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
