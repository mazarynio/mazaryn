-module(ae_wallet_server).
-export([start_link/0, create/4, get_wallet/1, get_wallets/0, deposit/2,
  withdraw/2, delete_wallet/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-include_lib("kernel/include/logger.hrl"). 
-include("../../records.hrl").

-behaviour(gen_server).
-record(state, {}).

start_link() ->
  ?LOG_NOTICE("Aeternity Wallet server has been started - ~p", [self()]),
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

create(Name, Password, Size, Token) ->
  gen_server:call({global, ?MODULE}, {create, Name, Password, Size, Token}).

get_wallet(Name) ->
  gen_server:call({global, ?MODULE}, {get_wallet, Name}).

get_wallets() ->
  gen_server:call({global, ?MODULE}, {get_wallets}).

deposit(Name, Amount) ->
  gen_server:call({global, ?MODULE}, {deposit, Name, Amount}).

withdraw(Name, Amount) ->
  gen_server:call({global, ?MODULE}, {withdraw, Name, Amount}).

delete_wallet(Name) ->
  gen_server:call({global, ?MODULE}, {delete_wallet, Name}).

init([]) ->
  {ok, []}.

handle_call({create, Name, Password, Size, Token}, _From, State) ->
  Address = ae_walletdb:insert(Name, Password, Size, Token),
  {reply, Address, State};

handle_call({get_wallet, Name}, _From, State) ->
  Res = ae_walletdb:get_wallet(Name),
  {reply, Res, State};

handle_call({get_wallets}, _From, State = #state{}) ->
  Res = ae_walletdb:get_wallets(),
  {reply, Res, State};

handle_call({deposit, Name, Amount}, _From, State) ->
  Res = ae_walletdb:deposit(Name, Amount),
  {reply, Res, State};

handle_call({withdraw, Name, Amount}, _From, State) ->
  Res = ae_wallet:withdraw(Name, Amount),
  {reply, Res, State};

handle_call({delete_wallet, Name}, _From, State) ->
  Res = ae_walletdb:delete_wallet(Name),
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