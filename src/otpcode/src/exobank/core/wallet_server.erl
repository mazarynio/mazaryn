-module(wallet_server).
-export([start_link/0, deposit/1, withdraw/1, get_balance/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []). 

deposit(Amount) ->
  gen_server:call({global, ?MODULE}, {deposit, Amount}).

withdraw(Amount) ->
  gen_server:call({global, ?MODULE}, {withdraw, Amount}).

get_balance() ->
  gen_server:call({global, ?MODULE}, {get_balance}).

init([]) ->
    ?LOG_NOTICE("Wallet server has been started - ~p", [self()]),
    {ok, #state{}}.

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
