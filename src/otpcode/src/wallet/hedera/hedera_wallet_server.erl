-module(hedera_wallet_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, create_account/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

create_account(Password) ->
    gen_server:call({global, ?MODULE}, {create_account, Password}).

init([]) ->
    ?LOG_NOTICE("User server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({create_account, Password}, _From, State = #state{}) ->
    Id = hedera_walletdb:insert(Password),
    {reply, Id, State};

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
