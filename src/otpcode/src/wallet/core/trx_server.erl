-module(trx_server).
-export([start_link/0, get_transaction/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).


-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

get_transaction() ->
    gen_server:call({global, ?MODULE}, {get_transaction}).

init([]) ->
    {ok, #state{}}.

handle_call({get_transaction}, _From, State) ->
    Res = trxdb:get_transaction(),
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