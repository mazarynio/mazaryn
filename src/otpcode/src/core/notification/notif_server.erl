-module(notif_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, notify/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

notify(From, To, Message) ->
    gen_server:call({global, ?MODULE}, {notify, From, To, Message}).

init([]) ->
    ?LOG_NOTICE("Notif server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_Call({notify, From, To, Message}, _From, State) ->
    Res = notifdb:insert(From, To, Message),
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
