-module(token_server).

-include_lib("kernel/include/logger.hrl").

-define(TIMEOUT,   timer:seconds(600)).

-export([start_link/0, validate/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

validate(TokenID) ->
    gen_server:call({global, ?MODULE}, {validate, TokenID}, ?TIMEOUT).

init([]) ->
    ?LOG_NOTICE("Token server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({validate, TokenID}, _From, State) ->
    Token = userdb:get_token_by_id(TokenID),
    {reply, Token, State};

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
