-module(msg_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0, get_user_by_id/1, delete_message/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
 code_change/3]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

get_message_by_id(Id) ->
  gen_server:call({global, ?MODULE}, {get_message_by_id, Id}).

get_user_by_id(Id) ->
  gen_server:call({global, ?MODULE}, {get_user_by_id, Id}).

delete_message(Id) ->
  gen_server:call({global, ?MODULE}, {delete_message, Id}).

init([]) ->
    ?LOG_NOTICE("Message server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({get_user_by_id, Id}, _From, State) ->
  Res = msgdb:get_user_by_id(Id),
  {reply, Res, State};

handle_call({delete_message, Id}, _From, State) ->
  msgdb:delete_message(Id),
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
