-module(ai_media_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

-export([start_link/0, insert/1, get_ai_media_by_ai_id/1, get_ai_media_by_media_id/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

insert(MediaID) ->
    gen_server:call({global, ?MODULE}, {insert, MediaID}).

get_ai_media_by_ai_id(ID) ->
    gen_server:call({global, ?MODULE}, {get_ai_media_by_ai_id, ID}).

get_ai_media_by_media_id(ID) ->
    gen_server:call({global, ?MODULE}, {get_ai_media_by_media_id, ID}).

init([]) ->
    ?LOG_NOTICE("AI Media server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({insert, MediaID}, _From, State) ->
    Res = ai_mediadb:insert(MediaID),
    {reply, Res, State};

handle_call({get_ai_media_by_ai_id, ID}, _From, State) ->
    Res = ai_mediadb:get_ai_media_by_ai_id(ID),
    {reply, Res, State};

handle_call({get_ai_media_by_media_id, ID}, _From, State) ->
    Res = ai_mediadb:get_ai_media_by_media_id(ID),
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