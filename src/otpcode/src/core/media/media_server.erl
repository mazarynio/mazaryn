-module(media_server).
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

%%API
-export([start_link/0, insert_music/2, insert_video/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

insert_music(Username, Single) ->
    gen_server:call({global, ?MODULE}, {insert_music, Username, Single}).

insert_video(Username, Single) ->
    gen_server:call({global, ?MODULE}, {insert_video, Username, Single}).

init([]) ->
    ?LOG_NOTICE("Media server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({insert_music, Username, Single}, _From, State) ->
    Res = mediadb:insert_music(Username, Single),
    {reply, Res, State};

handle_call({insert_video, Username, Single}, _From, State) ->
    Res = mediadb:insert_video(Username, Single),
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