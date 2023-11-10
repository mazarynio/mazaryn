-module(media_server).
-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE). 
-record(state, {}).

%%API
-export([start_link/0, insert_media/2, delete_file/1, get_media/1, get_all_media/1,
 report_media/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

insert_media(UserID, File) ->
    gen_server:call({global, ?MODULE}, {insert_media, UserID, File}).

delete_file(MediaID) ->
    gen_server:call({global, ?MODULE}, {delete_file, MediaID}).

get_media(MediaID) ->
    gen_server:call({global, ?MODULE}, {get_media, MediaID}).

get_all_media(UserID) ->
    gen_server:call({global, ?MODULE}, {get_all_media, UserID}).

report_media(MyID, MediaID, Type, Description) ->
    gen_server:call({global, ?MODULE}, {report_media, MyID, MediaID, Type, Description}).

init([]) ->
    ?LOG_NOTICE("Media server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({insert_media, UserID, File}, _From, State) ->
    Res = mediadb:insert_media(UserID, File),
    {reply, Res, State};

handle_call({delete_file, MediaID}, _From, State) ->
    Res = mediadb:delete_file(MediaID),
    {reply, Res, State};

handle_call({get_media, MediaID}, _From, State) ->
    Res = mediadb:get_media(MediaID),
    {reply, Res, State};

handle_call({get_all_media, UserID}, _From, State) ->
    Res = mediadb:get_all_media(UserID),
    {reply, Res, State};

handle_call({report_media, MyID, MediaID, Type, Description}, _From, State) ->
    Res = mediadb:report_media(MyID, MediaID, Type, Description),
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