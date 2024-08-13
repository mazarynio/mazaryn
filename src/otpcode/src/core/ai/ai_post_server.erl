-module(ai_post_server).
-author("Zaryn Technologies").
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

-export([start_link/0, insert/1, get_ai_post_by_ai_id/1, get_ai_post_by_post_id/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

insert(PostID) ->
    gen_server:call({global, ?MODULE}, {insert, PostID}).

get_ai_post_by_ai_id(ID) ->
    gen_server:call({global, ?MODULE}, {get_ai_post_by_ai_id, ID}).

get_ai_post_by_post_id(ID) ->
    gen_server:call({global, ?MODULE}, {get_ai_post_by_post_id, ID}).

init([]) ->
    ?LOG_NOTICE("AI Post server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({insert, PostID}, _From, State) ->
    Res = ai_postdb:insert(PostID),
    {reply, Res, State};

handle_call({get_ai_post_by_ai_id, ID}, _From, State) ->
    Res = ai_postdb:get_ai_post_by_ai_id(ID),
    {reply, Res, State};

handle_call({get_ai_post_by_post_id, ID}, _From, State) ->
    Res = ai_postdb:get_ai_post_by_post_id(ID),
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