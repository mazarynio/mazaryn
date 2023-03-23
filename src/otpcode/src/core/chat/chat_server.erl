-module(chat_server).
-author("mazaryn").

-define(NUM, 5).

-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, create_chat/2, get_chat_by_id/1]).

%% CALLBACKS
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    ?LOG_NOTICE("Chat server has been started - ~p", [self()]),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

create_chat(Peer_Ids, Title) ->
    gen_server:call({global, ?MODULE}, {insert, Peer_Ids, Title}).

get_chat_by_id(Id) ->
    gen_server:call({global, ?MODULE}, {get_by_id, Id}).

%% PRIVATE FUNCS
init([]) ->
    {ok, []}.

handle_call({insert, Peer_Ids, _Title}, _From, State) ->
    Id = chatdb:create_chat(Peer_Ids),
    Chat = chatdb:get_chat_by_id(Id),
    {reply, Chat, State};

handle_call({get_by_id, Id}, _From, State) ->
    Chat = chatdb:get_chat_by_id(Id),
    {reply, Chat, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.