-module(chat_server).
-author("Zaryn Technologies").

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

%%API
-export([start_link/0, send_msg/3, get_msg/1, get_all_msg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

send_msg(UserID, RecipientID, Body) ->
    gen_server:call({global, ?MODULE}, {send_msg, UserID, RecipientID, Body}).

get_msg(ChatID) ->
    gen_server:call({global, ?MODULE}, {get_msg, ChatID}).

get_all_msg(RecipientID) ->
    gen_server:call({global, ?MODULE}, {get_all_msg, RecipientID}).

init([]) ->
    ?LOG_NOTICE("Chat server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({send_msg, UserID, RecipientID, Body}, _From, State = #state{}) ->
    Res = chatdb:send_msg(UserID, RecipientID, Body),
    {reply, Res, State};

handle_call({get_msg, ChatID}, _From, State) ->
    Res = chatdb:get_msg(ChatID),
    {reply, Res, State};

handle_call({get_all_msg, RecipientID}, _From, State) ->
    Res = chatdb:get_all_msg(RecipientID),
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