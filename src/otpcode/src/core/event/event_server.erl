-module(event_server).
-compile([export_all, nowarn_export_all]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).

%%TODO: Change the file's logic later
-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

add_event(Name, Date, Loc, Desc) ->
    gen_server:call({global, ?MODULE}, {add_event, Name, Date, Loc, Desc}).

retrieve_all() ->
    gen_server:call({global, ?MODULE}, {retrieve_all}).

retrieve_per_date(Date) ->
    gen_server:call({global, ?MODULE}, {retrieve_per_date, Date}).

todays_event() ->
    gen_server:call({global, ?MODULE}, {todays_event}).

delete_event(Date) ->
    gen_server:call({global, ?MODULE}, {delete_event, Date}).

init([]) ->
    ?LOG_NOTICE("Post server has been started - ~p", [self()]),
    {ok, #state{}}.

handle_call({add_event, Name, Date, Loc, Desc}, _From, State) ->
    eventdb:insert(Name, Date, Loc, Desc),
    io:format("New Event is added on"),
    {reply, ok, State};

handle_call({retrieve_all}, _From, State) ->
    All_data = eventdb:get_all(),
    {reply, All_data, State};

handle_call({retrieve_per_date, Date}, _From, State) ->
    All_data = eventdb:retrieve_per_date(Date),
    {reply, All_data, State};

handle_call({todays_event}, _From, State) ->
    eventdb:todays_event(),
    {reply, ok, State};

handle_call({delete_event, Date}, _From, State) ->
    eventdb:delete(Date),
    io:format("Event is deleted on ~p", [Date]),
    {reply, ok, State};

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